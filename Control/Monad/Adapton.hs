{-# LANGUAGE TypeFamilies, FlexibleContexts, UndecidableInstances, ScopedTypeVariables, DeriveDataTypeable, KindSignatures, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, GADTs #-}

module Control.Monad.Adapton where

import Prelude hiding (mapM)
import Data.IORef
import Data.IntMap (IntMap(..))
import qualified Data.IntMap as IntMap
import Data.Set (Set(..))
import qualified Data.Set as Set
import Control.Monad.State (State(..),StateT(..),MonadState(..))
import qualified Control.Monad.State as State
import Control.Monad.Trans
import Data.Traversable
import Control.Monad hiding (mapM)
import Data.List as List
import Data.Map (Map(..))
import qualified Data.Map as Map

import Data.Typeable
import Data.Hashable
import System.Mem.MemoTable

-- | Ref of @a@
data M (m :: * -> *) (r :: * -> *) a = M {
		idM :: Id
	,	valueM :: r a
	,	dependentsM :: r (IntMap (SomeMU m r))
	} deriving Typeable

instance EqRef r => Eq (M m r a) where
	m1 == m2 = valueM m1 `eqRef` valueM m2 -- we could compare ids instead, but this should be safer

-- | Thunk of @a@
data U l (m :: * -> *) (r :: * -> *) a = U {
		idU :: Id
	,	valueU :: r a
	,	forceU :: r (l m r a)
	,	dependentsU :: r (IntMap (SomeMU m r))
	,	dependenciesU :: r [(Int,SomeMU m r)] -- shouldn't this be a list as in the paper?
	,	dirtyU :: r Bool
	,	memoU :: r (MemoTree m r)
	,	traceU :: r (Trace m r)
	} deriving Typeable

type Trace m r = [(Computation m r,SomeValue)]

instance EqRef r => Eq (U l m r a) where
	u1 == u2 = valueU u1 `eqRef` valueU u2 -- we could compare ids instead, but this should be safer

type CallStack m r = [SomeU m r] -- not sure

data MemoTree m r = Leaf SomeValue
				  | Node (Computation m r) (Map MemoKey (MemoTree m r))
				  | Empty

data Computation m r = Get (SomeM m r) | Force (SomeU m r)

treeify :: HasIO m => Trace m r -> SomeValue -> m (MemoTree m r)
treeify [] result = return $ Leaf result
treeify ((comp,SomeValue value):trace') result = do
	id <- doIO $ memoKey value
	tree' <- treeify trace' result
	return $ Node comp $ Map.singleton id tree'

insert_trace :: HasIO m => Trace m r -> MemoTree m r -> SomeValue -> m (MemoTree m r)
insert_trace [] tree result = return $ Leaf result
insert_trace trace (Leaf _) result = error "inconsistent computation"
insert_trace ((comp,SomeValue value):trace') (Node comp' tbl) result = do
	id <- doIO $ memoKey value
	case Map.lookup id tbl of
		Just mem -> do
			tree <- insert_trace trace' mem result
			return $ Node comp' $ Map.insert id tree tbl
		Nothing -> do
			tree <- treeify trace' result
			return $ Node comp' $ Map.insert id tree tbl
insert_trace trace Empty result = treeify trace result

data SomeValue where
	SomeValue :: (Eq a,Typeable a,Memo a) => a -> SomeValue

instance Eq SomeValue where
	(SomeValue (x :: a)) == (SomeValue (y :: b)) = case cast x :: Maybe b of
		Just y' -> y == y'
		Nothing -> False

data SomeM m r where
	SomeM :: (Eq a,Typeable m,Typeable r,Typeable a) => M m r a -> SomeM m r

instance Ref m r => Eq (SomeM m r) where
	(SomeM (x :: M m r a)) == (SomeM (y :: M m' r' b)) = case cast x :: Maybe (M m r b) of
		Just y' -> y == y'
		Nothing -> False

data SomeU m r where
	SomeU :: (Layer l m r,Typeable l,Eq a,Typeable m,Typeable r,Typeable a) => U l m r a -> SomeU m r

instance Ref m r => Eq (SomeU m r) where
	(SomeU (x :: U l m r a)) == (SomeU (y :: U l' m' r' b)) = case cast x :: Maybe (U l' m' r' b) of
		Just y' -> y == y'
		Nothing -> False

type SomeMU m r = Either (SomeM m r) (SomeU m r)

class (Typeable m,Typeable r,InL l,Monad (l m r),Ref m r,MonadState (Id, CallStack m r) (l m r)) => Layer l m r where
	inner :: Inner m r a -> l m r a
	force :: U l m r a -> l m r a
	thunk :: l m r a -> l m r (U l m r a)
	inOuter :: l m r a -> Outer m r a

get :: (Memo a,Eq a,Typeable a,Layer l m r) => M m r a -> l m r a
get m = do
	stack <- getCallStack
	case stack of
		callcell@(SomeU caller) : stack' -> do			
			inL $ mapRefM (return . (++ [(idM m,Left $ SomeM m)])) (dependenciesU caller)
			inL $ mapRefM (return . IntMap.insert (idU caller) (Right callcell)) (dependentsU caller)
			value <- inL $ readRef $ valueM m
			inL $ mapRefM (return . ((Get (SomeM m),SomeValue value) :)) (traceU caller) -- shouldn't it be inserted as the last?
			return value
		otherwise -> inL $ readRef $ valueM m

instance EqRef r => Memo (M m r a) where
	memoKey = return . MemoKey . refId . valueM

instance EqRef r => Memo (U l m r a) where
	memoKey = return . MemoKey . refId . valueU

memo :: (Layer l m r,Memo arg) => ((arg -> l m r (U l m r a)) -> arg -> l m r a) -> (arg -> l m r (U l m r a))
memo f = do
	let memo_func = memoLazy (thunk . f memo_func)
	memo_func

-- with change propagation
forceInner :: (U Inner m r a) -> Inner m r a
forceInner = undefined

-- without change propagation
forceOuter :: (U Outer m r a) -> Outer m r a
forceOuter = undefined

data Inner (m :: * -> *) (r :: * -> *) a = Inner { innerComp :: Id -> CallStack m r -> m (a,Id,CallStack m r) } deriving Typeable
data Outer (m :: * -> *) (r :: * -> *) a = Outer { outerComp :: Id -> CallStack m r -> m (a,Id,CallStack m r) } deriving Typeable

instance Ref m r => MonadState (Id,CallStack m r) (Inner m r) where
	get = Inner $ \id stack -> return ((id,stack),id,stack)
	put (id',stack') = Inner $ \id stack -> return ((),id',stack')

instance Ref m r => MonadState (Id,CallStack m r) (Outer m r) where
	get = Outer $ \id stack -> return ((id,stack),id,stack)
	put (id',stack') = Outer $ \id stack -> return ((),id',stack')

getCallStack :: MonadState (Id,CallStack m r) n => n (CallStack m r)
getCallStack = liftM snd State.get

putCallStack :: MonadState (Id,CallStack m r) n => CallStack m r -> n ()
putCallStack stack' = State.modify (\(id,stack) -> (id,stack'))

run :: Monad m => Outer m r a -> m a
run (Outer comp) = do
	(x,_,_) <- comp 0 []
	return x

instance (Typeable m,Typeable r,Ref m r) => Layer Inner m r where
	inner = id
	force = forceInner
	inOuter (Inner f) = Outer f

instance (Typeable m,Typeable r,Ref m r) => Layer Outer m r where
	inner (Inner c) = Outer c
	force = forceOuter
	inOuter = id

instance Ref m r => Monad (Inner m r) where
	return x = Inner $ \counter stack -> return (x,counter,stack)
	(Inner m) >>= f = Inner $ \counter stack -> do
		(x,counter',stack') <- m counter stack
		innerComp (f x) counter stack'

instance Ref m r => Monad (Outer m r) where
	return x = Outer $ \counter stack -> return (x,counter,stack)
	(Outer m) >>= f = Outer $ \counter stack -> do
		(x,counter',stack') <- m counter stack
		outerComp (f x) counter stack'

set :: (Ref m r,Eq a) => M m r a -> a -> Outer m r ()
set (M idM valueM dependentsM) v' = do
	v <- readRef valueM
	if v == v'
		then return ()
		else do
			writeRef valueM v'
			inL $ dirty dependentsM
			return ()

-- internal, used at set
dirty :: Ref m r => r (IntMap (SomeMU m r)) -> m (r (IntMap (SomeMU m r)))
dirty = mapRefM $ mapM $ either (return . Left) (liftM Right . dirty')
	where
	dirty' :: Ref m r => SomeU m r -> m (SomeU m r)
	dirty' (SomeU u) = do
		b <- readRef (dirtyU u)
		if b
			then return $ SomeU u
			else do
				writeRef (dirtyU u) True
				dirty (dependentsU u)
				return $ SomeU u

ref :: (NewRef (Outer m r) r) => a -> Outer m r (M m r a)
ref v = do
	valueM <- newRef v
	dependents <- newNonUniqueRef $ IntMap.empty -- we only needs ids for the ref/thunk value references
	return $ M (refId valueM) valueM dependents

class EqRef r where
	eqRef :: r a -> r a -> Bool
	refId :: r a -> Id

-- | References @r@ under monad @m@
class (EqRef r, Monad m) => Ref m r where
	newNonUniqueRef :: a -> m (r a) -- you should avoid using this function, as it does not handle unique ids. new refs should always be created under Inner or Outer instead
	readRef  :: r a -> m a
	writeRef :: r a -> a -> m ()

class (Ref m r) => NewRef m r where
	newRef   :: a -> m (r a)

data IdRef a = IdRef Id (IORef a)

instance EqRef IdRef where
	eqRef (IdRef i1 r1) (IdRef i2 r2) = eqRef r1 r2
	refId (IdRef i r) = i

instance Ref IO IdRef where
	newNonUniqueRef v = newIORef v >>= return . IdRef (-1)
	readRef (IdRef i r) = readIORef r
	writeRef (IdRef i r) v = writeIORef r v

instance Ref m r => Ref (Inner m r) r where
	newNonUniqueRef v = inL $ newNonUniqueRef v
	readRef r = inL $ readRef r
	writeRef r v = inL $ writeRef r v
	
instance Ref m r => Ref (Outer m r) r where
	newNonUniqueRef v = inL $ newNonUniqueRef v
	readRef r = inL $ readRef r
	writeRef r v = inL $ writeRef r v
         
instance (Ref m IdRef,Ref m IORef) => NewRef (Inner m IdRef) IdRef where -- the only instances, because we want to increment counters
	newRef v = Inner $ \counter stack -> newNonUniqueRef v >>= \r -> return (IdRef counter r,counter+1,stack)
instance (Ref m IdRef,Ref m IORef) => NewRef (Outer m IdRef) IdRef where -- the only instances, because we want to increment counters
	newRef v = Outer $ \counter stack -> newNonUniqueRef v >>= \r -> return (IdRef counter r,counter+1,stack)

instance EqRef IORef where
	eqRef = (==)
	refId r = error "no ids for IORef"

instance Ref IO IORef where
	newNonUniqueRef = newIORef
	readRef = readIORef
	writeRef = writeIORef

mapRefM :: Ref m r => (a -> m a) -> r a -> m (r a)
mapRefM f r = readRef r >>= f >>= writeRef r >> return r

type Id = Int

class InL (l :: (* -> *) -> (* -> *) -> * -> *) where
   inL :: Monad m => m a -> l m r a

instance InL Inner where
	inL m = Inner $ \counter stack -> m >>= \x -> return (x,counter,stack)

instance InL Outer where
	inL m = Outer $ \counter stack -> m >>= \x -> return (x,counter,stack)

mapSetM :: (Monad m,Eq b) => (a -> m b) -> Set a -> m (Set b)
mapSetM f s = liftM Set.fromAscList $ mapM f $ Set.toAscList s

class Monad m => HasIO m where
	doIO :: IO a -> m a

instance HasIO IO where
	doIO = id
