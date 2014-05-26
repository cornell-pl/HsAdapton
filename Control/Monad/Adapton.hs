{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, UndecidableInstances, ScopedTypeVariables, DeriveDataTypeable, KindSignatures, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, GADTs #-}

module Control.Monad.Adapton where

import Prelude hiding (mapM)
import Data.IORef
import Data.IntMap (IntMap(..))
import qualified Data.IntMap as IntMap
import Data.Set (Set(..))
import qualified Data.Set as Set
import Control.Monad.State (State(..),StateT(..),MonadState)
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
import GHC.Generics

import Debug.Trace

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
	,	dependentsU :: r (Dependents m r)
	,	dependenciesU :: r (Dependencies m r)
	,	dirtyU :: r Bool
	,	memoU :: r (MemoTree m r)
	,	traceU :: r (Trace m r)
	} deriving Typeable

type Dependents m r = IntMap (SomeMU m r)
type Dependencies m r = [(Int,SomeMU m r)] -- shouldn't this be a list as in the paper? order matters
type Trace m r = [(Computation m r,SomeValue)]

instance EqRef r => Eq (U l m r a) where
	u1 == u2 = valueU u1 `eqRef` valueU u2 -- we could compare ids instead, but this should be safer

type CallStack m r = [SomeU m r] -- not sure

data MemoTree m r = Leaf SomeValue
				  | Node (Computation m r) (Map MemoKey (MemoTree m r))
				  | Empty

data Computation m r = Get (SomeM m r) | Force (SomeU m r)

search_memo :: Layer Inner m r => SomeU m r -> Inner m r (Maybe SomeValue)
search_memo (SomeU t) = readRef (memoU t) >>= search_memo_helper where
	search_memo_helper (Leaf value) = return $ Just value
	search_memo_helper Empty = return Nothing
	search_memo_helper (Node comp tbl) = do
		k <- case comp of
			Get (SomeM m) -> get m >>= doIO . memoKey
			Force (SomeU t) -> case castInnerThunk t of
				Just t' -> force t' >>= doIO . memoKey
				Nothing -> error "force should have the same type"
		case Map.lookup k tbl of
			Just tree -> search_memo_helper tree
			Nothing -> return Nothing

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
	SomeM :: (Memo a,Eq a,Typeable m,Typeable r,Typeable a) => M m r a -> SomeM m r

instance Ref m r => Eq (SomeM m r) where
	(SomeM (x :: M m r a)) == (SomeM (y :: M m' r' b)) = case cast x :: Maybe (M m r b) of
		Just y' -> y == y'
		Nothing -> False

data SomeU m r where
	SomeU :: (Memo a,Layer l m r,Typeable l,Eq a,Typeable m,Typeable r,Typeable a) => U l m r a -> SomeU m r

instance Ref m r => Eq (SomeU m r) where
	(SomeU (x :: U l m r a)) == (SomeU (y :: U l' m' r' b)) = case cast x :: Maybe (U l' m' r' b) of
		Just y' -> y == y'
		Nothing -> False

type SomeMU m r = Either (SomeM m r) (SomeU m r)

class (HasIO (l m r),HasIO m,NewRef (l m r) r,Ref (l m r) r,Typeable l,Typeable m,Typeable r,InL l,Monad (l m r),Ref m r,MonadState (Id, CallStack m r) (l m r)) => Layer l m r where
	inner :: Inner m r a -> l m r a
	force :: (Memo a,Eq a,Typeable a) => U l m r a -> l m r a
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

memo :: (Eq a,Typeable a,Memo a,Layer l m r,Memo arg) => ((arg -> l m r (U l m r a)) -> arg -> l m r a) -> (arg -> l m r (U l m r a))
memo f = do
	let memo_func = memoLazy (thunk . f memo_func)
	memo_func

-- with change propagation
forceInner :: (Memo a,Eq a,Typeable a,Layer Inner m r) => U Inner m r a -> Inner m r a
forceInner t = do
	dirty <- readRef $ dirtyU t
	result <- if (not dirty)
		then readRef $ valueU t
		else do
			oldstack <- getCallStack
			mbv <- search_memo (SomeU t)
			case mbv of
				Just value -> castAsThunkValue t value
				Nothing -> do
					inL $ clearDependenciesDependents (idU t) (dependenciesU t)
					putCallStack (SomeU t : oldstack)
					value <- readRef (forceU t) >>= \c -> c
					writeRef (valueU t) value
					writeRef (dirtyU t) False
					putCallStack oldstack
					return value
	callstack <- getCallStack
	case callstack of
		(SomeU caller:_) -> do
			mapRefM (\tbl -> return $ tbl ++ [(idU t,Right $ SomeU t)]) (dependenciesU caller) -- insert at the end?
			mapRefM (return . IntMap.insert (idU caller) (Right $ SomeU caller)) (dependentsU t)
			mapRefM (\trace -> return $ (Force (SomeU t),SomeValue result) : trace) (traceU caller)
			return ()
		otherwise -> return ()
	return result

clearDependenciesDependents :: Ref m r => Id -> r (Dependencies m r) -> m (r (Dependencies m r))
clearDependenciesDependents id r = mapRefM (\deps -> mapM_ clearDependency deps >> return []) r where
	clearDependency (id',Left (SomeM m')) = mapRefM (return . IntMap.delete id) (dependentsM m')
	clearDependency (id',Right (SomeU t')) = mapRefM (return . IntMap.delete id) (dependentsU t')

castAsThunkValue :: (Typeable a,Layer l m r) => U l m r a -> SomeValue -> l m r a
castAsThunkValue (t :: U l m r a) v = castAsValue (undefined :: a) v

castAsValue :: (Typeable a,Layer l m r) => a -> SomeValue -> l m r a
castAsValue (_ :: a) (SomeValue (b :: b)) = case cast b :: Maybe a of
	Just b' -> return b'
	Nothing -> error "types do not match"

castInnerThunk :: (Typeable l,Typeable m,Typeable r,Typeable a) => U l m r a -> Maybe (U Inner m r a)
castInnerThunk (t :: U l m r a) = cast t :: Maybe (U Inner m r a)

-- without change propagation
forceOuter :: (Layer Outer m r,Eq a,Typeable a,Memo a) => (U Outer m r a) -> Outer m r a
forceOuter t = trace "forced at the outer layer" $ do
	dirty <- readRef $ dirtyU t
	result <- if (not dirty)
		then readRef $ valueU t
		else do
			oldstack <- getCallStack
			inL $ clearDependenciesDependents (idU t) (dependenciesU t)
			putCallStack (SomeU t : oldstack)
			value <- readRef (forceU t) >>= \c -> c
			writeRef (valueU t) value
			writeRef (dirtyU t) False
			putCallStack oldstack
			return value
	callstack <- getCallStack
	case callstack of
		(SomeU caller:_) -> do
			mapRefM (\tbl -> return $ tbl ++ [(idU t,Right $ SomeU t)]) (dependenciesU caller) -- insert at the end?
			mapRefM (return . IntMap.insert (idU caller) (Right $ SomeU caller)) (dependentsU t)
			mapRefM (\trace -> return $ (Force (SomeU t),SomeValue result) : trace) (traceU caller)
			return ()
		otherwise -> return ()
	return result

thunk :: (Eq a,Typeable a,Memo a,Layer l m r) => l m r a -> l m r (U l m r a)
thunk c = do
	valueU <- newRef $ error "no old value in thunk"
	let idU = refId valueU
	forceU <- newNonUniqueRef $ return $ error "no old value in thunk"
	dependentsU <- newNonUniqueRef IntMap.empty
	dependenciesU <- newNonUniqueRef []
	dirtyU <- newNonUniqueRef True
	memoU <- newNonUniqueRef Empty
	traceU <- newNonUniqueRef []
	let t = U idU valueU forceU dependentsU dependenciesU dirtyU memoU traceU
	let final_comp = do
		inL $ mapRefM (\x -> return []) traceU
		result <- c
		inL $ mapRefM (\memo -> readRef traceU >>= \trace -> insert_trace (reverse trace) memo (SomeValue result)) memoU
		return result
	inL $ writeRef forceU final_comp
	return t

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

instance (HasIO m,NewRef (Inner m r) r,Typeable m,Typeable r,Ref m r) => Layer Inner m r where
	inner = id
	force = forceInner
	inOuter (Inner f) = Outer f

instance (HasIO m,NewRef (Outer m r) r,Typeable m,Typeable r,Ref m r) => Layer Outer m r where
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
dirty :: Ref m r => r (Dependents m r) -> m (r (Dependents m r))
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

data IdRef a = IdRef Id (IORef a) deriving Typeable

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

instance (Ref m r,HasIO m) => HasIO (Inner m r) where
	doIO m = inL (doIO m)

instance (Ref m r,HasIO m) => HasIO (Outer m r) where
	doIO m = inL (doIO m)


-- * generic mechanism for displaying data types with refs/thunks

class (Layer l m r) => Display l m r a where
	display :: a -> l m r ()
	display x = showL x >>= doIO . print
	showL :: a -> l m r String

instance (Display l m r a,Eq a,Typeable a,Memo a) => Display l m r (U l m r a) where
	showL t = force t >>= showL

instance (Display l m r a,Eq a,Typeable a,Memo a) => Display l m r (M m r a) where
	showL m = get m >>= showL

instance Layer l m r => Display l m r (U1 p) where
	showL U1 = return ""
instance (Display l m r c,Layer l m r) => Display l m r (K1 i c p) where
	showL (K1 c) = showL c
instance (Display l m r (f p),Constructor c) => Display l m r (M1 C c f p) where
	showL m1 = do
		str <- showL (unM1 m1)
		return $ "(" ++ conName m1 ++ " " ++ str ++ ")"
instance (Display l m r (f p)) => Display l m r (M1 D c f p) where
	showL m1 = showL (unM1 m1)
instance (Display l m r (f p)) => Display l m r (M1 S c f p) where
	showL m1 = showL (unM1 m1)
instance (Display l m r (f p),Display l m r (g p)) => Display l m r ((f :+: g) p) where
	showL (L1 x) = showL x
	showL (R1 x) = showL x
instance (Display l m r (f p),Display l m r (g p)) => Display l m r ((f :*: g) p) where
	showL (x :*: y) = do
		str1 <- showL x
		str2 <- showL y
		return $ str1 ++ " " ++ str2
instance Layer l m r => Display l m r Int where
	showL i = return $ show i

