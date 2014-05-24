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

import Data.Typeable
import Data.Hashable
import System.Mem.MemoTable

-- | Ref of @a@
data M (m :: * -> *) (r :: * -> *) a = M {
		idM :: Id
	,	valueM :: r a
	,	dependentsM :: r (IntMap (Set (SomeMU m r)))
	} deriving Typeable

instance EqRef r => Eq (M m r a) where
	m1 == m2 = valueM m1 `eqRef` valueM m2 -- we could compare ids instead, but this should be safer

-- | Thunk of @a@
data U l (m :: * -> *) (r :: * -> *) a = U {
		idU :: Id
	,	valueU :: r a
	,	forceU :: r (l m r a)
	,	dependentsU :: r (IntMap (Set (SomeMU m r)))
	,	dependenciesU :: r (IntMap (Set (SomeMU m r))) -- shouldn't this be a list as in the paper?
	,	dirtyU :: r Bool
	,	memoU :: r (MemoTree m r)
	,	traceU :: [(Computation m r,SomeValue)]
	} deriving Typeable

instance EqRef r => Eq (U l m r a) where
	u1 == u2 = valueU u1 `eqRef` valueU u2 -- we could compare ids instead, but this should be safer

type CallStack m r = [SomeU m r] -- not sure

data MemoTree m r = Leaf SomeValue
				  | Node (Computation m r) (IntMap (MemoTree m r)) (IntMap (MemoTree m r))
				  | Empty

data Computation m r = Get (SomeM m r) | Force (SomeU m r)

data SomeValue where
	SomeValue :: (Eq a,Typeable a) => a -> SomeValue

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

class (Monad (l m r),Ref m r) => Layer l m r where
	get :: M m r a -> l m r a
	inner :: Inner m r a -> l m r a
	force :: (U l m r a) -> l m r a
	thunk :: l m r a -> l m r (U l m r a)

instance EqRef r => Memo (M m r a) where
	type MemoKey (M m r a) = Id
	memoKey = return . refId . valueM

instance EqRef r => Memo (U l m r a) where
	type MemoKey (U l m r a) = Id
	memoKey = return . refId . valueU

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

run :: Monad m => Outer m r a -> m a
run (Outer comp) = do
	(x,_,_) <- comp 0 []
	return x

instance Ref m r => Layer Inner m r where
	get = undefined
	inner = id
	force = forceInner

instance Ref m r => Layer Outer m r where
	get = undefined
	inner (Inner c) = Outer c
	force = forceOuter

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
dirty :: Ref m r => r (IntMap (Set (SomeMU m r))) -> m (r (IntMap (Set (SomeMU m r))))
dirty = mapRefM $ mapM $ mapSetM $ either (return . Left) (liftM Right . dirty')
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
	dependents <- newRef $ IntMap.empty
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
