{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, KindSignatures, GADTs, DeriveDataTypeable #-}

module Control.Monad.Adapton.DataTypes where

import System.Mem.MemoTable
import Data.Typeable
import Data.IntMap (IntMap(..))
import qualified Data.IntMap as IntMap
import Data.Map (Map(..))
import qualified Data.Map as Map
import Control.Monad.Ref
import Control.Monad.State as State

-- | Ref of @a@. Modifiable references are just special thunks.
newtype M m r a = M { unM :: U m r a } deriving Typeable

-- | Thunk of @a@
data U (m :: * -> *) (r :: * -> *) a = U {
		idU :: Id
	,	valueU :: IdRef r a
	,	forceU :: r (Inner m r a)
	,	dependentsU :: r (Dependents m r)
	,	dependenciesU :: r (Dependencies m r)
	,	dirtyU :: r Bool
	,	memoU :: r (MemoTree m r)
	,	traceU :: r (Trace m r)
	} deriving Typeable

type Dependents m r = IntMap (SomeU m r)
type Dependencies m r = [(Int,SomeU m r)] -- shouldn't this be a list as in the paper? order matters
type Trace m r = [(SomeU m r,SomeValue)]

type CallStack m r = Maybe (SomeU m r) -- not sure

data MemoTree m r = Leaf SomeValue
				  | Node (SomeU m r) (Map MemoKey (MemoTree m r))
				  | Empty

data SomeValue where
	SomeValue :: (Typeable a,Memo a) => a -> SomeValue
	
data SomeU m r where
	SomeU :: (Memo a,Typeable m,Typeable r,Typeable a) => U m r a -> SomeU m r
	
data Inner (m :: * -> *) (r :: * -> *) a = Inner { innerComp :: Id -> CallStack m r -> m (a,Id,CallStack m r) } deriving Typeable
data Outer (m :: * -> *) (r :: * -> *) a = Outer { outerComp :: Id -> CallStack m r -> m (a,Id,CallStack m r) } deriving Typeable

instance EqRef r => Eq (M m r a) where
	(M m1) == (M m2) = valueU m1 `eqRef` valueU m2 -- we could compare ids instead, but this should be safer

instance EqRef r => Eq (U m r a) where
	u1 == u2 = valueU u1 `eqRef` valueU u2 -- we could compare ids instead, but this should be safer

instance Ref m r => Monad (Inner m r) where
	return x = Inner $ \counter stack -> return (x,counter,stack)
	(Inner m) >>= f = Inner $ \counter stack -> do
		(x,counter',stack') <- m counter stack
		innerComp (f x) counter' stack'

instance Ref m r => Monad (Outer m r) where
	return x = Outer $ \counter stack -> return (x,counter,stack)
	(Outer m) >>= f = Outer $ \counter stack -> do
		(x,counter',stack') <- m counter stack
		outerComp (f x) counter' stack'

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

class InL (l :: (* -> *) -> (* -> *) -> * -> *) where
   inL :: Monad m => m a -> l m r a

instance InL Inner where
	inL m = Inner $ \counter stack -> m >>= \x -> return (x,counter,stack)

instance InL Outer where
	inL m = Outer $ \counter stack -> m >>= \x -> return (x,counter,stack)

instance (Ref m r,MonadIO m) => MonadIO (Inner m r) where
	liftIO m = inL (liftIO m)

instance (Ref m r,MonadIO m) => MonadIO (Outer m r) where
	liftIO m = inL (liftIO m)
	
instance Ref m r => Ref (Inner m r) r where
	newRef v = inL $ newRef v
	readRef r = inL $ readRef r
	writeRef r v = inL $ writeRef r v

instance Ref m r => Ref (Outer m r) r where
	newRef v = inL $ newRef v
	readRef r = inL $ readRef r
	writeRef r v = inL $ writeRef r v

instance Ref m r => Ref (Inner m r) (IdRef r) where
	newRef v = error "newIdRef Inner"
	readRef (IdRef _ r) = inL $ readRef r
	writeRef (IdRef _ r) v = inL $ writeRef r v
	
instance Ref m r => Ref (Outer m r) (IdRef r) where
	newRef v = error "newIdRef Outer"
	readRef (IdRef _ r) = inL $ readRef r
	writeRef (IdRef _ r) v = inL $ writeRef r v

instance Ref m r => UniqueRef (Inner m r) (IdRef r) where
	newUniqueRef v = do
		r <- inL $ newRef v
		Inner $ \count stack -> return (IdRef count r,count+1,stack)

instance Ref m r => UniqueRef (Outer m r) (IdRef r) where
	newUniqueRef v = do
		r <- inL $ newRef v
		Outer $ \count stack -> return (IdRef count r,count+1,stack)

intMapM_ :: Monad m => (a -> m b) -> IntMap a -> m ()
intMapM_ f = IntMap.foldl (\m a -> f a >> m) (return ())

instance EqRef r => Memo (M m r a) where
	memoKey = MemoKey . refId . valueU . unM

instance EqRef r => Memo (U m r a) where
	memoKey = MemoKey . refId . valueU