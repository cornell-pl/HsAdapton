{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, GADTs, KindSignatures #-}

module Control.Monad.BiAdapton.DataTypes where

import Control.Monad.Ref
import Data.Typeable
import System.Mem.MemoTable
import Data.IntMap (IntMap(..))
import qualified Data.IntMap as IntMap
import Data.Map (Map(..))
import qualified Data.Map as Map
import Control.Monad.State as State
import Control.Monad.ST.Lazy
import Data.STRef.Lazy
import Control.Monad.Box

data Direction = Forward | Backward deriving (Eq,Show,Typeable)

opposite Forward = Backward
opposite Backward = Forward

isForward Forward = True
isForward Backward = False

isBackward = not . isForward

bx :: a -> a -> (Direction -> a)
bx fwd bwd Forward = fwd
bx fwd bwd Backward = bwd

-- | Thunk of @a@
data U (m :: * -> *) (r :: * -> *) a = U {
		idU :: Id
	,	valueU :: IdRef r (Maybe a)
	,	directionU :: r Direction
	,	forceU :: Direction -> r (Inner m r a)
	,	dependentsU :: Direction -> r (Dependents m r)
	,	dependenciesU :: Direction -> r (Dependencies m r)
	,	dirtyU :: Direction -> r Bool
	,	memoU :: Direction -> r (MemoTree m r)
	,	traceU :: Direction -> r (Trace m r)
	} deriving Typeable

type Dependents m r = IntMap (SomeU m r)
type Dependencies m r = [(Int,SomeU m r)] -- shouldn't this be a list as in the paper? order matters
type Trace m r = [(Computation m r,SomeValue)]

type CallStack m r = Maybe (SomeU m r) -- we don't really need a stack, just the cuurrent caller should be enough

data MemoTree m r = Leaf SomeValue
				  | Node (Computation m r) (Map MemoKey (MemoTree m r))
				  | Empty

type Computation m r = (SomeU m r)

data SomeValue where
	SomeValue :: (Typeable a,Memo a) => a -> SomeValue

data SomeU m r where
	SomeU :: (Memo a,Typeable m,Typeable r,Typeable a) => U m r a -> SomeU m r

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

instance Ref m r => Ref (Inner m r) r where
	newRef v = inL $ newRef v
	readRef r = inL $ readRef r
	writeRef r v = inL $ writeRef r v
	
instance Ref m r => Ref (Outer m r) r where
	newRef v = inL $ newRef v
	readRef r = inL $ readRef r
	writeRef r v = inL $ writeRef r v

instance (Ref m (STRef s),Box (STRef s) m) => Box (STRef s) (Inner m (STRef s)) where
	box v = inL $ box v
	unbox k = inL $ unbox k
	inbox k f = mapRefM f k
	
