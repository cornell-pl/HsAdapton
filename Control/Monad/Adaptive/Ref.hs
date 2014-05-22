{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
-- A class for monads with mutable references

module Control.Monad.Adaptive.Ref where

import Control.Monad.ST
import Data.IORef
import Data.STRef
import Control.Monad.State (State(..),StateT(..))
import Control.Monad.State as State

import System.IO.Unsafe

import Debug.Trace

class EqRef r where
    eqRef :: r a -> r a -> Bool

class (RefId r,EqRef r, Functor m, Monad m) => Ref m r where
  newRef   :: a -> m (r a)
  readRef  :: r a -> m a
  writeRef :: r a -> a -> m ()

class RefId r where
	refId :: r a -> Int

instance Ref IO ShowRef where
	newRef x = newIORef x >>= return . ShowRef (-1)
	readRef (ShowRef i r) = readIORef r
	writeRef (ShowRef i r) = writeIORef r

--instance EqRef (STRef s) where eqRef = (==)
--
--instance Ref (ST s) (STRef s) where
--  newRef = newSTRef
--  readRef = readSTRef
--  writeRef = writeSTRef

instance RefId IORef where
	refId r = (-1)

instance EqRef IORef where
	eqRef = (==)

instance Ref IO IORef where
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef

instance EqRef ShowRef where
	eqRef (ShowRef i r) (ShowRef i' r') = r == r'

instance Ref (StateT Int IO) ShowRef where
	newRef x = do
		i <- State.get
		State.put (succ i)
		r <- lift $ newIORef x
		return $ ShowRef i r
	readRef (ShowRef i r) = lift $ readIORef r
	writeRef (ShowRef i r) x = lift $ writeIORef r x

data ShowRef a = ShowRef Int (IORef a)

instance Show a => Show (ShowRef a) where
	show (ShowRef i r) = "<ref " ++ show i ++ " " ++ unsafePerformIO (readRef r >>= return . show) ++ ">"

instance RefId ShowRef where
	refId (ShowRef i r) = i

mapRef :: Ref m r => (a -> a) -> r a -> m ()
mapRef f r = readRef r >>= writeRef r . f


data RefStateT s r m a = RefStateT (r s -> m a)

refStateT :: RefStateT s r m a -> r s -> m a
refStateT (RefStateT f) r = f r

runRefStateT :: Ref m r => RefStateT s r m a -> s -> m (a,s)
runRefStateT m s = do
	r <- newRef s
	x <- refStateT m r
	s' <- readRef r
	return (x,s')

evalRefStateT :: (Monad m,Ref m r) => RefStateT s r m a -> s -> m a
evalRefStateT m s = liftM fst $ runRefStateT m s

execRefStateT :: (Monad m,Ref m r) => RefStateT s r m a -> s -> m s
execRefStateT m s = liftM snd $ runRefStateT m s

mapRefStateT :: (m a -> n b) -> RefStateT s r m a -> RefStateT s r n b
mapRefStateT f m = RefStateT $ \r -> f $ refStateT m r
	
	
	--RefStateT $ f . refStateT m

instance Monad m => Monad (RefStateT s r m) where
	return x = RefStateT $ \r -> return x
	m >>= g = RefStateT $ \r -> refStateT m r >>= \x -> refStateT (g x) r

instance Ref m r => MonadState s (RefStateT s r m) where
	get = RefStateT $ \r -> readRef r
	put s = RefStateT $ \r -> writeRef r s
	state f = RefStateT $ \r -> readRef r >>= \s -> let (x,s') = f s in writeRef r s' >> return x

instance MonadTrans (RefStateT s r) where
	lift m = RefStateT $ \r -> m
	

	
	
	
