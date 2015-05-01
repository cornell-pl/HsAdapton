{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module Control.Concurrent.Future where

import Control.Concurrent.MVar
import Control.Concurrent
import GHC.Exts

class Monad m => Futuristic m where
	
	-- threads with a return value
	type family Future m :: * -> *
	type family FutureK m a :: Constraint
	
	future :: FutureK m a => m a -> m (Future m a)
	
	join :: FutureK m a => Future m a -> m a

instance Futuristic IO where
	type Future IO = MVar
	type FutureK IO a = ()
	
	future io = do
		f <- newEmptyMVar
		forkIO $ io >>= putMVar f
		return f
	join f = readMVar f