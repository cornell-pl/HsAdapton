{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module Control.Concurrent.Promise where
	
import Control.Monad
import Control.Concurrent.MVar
import GHC.Exts

class Monad m => Promising m where

	type family Promise m :: * -> *
	type family PromiseK m a :: Constraint

	promise :: PromiseK m a => m (Promise m a)
	
	-- cannot be delivered more than once (further deliveries block)
	deliver :: PromiseK m a => Promise m a -> a -> m ()
	
	-- can be claimed multiple times
	claim :: PromiseK m a => Promise m a -> m a

instance Promising IO where
	
	type Promise IO = MVar
	type PromiseK IO a = ()
	
	promise = newEmptyMVar
	deliver p v = putMVar p v
	claim p = readMVar p