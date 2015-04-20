module Control.Concurrent.Lock.Exts (
	  module Control.Concurrent.Lock
	, tryWait, tryWaits, tryAcquires, releases
	) where

import Control.Concurrent.Lock
import Data.Foldable as Foldable
import Control.Monad
import Control.Exception
import Data.Maybe

{-# INLINE tryWait #-}
tryWait :: Lock -> IO Bool
tryWait mv = liftM isJust $ tryWith mv $ return ()
{-# INLINE tryWaits #-}
tryWaits :: (Foldable t) => t Lock -> IO Bool
tryWaits = Foldable.foldlM (\b lck -> liftM (b&&) $ tryWait lck) True
{-# INLINE tryAcquires #-}
tryAcquires :: (Foldable t) => t Lock -> IO Bool
tryAcquires xs = mask_ $ do
	let acq lck m = do
		x <- tryAcquire lck
		if x
			then liftM (\(b,lcks) -> (b,lck:lcks)) m
			else return (False,[]) -- don't acquire the tail
	(b,acquired) <- Foldable.foldr acq (return (True,[])) xs
	unless b $ releases acquired
	return b
{-# INLINE releases #-}
releases :: (Foldable t) => t Lock -> IO ()
releases = Foldable.mapM_ release
