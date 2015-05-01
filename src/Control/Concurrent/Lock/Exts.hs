{-# LANGUAGE DeriveDataTypeable #-}

module Control.Concurrent.Lock.Exts (
	  module Control.Concurrent.Lock
	, tryWait, tryWaits, tryAcquires, releases, tryAcquire, with, wait, acquire
	) where

import Control.Concurrent.Lock hiding (tryAcquire,with,wait,acquire)
import qualified Control.Concurrent.Lock
import Data.Foldable as Foldable
import Control.Monad
import Control.Exception
import Data.Maybe
import Data.Typeable

acquire msg t = Control.Concurrent.Lock.acquire t `catch` catchLock msg

wait msg t = Control.Concurrent.Lock.wait t `catch` catchLock msg

tryAcquire msg t = Control.Concurrent.Lock.tryAcquire t `catch` catchLock msg

with msg t m = Control.Concurrent.Lock.with t m `catch` catchLock msg

{-# INLINE tryWait #-}
tryWait :: String -> Lock -> IO Bool
tryWait msg mv = liftM isJust (tryWith mv $ return ()) `catch` catchLock msg
{-# INLINE tryWaits #-}
tryWaits :: (Foldable t) => String -> t Lock -> IO Bool
tryWaits msg t = Foldable.foldlM (\b lck -> liftM (b&&) $ tryWait msg lck) True t `catch` catchLock msg
{-# INLINE tryAcquires #-}
tryAcquires :: (Foldable t) => String -> t Lock -> IO Bool
tryAcquires msg xs = mask_ (do
	let acq lck m = do
		x <- tryAcquire msg lck
		if x
			then liftM (\(b,lcks) -> (b,lck:lcks)) m
			else return (False,[]) -- don't acquire the tail
	(b,acquired) <- Foldable.foldr acq (return (True,[])) xs
	unless b $ releases msg acquired
	return b) `catch` catchLock msg
{-# INLINE releases #-}
releases :: (Foldable t) => String -> t Lock -> IO ()
releases msg t = Foldable.mapM_ release t `catch` catchLock msg


catchLock :: String -> BlockedIndefinitelyOnMVar -> IO a
catchLock msg e = throw $ BlockedLock msg e

data BlockedLock e = BlockedLock String e deriving (Eq,Show,Typeable)
instance Exception e => Exception (BlockedLock e)