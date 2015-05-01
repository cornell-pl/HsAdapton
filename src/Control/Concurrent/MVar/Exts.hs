{-# LANGUAGE DeriveDataTypeable #-}

module Control.Concurrent.MVar.Exts (
	  module Control.Concurrent.MVar
	, modifyMVarMasked_', modifyMVarMasked', readMVar, putMVar, takeMVar
	) where
		
import Control.Concurrent.MVar hiding (readMVar,putMVar, takeMVar)
import qualified Control.Concurrent.MVar
import Control.Exception
import Data.Typeable

{-# INLINE modifyMVarMasked_' #-}
modifyMVarMasked_' :: String -> MVar a -> (a -> IO a) -> IO ()
modifyMVarMasked_' msg m io =
  mask_ (do
    a  <- takeMVar msg m
    a' <- a `seq` io a `onException` putMVar msg m a
    a' `seq` putMVar msg m a') -- `catch` catchMVar msg

{-# INLINE modifyMVarMasked' #-}
modifyMVarMasked' :: String -> MVar a -> (a -> IO (a,b)) -> IO b
modifyMVarMasked' msg m io =
  mask_ (do
    a      <- takeMVar msg m
    (a',b) <- a `seq` (io a >>= evaluate) `onException` putMVar msg m a
    a' `seq` putMVar msg m a'
    b `seq` return b) -- `catch` catchMVar msg

readMVar :: String -> MVar a -> IO a
readMVar msg m = Control.Concurrent.MVar.readMVar m -- `catch` catchMVar msg

putMVar :: String -> MVar a -> a -> IO ()
putMVar msg m v = Control.Concurrent.MVar.putMVar m v -- `catch` catchMVar msg

takeMVar :: String -> MVar a -> IO a
takeMVar msg m = Control.Concurrent.MVar.takeMVar m -- `catch` catchMVar msg

--catchMVar :: String -> BlockedIndefinitelyOnMVar -> IO a
--catchMVar msg e = throw $ BlockedMVar msg e

--data BlockedMVar e = BlockedMVar String e deriving (Eq,Show,Typeable)
--instance Exception e => Exception (BlockedMVar e)