{-# LANGUAGE DeriveDataTypeable #-}

module Control.Concurrent.MVar.Exts (
	  module Control.Concurrent.MVar
	, modifyMVarMasked_', modifyMVarMasked', readMVar, putMVar, takeMVar
	) where
		
import Control.Concurrent.MVar 
import qualified Control.Concurrent.MVar
import Control.Exception
import Data.Typeable

{-# INLINE modifyMVarMasked_' #-}
modifyMVarMasked_' :: MVar a -> (a -> IO a) -> IO ()
modifyMVarMasked_' m io =
  mask_ (do
    a  <- takeMVar m
    a' <- a `seq` io a `onException` putMVar m a
    a' `seq` putMVar m a') 

{-# INLINE modifyMVarMasked' #-}
modifyMVarMasked' :: MVar a -> (a -> IO (a,b)) -> IO b
modifyMVarMasked' m io =
  mask_ (do
    a      <- takeMVar m
    (a',b) <- a `seq` (io a >>= evaluate) `onException` putMVar m a
    a' `seq` putMVar m a'
    b `seq` return b)

