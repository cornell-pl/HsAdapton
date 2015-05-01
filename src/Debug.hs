{-# LANGUAGE TemplateHaskell #-}

module Debug where

-- just an easy way to turn debugging on/off

import Debug.Trace

import Data.IORef
import System.IO.Unsafe
import System.Mem.Weak

import Control.Monad.IO.Class
import Data.Unique
import Data.Hashable
import Control.Concurrent
import Data.Global.TH as TH
import Control.Monad
import Control.Exception

import Data.UUID
import Data.UUID.V1

debug :: String -> a -> a
debug str = id
--debug str x = trace str x

-- a channel for synchronous stdout debugging messages
TH.declareChan "debugChan" [t| String |]

-- makes sure to empty the buffer before killing the debugger thread
debugger :: IO ()
debugger = flip finally (getChanContents debugChan >>= Control.Monad.mapM_ putStrLn) $ do
    readChan debugChan >>= putStrLn
    debugger

debugM :: MonadIO m => String -> m a -> m a
debugM str m = do
	threadid <- liftIO $ myThreadId
	liftIO $ writeChan debugChan $ ("{"++ show threadid ++ "}" ++ str)
	m