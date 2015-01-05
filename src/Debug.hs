module Debug where

-- just an easy way to turn debugging on/off

import Debug.Trace

import Data.IORef
import qualified Data.HashTable.IO as HashIO
import qualified Data.HashTable.ST.Basic as HashST
import System.IO.Unsafe
import System.Mem.Weak
import System.Mem.WeakKey
import Control.Monad.IO.Class
import Data.Unique
import Control.Monad.Ref
import Data.Hashable
import Control.Concurrent

import Data.UUID
import Data.UUID.V1

debug :: String -> a -> a
--debug str = id
debug str x = trace str x

debugM :: MonadIO m => String -> m a -> m a
debugM str m = do
	threadid <- liftIO $ myThreadId
	debug ("{"++ show threadid ++ "}" ++ str) m
