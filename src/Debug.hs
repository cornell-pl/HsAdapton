module Debug where

-- just an easy way to turn debugging on/off

import Debug.Trace

import Data.IORef
import qualified Data.HashTable.IO as HashIO
import qualified Data.HashTable.ST.Basic as HashST
import System.IO.Unsafe
import System.Mem.Weak
import System.Mem.WeakRef
import Control.Monad.IO.Class
import Data.Unique
import Control.Monad.Ref
import Data.Hashable
import Control.Concurrent

import Data.UUID
import Data.UUID.V1

debug :: String -> a -> a
debug str = id
--debug str x = trace str x

--type RefDB = HashIO.IOHashTable HashST.HashTable UUID (IO ())
--
--{-# NOINLINE refDB #-}
--refDB :: RefDB
--refDB = unsafePerformIO $ HashIO.new
--
--newRefDB :: (MonadRef r m,WeakRef r,MonadIO m) => a -> String -> m (r a)
--newRefDB v msg = do newRef v
----	r <- newRef v
----	uid <- liftIO $ nextUUIDSafe
----	w <- liftIO $ mkWeakRef r (HashIO.delete refDB uid >> putStrLn (msg ++" has died"))
----	liftIO $ HashIO.insert refDB uid $ do
----		mb <- deRefWeak w
----		case mb of
----			Nothing -> finalize w
----			Just _ -> putStrLn $ msg ++" is alive"
----	return r
--
--printDB :: IO ()
--printDB = do
--	db <- HashIO.toList refDB
--	sequence_ $ map snd db
