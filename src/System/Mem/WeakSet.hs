{-# LANGUAGE BangPatterns, FunctionalDependencies, MultiParamTypeClasses, MagicHash, ScopedTypeVariables, GADTs, FlexibleContexts, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module System.Mem.WeakSet (
	  WeakSet(..)
	, new, purge, insertWeak, mapM_, mapM', mapPurgeM_,toList
	) where

-- | Implementation of memo tables using hash tables and weak pointers as presented in http://community.haskell.org/~simonmar/papers/weak.pdf.
-- | Requires the package hashtables.

-- weak hash table for smaller sizes, encoded as an @IORef (IntMap v)@
-- the keys are used to indetify elements in the set


import Prelude hiding (lookup,mapM_)
import qualified Prelude

import System.Mem.Weak (Weak(..))
import qualified System.Mem.Weak as Weak
import System.IO.Unsafe
import Data.IORef
import Control.Monad hiding (mapM_)
import Data.Hashable

import GHC.Base
import Control.Monad.Trans
import Data.Unique
import Control.Monad.Ref
import System.Mem.WeakRef (WeakRef(..))
import qualified System.Mem.WeakRef as WeakRef
import Data.Strict.List (SList(..))
import qualified Data.Strict.List as SList

import Debug

type WeakSet v = IORef (SList (Weak v))

{-# INLINE new #-}
new :: IO (WeakSet v)
new = newRef SNil

{-# INLINE purge #-}
purge :: WeakSet v -> IO ()
purge = mapIORefM_ (SList.foldM (\xs w -> purge' xs w) SNil)
{-# INLINE purge' #-}
purge' = \xs w -> do
	mb <- Weak.deRefWeak w
	case mb of
		Nothing -> return xs
		Just _ -> return $ SCons w xs

-- | Insert a given weak reference and adds no finalizer to the provided argument
{-# INLINE insertWeak #-}
insertWeak ::  WeakSet v -> Weak v -> IO ()
insertWeak tbl weak = modifyIORef tbl (SCons weak)

{-# INLINE mapM_ #-}
mapM_ :: MonadIO m => (v -> m ()) -> WeakSet v -> m ()
mapM_ f tbl = liftIO (readIORef tbl) >>= SList.mapM_ g where
	{-# INLINE g #-}
	g weak = do
		mbv <- liftIO $ Weak.deRefWeak weak
		case mbv of
			Nothing -> return ()
			Just v -> f v

{-# INLINE mapIORefM_ #-}
mapIORefM_ :: (a -> IO a) -> IORef a -> IO ()
mapIORefM_ f r = readIORef r >>= f >>= writeIORef r

{-# INLINE mapM' #-}
mapM' :: MonadIO m => (Weak v -> m a) -> WeakSet v -> m (SList a)
mapM' f tbl = liftIO (readIORef tbl) >>= SList.mapM f

{-# INLINE mapPurgeM_ #-}
mapPurgeM_ :: MonadIO m => (v -> m ()) -> WeakSet v -> m ()
mapPurgeM_ f tbl = do
	xs <- liftIO (readIORef tbl)
	xs' <- SList.foldM step SNil xs
	liftIO $ writeIORef tbl xs'
  where
	step xs weak = do
		mb <- liftIO $ Weak.deRefWeak weak
		case mb of
			Just v -> do
				f v
				return $ SCons weak xs
			Nothing -> return xs

toList :: MonadIO m => WeakSet v -> m [v]
toList = toList' <=< liftIO . readIORef
	where
	toList' SNil = return []
	toList' (SCons w xs) = do
		mb <- liftIO $ Weak.deRefWeak w
		case mb of
			Nothing -> toList' xs
			Just x -> liftM (x:) $ toList' xs
	
	
