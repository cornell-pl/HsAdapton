{-# LANGUAGE TypeOperators, Rank2Types, BangPatterns, FunctionalDependencies, MultiParamTypeClasses, MagicHash, ScopedTypeVariables, GADTs, FlexibleContexts, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module System.Mem.WeakSet (
	  WeakSet(..)
	, new, fromWeakSList, purge, insertWeak, mapM_, mapM',mapM'', mapPurgeM_,toList,toWeakSList,modifyWeak,mapPurgeM,copy,copyWithKey,toListPurge
	) where

-- | Implementation of memo tables using hash tables and weak pointers as presented in http://community.haskell.org/~simonmar/papers/weak.pdf.
-- | Requires the package hashtables.

-- weak hash table for smaller sizes, encoded as an @IORef (IntMap v)@
-- the keys are used to indetify elements in the set


import Prelude hiding (lookup,mapM_)
import qualified Prelude

import System.Mem.WeakKey
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
import System.Mem.WeakKey (WeakKey(..))
import qualified System.Mem.WeakKey as WeakKey
import Data.Strict.List (SList(..))
import qualified Data.Strict.List as SList
import Data.Strict.Tuple as STuple

import Debug

-- ** weak set

newtype WeakSet v = WeakSet (WeakSet' v :!: Weak (WeakSet' v))
type WeakSet' v = IORef (SList (Weak v))

weak :: WeakSet v -> Weak (WeakSet' v)
weak (WeakSet (_ :!: w)) = w

fromWeakSList :: MonadIO m => SList (Weak v) -> m (WeakSet v)
fromWeakSList sxs = liftIO $ do
	xs <- newIORef sxs
	weak_xs <- mkWeakIORef xs $ weakset_finalizer xs
	return $ WeakSet (xs :!: weak_xs)

{-# INLINE new #-}
new :: IO (WeakSet v)
new = do
	xs <- newRef SNil
	weak_xs <- mkWeakIORef xs $ weakset_finalizer xs
	return $ WeakSet (xs :!: weak_xs)

weakset_finalizer :: WeakSet' v -> IO ()
weakset_finalizer set = do
	xs <- readIORef set
	SList.mapM_ Weak.finalize xs

{-# INLINE purge #-}
purge :: WeakSet v -> IO ()
purge (WeakSet (_ :!: w_set)) = purgeWeak w_set

{-# INLINE purgeWeak #-}
purgeWeak :: Weak (WeakSet' v) -> IO ()
purgeWeak w_set = do
	mb <- Weak.deRefWeak w_set
	case mb of
		Nothing -> return ()
		Just set -> mapIORefM_ (SList.foldM (\xs w -> purge' xs w) SNil) set
  where
	{-# INLINE purge' #-}
	purge' = \xs w -> do
		mb <- Weak.deRefWeak w
		case mb of
			Nothing -> return xs
			Just _ -> return $ SCons w xs

-- | Insert a given weak reference and adds no finalizer to the provided argument
{-# INLINE insertWeak #-}
insertWeak :: MonadIO m => WeakSet v -> Weak v -> m ()
insertWeak (WeakSet (tbl :!: _)) weak = liftIO $ modifyIORef tbl (SCons weak)

{-# INLINE mapM_ #-}
mapM_ :: MonadIO m => (v -> m ()) -> WeakSet v -> m ()
mapM_ f (WeakSet (tbl :!: _)) = liftIO (readIORef tbl) >>= SList.mapM_ g where
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
mapM' f (WeakSet (tbl :!: _)) = liftIO (readIORef tbl) >>= SList.mapM f

{-# INLINE mapM'' #-}
mapM'' :: Monad m => (forall x . IO x -> m x) -> (Weak v -> m a) -> WeakSet v -> m (SList a)
mapM'' liftIO f (WeakSet (tbl :!: _)) = liftIO (readIORef tbl) >>= SList.mapM f

{-# INLINE mapPurgeM_ #-}
mapPurgeM_ :: MonadIO m => (v -> m ()) -> WeakSet v -> m ()
mapPurgeM_ f (WeakSet (tbl :!: _)) = do
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

{-# INLINE mapPurgeM #-}
mapPurgeM :: MonadIO m => (v -> Weak v -> m (Weak v)) -> WeakSet v -> m ()
mapPurgeM f (WeakSet (tbl :!: _)) = do
	xs <- liftIO (readIORef tbl)
	xs' <- SList.foldM step SNil xs
	liftIO $ writeIORef tbl xs'
  where
	step xs weak = do
		mb <- liftIO $ Weak.deRefWeak weak
		case mb of
			Just v -> do
				weak' <- f v weak
				return $ SCons weak' xs
			Nothing -> return xs

toWeakSList :: MonadIO m => WeakSet v -> m (SList (Weak v))
toWeakSList (WeakSet (tbl :!: _)) = liftIO $ readIORef tbl

toList :: MonadIO m => WeakSet v -> m [v]
toList (WeakSet (tbl :!: _)) = toList' =<< liftIO (readIORef tbl)
	where
	toList' SNil = return []
	toList' (SCons w xs) = do
		mb <- liftIO $ Weak.deRefWeak w
		case mb of
			Nothing -> toList' xs
			Just x -> liftM (x:) $ toList' xs

toListPurge :: MonadIO m => WeakSet v -> m [v]
toListPurge (WeakSet (tbl :!: _)) = do
	set <- liftIO $ readIORef tbl
	(set',xs) <- toList' set
	liftIO $ writeIORef tbl set'
	return xs
  where
	toList' SNil = return (SNil,[])
	toList' (SCons w xs) = do
		mb <- liftIO $ Weak.deRefWeak w
		case mb of
			Nothing -> toList' xs
			Just x -> do
				(xs',lst') <- toList' xs
				return (SCons w xs',x:lst')
	
modifyWeak :: MonadIO m => WeakSet v -> (SList (Weak v) -> m (SList (Weak v))) -> m ()
modifyWeak (WeakSet (tbl :!: _)) f = do
	xs <- liftIO $ readIORef tbl
	xs' <- f xs
	liftIO $ writeIORef tbl xs'

copy :: MonadIO m => WeakSet v -> m (WeakSet v)
copy s = toWeakSList s >>= fromWeakSList

copyWithKey :: MonadIO m => (v -> MkWeak) -> WeakSet v -> m (WeakSet v)
copyWithKey getKey s = do
	xs <- toWeakSList s
	s' <- liftIO $ newIORef (error "no content")
	weak_s' <- liftIO $ mkWeakIORef s' $ return () -- the copy has no finalization, because if the copy dies we don't want it to kill the weak pointers of the original set
	let weakset' = WeakSet (s' :!: weak_s')
	
	let addFinalizers slist w = do
		mb <- Weak.deRefWeak w
		case mb of
			Nothing -> return slist
			Just x -> do
				let MkWeak mkWeak = getKey x
				mkWeak () (Just $ purgeWeak weak_s')
				return $ SCons w slist
	xs' <- liftIO $ SList.foldM addFinalizers SNil xs
	
	liftIO $ writeIORef s' xs'

	return weakset'

