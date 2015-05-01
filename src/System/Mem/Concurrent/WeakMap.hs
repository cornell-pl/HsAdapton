{-# LANGUAGE TupleSections, TypeOperators, Rank2Types, BangPatterns, FunctionalDependencies, MultiParamTypeClasses, MagicHash, ScopedTypeVariables, GADTs, FlexibleContexts, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module System.Mem.Concurrent.WeakMap (
	  WeakMap(..)
	, new,new',copy'
	, lookup
	, insertWithMkWeak, insertWeak, mergeWeak
	, deleteFinalized, finalizeEntry
	, unionWithKey, extendWithKey, unionWithKey', mergeWithKey
	, toMap,toMap'
	, mapM_,mapM_',mapM'',purge, foldrM
	) where

-- | Implementation of memo tables using hash tables and weak pointers as presented in http://community.haskell.org/~simonmar/papers/weak.pdf.
-- | Requires the package hashtables.

import Prelude hiding (lookup,mapM_)
import qualified Prelude
import Control.Exception
import Control.Concurrent.MVar


import Data.Atomics
import System.Mem.Weak.Exts (Weak(..),MkWeak(..))
import qualified System.Mem.Weak.Exts as Weak
import System.IO.Unsafe
import Control.Monad hiding (mapM_,foldM)
import qualified Control.Monad
import Data.Hashable

import GHC.Base
import Control.Monad.Trans
import Data.Unique

import Data.Strict.Tuple as Strict
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map
import Data.IORef
import qualified Data.Foldable as Foldable
import Data.Strict.List as SList

import Debug

newtype WeakMap k v = WeakMap (WeakMap' k v :!: Weak (WeakMap' k v))
type WeakMap' k v = MVar (Map k (Weak v))

toMap :: MonadIO m => WeakMap k v -> m (Map k (Weak v))
toMap (WeakMap (tbl :!: _)) = liftIO $ readMVar tbl

toMap' :: (MonadIO m,Ord k) => WeakMap k v -> m (Map k v)
toMap' w = do
	m <- toMap w
	let add k w m = do
		mb <- liftIO (Weak.deRefWeak w)
		case mb of
			Nothing -> m
			Just v -> liftM (Map.insert k v) m
	liftIO $ Map.foldrWithKey add (return Map.empty) m

{-# NOINLINE new #-}
new :: (Eq k,Hashable k) => IO (WeakMap k v)
new = do
	tbl <- newMVar Map.empty
	weak_tbl <- Weak.mkWeakKey tbl tbl $ Just $ table_finalizer tbl
	return $ WeakMap (tbl :!: weak_tbl)

-- without finalization
{-# NOINLINE new' #-}
new' :: (Eq k,Hashable k) => IO (WeakMap k v)
new' = do
	tbl <- newMVar Map.empty
	weak_tbl <- Weak.mkWeakKey tbl tbl Nothing
	return $ WeakMap (tbl :!: weak_tbl)

{-# NOINLINE copy' #-}
copy' :: (Eq k,Hashable k) => WeakMap k v -> IO (WeakMap k v)
copy' (WeakMap (src_tbl :!: _)) = do
	tbl <- readMVar src_tbl >>= newMVar
	weak_tbl <- Weak.mkWeakKey tbl tbl Nothing
	return $ WeakMap (tbl :!: weak_tbl)

--{-# NOINLINE newFor #-}
---- | creates a new weak table that is uniquely identified by an argument value @a@
--newFor :: (Eq k,Hashable k) => a -> IO (WeakMap k v)
--newFor a = do
--	tbl <- CMap.empty
--	let (MkWeak mkWeak) = MkWeak (mkWeakKey tbl) `orMkWeak` MkWeak (Weak.mkWeak a)
--	weak_tbl <- mkWeak tbl $ Just $ table_finalizer tbl
--	return $ WeakMap (tbl :!: weak_tbl)
--	
--newForMkWeak :: (Eq k,Hashable k) => MkWeak -> IO (WeakMap k v)
--newForMkWeak (MkWeak mkWeak) = do
--	tbl <- newIORef Map.empty
--	weak_tbl <- mkWeak tbl $ Just $ table_finalizer tbl
--	return $ WeakMap (tbl :!: weak_tbl)

finalize :: (Eq k,Hashable k) => WeakMap k v -> IO ()
finalize w_tbl@(WeakMap (_ :!: weak_tbl)) = do
	mb <- Weak.deRefWeak weak_tbl
	case mb of
		Nothing -> return ()
		Just weak_tbl' -> table_finalizer weak_tbl'

table_finalizer :: (Eq k,Hashable k) => WeakMap' k v -> IO ()
table_finalizer tbl = do
	pairs <- readMVar tbl
	Foldable.mapM_ Weak.finalize pairs

finalizeEntry :: Ord k => WeakMap k v -> k -> IO ()
finalizeEntry (WeakMap (_ :!: weak_tbl)) k = do
	mb <- Weak.deRefWeak weak_tbl
	case mb of
		Nothing -> return ()
		Just weak_tbl' -> do
			tbl <- readMVar weak_tbl'
			case Map.lookup k tbl of
				Nothing -> return ()
				Just w -> Weak.finalize w

--{-# INLINE insert #-}
--insert :: (Eq k,Hashable k) => WeakMap k v -> k -> v -> IO ()
--insert tbl k v = insertWith tbl k k v
--
---- | the key @k@ stores the entry for the value @a@ in the table
--insertWith :: (Eq k,Hashable k) => WeakMap k v -> a -> k -> v -> IO ()
--insertWith w_tbl@(WeakMap (tbl :!: weak_tbl)) a k v = do
--	weak <- Weak.mkWeak a v $ Just $ finalizeEntry' weak_tbl k
--	CMap.insert k weak tbl
--
insertWithMkWeak :: (Ord k,Hashable k) => WeakMap k v -> MkWeak -> k -> v -> IO ()
insertWithMkWeak w_tbl@(WeakMap (tbl :!: _)) (MkWeak mkWeak) k v = do
	weak <- mkWeak v $ Just $ deleteFinalized w_tbl k
	finalizeEntry w_tbl k
	modifyMVarMasked_ tbl (return . Map.insert k weak)
	
{-# INLINE insertWeak #-}
insertWeak :: (Ord k,Hashable k,MonadIO m) => WeakMap k v -> k -> Weak v -> m ()
insertWeak (WeakMap (tbl :!: _)) k weak = liftIO $ modifyMVarMasked_ tbl (return . Map.insert k weak)

-- non-overlapping union
extendWeak :: (Ord k,Hashable k) => WeakMap k v -> k -> Weak v -> IO ()
extendWeak = mergeWeak (\_ _ -> return False)

-- non-overlapping union
mergeWeak :: (Ord k,Hashable k) => (v -> v -> IO Bool) -> WeakMap k v -> k -> Weak v -> IO ()
mergeWeak doOverwrite (WeakMap (tbl :!: _)) k weak = modifyMVarMasked_ tbl $ \m -> do
	case Map.lookup k m of
		Nothing -> do
			return $ Map.insert k weak m
		Just w -> do
			mb <- liftIO $ Weak.deRefWeak w
			case mb of
				Nothing -> return $ Map.insert k weak m
				Just oldv -> do
					mb <- liftIO $ Weak.deRefWeak weak
					case mb of
						Nothing -> return m
						Just newv -> do
							b <- doOverwrite oldv newv
							if b
								then return $ Map.insert k weak m
								else return m

-- only deletes the entry if it is already dead
deleteFinalized :: (Ord k,Hashable k) => WeakMap k v -> k -> IO ()
deleteFinalized (WeakMap (_ :!: weak_tbl)) = finalizeEntry' weak_tbl where
	finalizeEntry' weak_tbl k = do
		mb <- Weak.deRefWeak weak_tbl
		case mb of
			Nothing -> return ()
			Just r -> modifyMVarMasked_ r $ \m -> do
				case Map.lookup k m of
					Nothing -> return m
					Just w -> do
						mb <- Weak.deRefWeak w
						case mb of
							Nothing -> return $ Map.delete k m
							Just x -> return m

lookup :: (Ord k,Hashable k,MonadIO m) => WeakMap k v -> k -> m (Maybe v)
lookup (WeakMap (tbl :!: weak_tbl)) k = liftIO $ do
	xs <- readMVar tbl
	let mb = Map.lookup k xs
	case mb of
		Nothing -> return Nothing
		Just w -> Weak.deRefWeak w

-- right-biased
-- the second @WeakMap@ is not accessed concurrently
unionWithKey :: (Ord k,Hashable k,MonadIO m) => (v -> MkWeak) -> WeakMap k v -> WeakMap k v -> m ()
unionWithKey getKey wmap m@(WeakMap (tbl :!: _)) = do
	xs <- liftM Map.toList $ liftIO $ readMVar tbl
	
	let addFinalizers (k,w) = do
		mb <- Weak.deRefWeak w
		case mb of
			Nothing -> return ()
			Just x -> do
				let MkWeak mkWeak = getKey x
				mkWeak () (Just $ deleteFinalized wmap k)
				insertWeak wmap k w
	
	liftIO $ Foldable.mapM_ addFinalizers xs

-- right-biased
-- the second @WeakMap@ is not accessed concurrently
-- without adding finalizers
unionWithKey' :: (Ord k,Hashable k,MonadIO m) => WeakMap k v -> WeakMap k v -> m ()
unionWithKey' wmap m@(WeakMap (tbl :!: _)) = do
	xs <- liftM Map.toList $ liftIO $ readMVar tbl
	
	let addFinalizers (k,w) = do
		mb <- Weak.deRefWeak w
		case mb of
			Nothing -> return ()
			Just x -> insertWeak wmap k w
	
	liftIO $ Foldable.mapM_ addFinalizers xs

extendWithKey :: (Ord k,Hashable k) => (v -> MkWeak) -> WeakMap k v -> WeakMap k v -> IO ()
extendWithKey = mergeWithKey (\_ _ -> return False)

mergeWithKey :: (Ord k,Hashable k) => (v -> v -> IO Bool) -> (v -> MkWeak) -> WeakMap k v -> WeakMap k v -> IO ()
mergeWithKey merge getKey wmap m@(WeakMap (tbl :!: _)) = do
	xs <- liftM Map.toList $ liftIO $ readMVar tbl
	
	let addFinalizers (k,w) = do
		mb <- liftIO $ Weak.deRefWeak w
		case mb of
			Nothing -> return ()
			Just x -> do
				let MkWeak mkWeak = getKey x
				liftIO $ mkWeak () (Just $ deleteFinalized wmap k)
				mergeWeak merge wmap k w
	
	Foldable.mapM_ addFinalizers xs

purge :: (Ord k,Hashable k) => WeakMap k v -> IO ()
purge (WeakMap (_ :!: w_map)) = purgeWeak w_map where
	purgeWeak :: (Ord k,Hashable k) => Weak (WeakMap' k v) -> IO ()
	purgeWeak w_map = do
		mb <- Weak.deRefWeak w_map
		case mb of
			Nothing -> return ()
			Just wm -> modifyMVarMasked_ wm (\m -> Foldable.foldlM purgeMap Map.empty (Map.toList m))
	
	purgeMap :: (Ord k,Hashable k) => Map k (Weak v) -> (k,Weak v) -> IO (Map k (Weak v))
	purgeMap m (k,w) = do
			mb <- Weak.deRefWeak w
			case mb of
				Nothing -> return m
				Just v -> return $ Map.insert k w m

{-# INLINE mapM'' #-}
mapM'' :: Monad m => (forall x . IO x -> m x) -> (Weak v -> m a) -> WeakMap k v -> m [a]
mapM'' liftIO f (WeakMap (tbl :!: _)) = liftIO (readMVar tbl) >>= Control.Monad.mapM f . Map.elems

mapM_' :: Monad m => (forall x . IO x -> m x) -> ((k,v) -> m a) -> WeakMap k v -> m ()
mapM_' liftIO f (WeakMap (tbl :!: _)) = liftIO (readMVar tbl) >>= Control.Monad.mapM_ g . Map.toAscList where
	g (k,w) = do
		mb <- liftIO $ Weak.deRefWeak w
		case mb of
			Nothing -> return ()
			Just v -> f (k,v) >> return ()

mapM_ :: MonadIO m => ((k,v) -> m a) -> WeakMap k v -> m ()			
mapM_ = mapM_' liftIO

foldrM :: MonadIO m => ((k,v) -> b -> m b) -> b -> WeakMap k v -> m b
foldrM f z (WeakMap (tbl :!: _)) = do
	xs <- liftIO $ readMVar tbl
	let dof k w m = do
		mb <- liftIO $ Weak.deRefWeak w
		case mb of
			Nothing -> m
			Just v -> m >>= f (k,v)
	Map.foldrWithKey dof (return z) xs


