{-# LANGUAGE TypeOperators, Rank2Types, BangPatterns, FunctionalDependencies, MultiParamTypeClasses, MagicHash, ScopedTypeVariables, GADTs, FlexibleContexts, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module System.Mem.Concurrent.WeakMap (
	  WeakMap(..)
	, new
--	,newForMkWeak
--	, newFor
--	, lookup
--	, insert,delete
--	, insertWith, insertWithRefKey
	, insertWithMkWeak, insertWeak
--	, finalize,
	, deleteFinalized
--	, mapM_
--	, mapMGeneric_,foldM
	, copyWithKey
	, unionWithKey, extendWithKey
	, atomicModifyWeakMap_
	, toMap
	, mapM_,mapM''
	) where

-- | Implementation of memo tables using hash tables and weak pointers as presented in http://community.haskell.org/~simonmar/papers/weak.pdf.
-- | Requires the package hashtables.

import Prelude hiding (lookup,mapM_)
import qualified Prelude

import Data.Atomics
import System.Mem.Weak (Weak(..))
import qualified System.Mem.Weak as Weak
import System.IO.Unsafe
import qualified Data.HashTable.IO as HashIO
import qualified Data.HashTable.ST.Basic as HashST
import Control.Monad hiding (mapM_,foldM)
import qualified Control.Monad
import Data.Hashable

import GHC.Base
import Control.Monad.Trans
import Data.Unique
import Control.Monad.Ref
import System.Mem.WeakKey as WeakKey
import qualified System.Mem.WeakKey as WeakKey
import Data.Strict.Tuple as Strict
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.IORef
import Data.Foldable as Foldable
import Data.Strict.List as SList

import Debug

newtype WeakMap k v = WeakMap (WeakMap' k v :!: Weak (WeakMap' k v))
type WeakMap' k v = IORef (Map k (Weak v))

toMap :: MonadIO m => WeakMap k v -> m (Map k (Weak v))
toMap (WeakMap (tbl :!: _)) = liftIO $ readIORef tbl

{-# NOINLINE new #-}
new :: (Eq k,Hashable k) => IO (WeakMap k v)
new = do
	tbl <- newIORef Map.empty
	weak_tbl <- mkWeakKey tbl tbl $ Just $ table_finalizer tbl
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
--	tbl <- CMap.empty
--	weak_tbl <- mkWeak tbl $ Just $ table_finalizer tbl
--	return $ WeakMap (tbl :!: weak_tbl)
--
--finalize :: (Eq k,Hashable k) => WeakMap k v -> IO ()
--finalize w_tbl@(WeakMap (_ :!: weak_tbl)) = do
--	mb <- Weak.deRefWeak weak_tbl
--	case mb of
--		Nothing -> return ()
--		Just weak_tbl' -> table_finalizer weak_tbl'

table_finalizer :: (Eq k,Hashable k) => WeakMap' k v -> IO ()
table_finalizer tbl = do
	pairs <- readIORef tbl
	Foldable.mapM_ Weak.finalize pairs

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
	atomicModifyWeakMap'CAS_ tbl (return . Map.insert k weak)
	
{-# INLINE insertWeak #-}
insertWeak :: (Ord k,Hashable k,MonadIO m) => WeakMap k v -> k -> Weak v -> m ()
insertWeak (WeakMap (tbl :!: _)) k weak = liftIO $ atomicModifyWeakMap'CAS_ tbl (return . Map.insert k weak)

-- non-overlapping union
extendWeak :: (Ord k,Hashable k,MonadIO m) => WeakMap k v -> k -> Weak v -> m ()
extendWeak (WeakMap (tbl :!: _)) k weak = liftIO $ atomicModifyWeakMap'CAS_ tbl $ \m -> do
	case Map.lookup k m of
		Nothing -> do
			return $ Map.insert k weak m
		Just w -> do
			mb <- Weak.deRefWeak w
			case mb of
				Nothing -> return $ Map.insert k weak m
				otherwise -> return m
	
--
---- | @insertWith@ that uses a reference as key
--insertWithRefKey :: (Eq k,Hashable k,WeakRef r) => WeakMap k v -> r a -> k -> v -> IO ()
--insertWithRefKey w_tbl@(WeakMap (tbl :!: weak_tbl)) a k v = do
--	weak <- WeakKey.mkWeakRefKey a v $ Just $ finalizeEntry' weak_tbl k
--	CMap.insert k weak tbl

-- only deletes the entry if it is already dead
deleteFinalized :: (Ord k,Hashable k) => WeakMap k v -> k -> IO ()
deleteFinalized (WeakMap (_ :!: weak_tbl)) = finalizeEntry' weak_tbl where
	finalizeEntry' weak_tbl k = do
		mb <- Weak.deRefWeak weak_tbl
		case mb of
			Nothing -> return ()
			Just r -> atomicModifyWeakMap'CAS_ r $ \m -> do
				case Map.lookup k m of
					Nothing -> return m
					Just w -> do
						mb <- Weak.deRefWeak w
						case mb of
							Nothing -> return $ Map.delete k m
							Just x -> return m

--lookup :: (Eq k,Hashable k) => WeakMap k v -> k -> IO (Maybe v)
--lookup (WeakMap (tbl :!: weak_tbl)) k = do
--	w <- CMap.lookup k tbl
--	case w of
--		Nothing -> return Nothing
--		Just r -> Weak.deRefWeak r
--
--delete :: (Eq k,Hashable k) => WeakMap k v -> k -> IO ()
--delete (WeakMap (tbl :!: _)) k = do
--	tick <- readForCAS
--	(success,casIORef tbl tick
--	CMap.delete k tbl
--
----{-# INLINE mapM_ #-}
----mapM_ :: ((k,v) -> IO ()) -> WeakMap k v -> IO ()
----mapM_ f (WeakMap (tbl :!: weak_tbl)) = HashIO.mapM_ g tbl where
----	g (k,weak) = do
----		mbv <- Weak.deRefWeak weak
----		case mbv of
----			Nothing -> return ()
----			Just v -> f (k,v)
----
----{-# INLINE mapMGeneric_ #-}
----mapMGeneric_ :: (Hashable k,MonadIO m,Eq k) => ((k,v) -> m ()) -> WeakMap k v -> m ()
----mapMGeneric_ f (WeakMap (tbl :!: weak_tbl)) = liftIO (HashIO.toList tbl) >>= Prelude.mapM_ g where
----	g (k,weak) = do
----		mbv <- liftIO $ Weak.deRefWeak weak
----		case mbv of
----			Nothing -> return ()
----			Just v -> f (k,v)
----			
----foldM :: (Hashable k,MonadIO m,Eq k) => (a -> (k,v) -> m a) -> a -> WeakMap k v -> m a
----foldM f z (WeakMap (tbl :!: weak_tbl)) = liftIO (HashIO.toList tbl) >>= Control.Monad.foldM g z where
----	g x (k,weak) = do
----		mbv <- liftIO $ Weak.deRefWeak weak
----		case mbv of
----			Nothing -> return x
----			Just v -> f x (k,v)
--
copyWithKey :: (Ord k,Hashable k,MonadIO m) => (v -> MkWeak) -> WeakMap k v -> m (WeakMap k v)
copyWithKey getKey (WeakMap (tbl :!: weak_tbl)) = do
	xs <- liftM Map.toList $ liftIO $ readIORef tbl
	tbl' <- liftIO $ newIORef Map.empty
	weak_tbl' <- liftIO $ mkWeakKey tbl' tbl' $ Just $ return () -- the copy has no finalization, because if the copy dies we don't want it to kill the weak pointers of the original set
	let weaktbl' = WeakMap (tbl' :!: weak_tbl')
	
	let addFinalizers (k,w) = do
		mb <- Weak.deRefWeak w
		case mb of
			Nothing -> return ()
			Just x -> do
				let MkWeak mkWeak = getKey x
				mkWeak () (Just $ deleteFinalized weaktbl' k)
				insertWeak weaktbl' k w
	xs' <- liftIO $ Control.Monad.mapM_ addFinalizers xs

	return weaktbl'


-- the second @WeakMap@ is not accessed concurrently
unionWithKey :: (Ord k,Hashable k,MonadIO m) => (v -> MkWeak) -> WeakMap k v -> WeakMap k v -> m ()
unionWithKey getKey wmap m@(WeakMap (tbl :!: _)) = do
	xs <- liftM Map.toList $ liftIO $ readIORef tbl
	
	let addFinalizers (k,w) = do
		mb <- Weak.deRefWeak w
		case mb of
			Nothing -> return ()
			Just x -> do
				let MkWeak mkWeak = getKey x
				mkWeak () (Just $ deleteFinalized wmap k)
				insertWeak wmap k w
	
	liftIO $ Foldable.mapM_ addFinalizers xs

extendWithKey :: (Ord k,Hashable k,MonadIO m) => (v -> MkWeak) -> WeakMap k v -> WeakMap k v -> m ()
extendWithKey getKey wmap m@(WeakMap (tbl :!: _)) = do
	xs <- liftM Map.toList $ liftIO $ readIORef tbl
	
	let addFinalizers (k,w) = do
		mb <- Weak.deRefWeak w
		case mb of
			Nothing -> return ()
			Just x -> do
				let MkWeak mkWeak = getKey x
				mkWeak () (Just $ deleteFinalized wmap k)
				extendWeak wmap k w
	
	liftIO $ Foldable.mapM_ addFinalizers xs

-- not that this is highly unsafe to use in general, since the side-effects are not revocable
atomicModifyWeakMap'CAS_ :: WeakMap' k v -> (Map k (Weak v) -> IO (Map k (Weak v))) -> IO ()
atomicModifyWeakMap'CAS_ tbl f = readForCAS tbl >>= loop
	where
	loop tick = do
		v' <- f (peekTicket tick)
		(success,tick') <- casIORef tbl tick v'
		if success then return () else loop tick'

atomicModifyWeakMap_ :: MonadIO m => WeakMap k v -> (Map k (Weak v) -> Map k (Weak v)) -> m ()
atomicModifyWeakMap_ (WeakMap (tbl :!: _)) f = liftIO $ atomicModifyIORef tbl (\x -> (f x,()))

purge :: (Ord k,Hashable k) => WeakMap k v -> IO ()
purge (WeakMap (_ :!: w_map)) = purgeWeak w_map where
	purgeWeak :: (Ord k,Hashable k) => Weak (WeakMap' k v) -> IO ()
	purgeWeak w_map = do
		mb <- Weak.deRefWeak w_map
		case mb of
			Nothing -> return ()
			Just wm -> atomicModifyWeakMap'CAS_ wm (\m -> Foldable.foldlM purgeMap Map.empty (Map.toList m))
	
	purgeMap :: (Ord k,Hashable k) => Map k (Weak v) -> (k,Weak v) -> IO (Map k (Weak v))
	purgeMap m (k,w) = do
			mb <- Weak.deRefWeak w
			case mb of
				Nothing -> return m
				Just v -> return $ Map.insert k w m

{-# INLINE mapM'' #-}
mapM'' :: Monad m => (forall x . IO x -> m x) -> (Weak v -> m a) -> WeakMap k v -> m [a]
mapM'' liftIO f (WeakMap (tbl :!: _)) = liftIO (readIORef tbl) >>= Control.Monad.mapM f . Map.elems





