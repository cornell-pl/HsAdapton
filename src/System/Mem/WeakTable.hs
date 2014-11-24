{-# LANGUAGE TypeOperators, Rank2Types, BangPatterns, FunctionalDependencies, MultiParamTypeClasses, MagicHash, ScopedTypeVariables, GADTs, FlexibleContexts, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module System.Mem.WeakTable (
	  WeakTable(..)
	, new,newForMkWeak
	, newFor
	, lookup
	, insert,delete
	, insertWith, insertWithRefKey, insertWithMkWeak
	, finalize,finalizeEntry
	, mapM_
	, mapMGeneric_,foldM
	, debugWeakTable
	) where

-- | Implementation of memo tables using hash tables and weak pointers as presented in http://community.haskell.org/~simonmar/papers/weak.pdf.
-- | Requires the package hashtables.

import Prelude hiding (lookup,mapM_)
import qualified Prelude

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

import Debug

newtype WeakTable k v = WeakTable (WeakTable' k v :!: Weak (WeakTable' k v))
type WeakTable' k v = HashIO.IOHashTable HashST.HashTable k (Weak v)
	
{-# NOINLINE new #-}
new :: (Eq k,Hashable k) => IO (WeakTable k v)
new = do
	tbl <- HashIO.new
	weak_tbl <- mkWeakKey tbl tbl $ Just $ table_finalizer tbl
	return $ WeakTable (tbl :!: weak_tbl)

{-# NOINLINE newFor #-}
-- | creates a new weak table that is uniquely identified by an argument value @a@
newFor :: (Eq k,Hashable k) => a -> IO (WeakTable k v)
newFor a = do
	tbl <- HashIO.new
	let (MkWeak mkWeak) = MkWeak (mkWeakKey tbl) `orMkWeak` MkWeak (Weak.mkWeak a)
	weak_tbl <- mkWeak tbl $ Just $ table_finalizer tbl
	return $ WeakTable (tbl :!: weak_tbl)
	
newForMkWeak :: (Eq k,Hashable k) => MkWeak -> IO (WeakTable k v)
newForMkWeak (MkWeak mkWeak) = do
	tbl <- HashIO.new
	weak_tbl <- mkWeak tbl $ Just $ table_finalizer tbl
	return $ WeakTable (tbl :!: weak_tbl)

finalize :: (Eq k,Hashable k) => WeakTable k v -> IO ()
finalize w_tbl@(WeakTable (_ :!: weak_tbl)) = do
	mb <- Weak.deRefWeak weak_tbl
	case mb of
		Nothing -> return ()
		Just weak_tbl' -> table_finalizer weak_tbl'

table_finalizer :: (Eq k,Hashable k) => WeakTable' k v -> IO ()
table_finalizer tbl = do
	pairs <- HashIO.toList tbl
	sequence_ [ Weak.finalize w | (_,w) <- pairs ]

{-# INLINE insert #-}
insert :: (Eq k,Hashable k) => WeakTable k v -> k -> v -> IO ()
insert tbl k v = System.Mem.WeakTable.insertWith tbl k k v

-- | the key @k@ stores the entry for the value @a@ in the table
insertWith :: (Eq k,Hashable k) => WeakTable k v -> a -> k -> v -> IO ()
insertWith w_tbl@(WeakTable (tbl :!: weak_tbl)) a k v = do
	weak <- Weak.mkWeak a v $ Just $ System.Mem.WeakTable.finalizeEntry' weak_tbl k
	HashIO.insert tbl k weak

insertWithMkWeak :: (Eq k,Hashable k) => WeakTable k v -> MkWeak -> k -> v -> IO ()
insertWithMkWeak w_tbl@(WeakTable (tbl :!: weak_tbl)) (MkWeak mkWeak) k v = do
	weak <- mkWeak v $ Just $ System.Mem.WeakTable.finalizeEntry' weak_tbl k
	HashIO.insert tbl k weak

-- | @insertWith@ that uses a reference as key
insertWithRefKey :: (Eq k,Hashable k,WeakRef r) => WeakTable k v -> r a -> k -> v -> IO ()
insertWithRefKey w_tbl@(WeakTable (tbl :!: weak_tbl)) a k v = do
	weak <- WeakKey.mkWeakRefKey a v $ Just $ System.Mem.WeakTable.finalizeEntry' weak_tbl k
	HashIO.insert tbl k weak

finalizeEntry :: (Eq k,Hashable k) => WeakTable k b -> k -> IO ()
finalizeEntry (WeakTable (_ :!: weak_tbl)) = finalizeEntry' weak_tbl

finalizeEntry' :: (Eq k,Hashable k) => Weak (WeakTable' k b) -> k -> IO ()
finalizeEntry' weak_tbl k = do
	r <- Weak.deRefWeak weak_tbl
	case r of
		Nothing -> return ()
		Just tbl -> HashIO.delete tbl k

lookup :: (Eq k,Hashable k) => WeakTable k v -> k -> IO (Maybe v)
lookup (WeakTable (tbl :!: weak_tbl)) k = do
	w <- HashIO.lookup tbl k
	case w of
		Nothing -> return Nothing
		Just r -> Weak.deRefWeak r

delete :: (Eq k,Hashable k) => WeakTable k v -> k -> IO ()
delete (WeakTable (tbl :!: _)) k = HashIO.delete tbl k

{-# INLINE mapM_ #-}
mapM_ :: ((k,v) -> IO ()) -> WeakTable k v -> IO ()
mapM_ f (WeakTable (tbl :!: weak_tbl)) = HashIO.mapM_ g tbl where
	g (k,weak) = do
		mbv <- Weak.deRefWeak weak
		case mbv of
			Nothing -> return ()
			Just v -> f (k,v)

{-# INLINE mapMGeneric_ #-}
mapMGeneric_ :: (Hashable k,MonadIO m,Eq k) => ((k,v) -> m ()) -> WeakTable k v -> m ()
mapMGeneric_ f (WeakTable (tbl :!: weak_tbl)) = liftIO (HashIO.toList tbl) >>= Prelude.mapM_ g where
	g (k,weak) = do
		mbv <- liftIO $ Weak.deRefWeak weak
		case mbv of
			Nothing -> return ()
			Just v -> f (k,v)
			
foldM :: (Hashable k,MonadIO m,Eq k) => (a -> (k,v) -> m a) -> a -> WeakTable k v -> m a
foldM f z (WeakTable (tbl :!: weak_tbl)) = liftIO (HashIO.toList tbl) >>= Control.Monad.foldM g z where
	g x (k,weak) = do
		mbv <- liftIO $ Weak.deRefWeak weak
		case mbv of
			Nothing -> return x
			Just v -> f x (k,v)

debugWeakTable :: (Show k,Show v) => WeakTable k v -> IO ()
debugWeakTable (WeakTable (tbl :!: weak_tbl)) = do
	let debugEntry (k,w) = do
		mb <- Weak.deRefWeak w
		case mb of
			Nothing -> return ()
			Just v -> putStrLn $ show k ++ " => " ++ show v
	putStrLn "WeakTable..."
	HashIO.mapM_ debugEntry tbl
	putStrLn "...WeakTable"



