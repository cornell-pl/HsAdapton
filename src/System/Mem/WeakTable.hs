{-# LANGUAGE DeriveDataTypeable, TypeOperators, Rank2Types, BangPatterns, FunctionalDependencies, MultiParamTypeClasses, MagicHash, ScopedTypeVariables, GADTs, FlexibleContexts, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module System.Mem.WeakTable (
	  WeakTable(..),WeakKey(..)
	, new,newForMkWeak
	, newFor
	, lookup
	, insert,delete
	, insertWith, insertWithMkWeak
	, finalize,table_finalizer,finalizeEntry
	, mapM_
	, mapMGeneric_,mapMGeneric__,foldM
	, debugWeakTable
	, stableName
	) where

-- | Implementation of memo tables using hash tables and weak pointers as presented in http://community.haskell.org/~simonmar/papers/weak.pdf.
-- | Requires the package hashtables.

import Prelude hiding (lookup,mapM_)
import qualified Prelude
import System.Mem.StableName

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
import Data.IORef

import Debug
import Data.Typeable

-- an @IORef@ so that we have precise weak pointers
newtype WeakTable k v = WeakTable (IORef (WeakTable' k v) :!: Weak (WeakTable' k v)) deriving Typeable
type WeakTable' k v = HashIO.IOHashTable HashST.HashTable k (Weak v)
	
instance WeakKey (WeakTable k v) where
	mkWeakKey (WeakTable (r :!: _)) = mkWeakKey r
	
new :: (Eq k,Hashable k) => IO (WeakTable k v)
new = do
	tbl <- HashIO.new
	tbl_ref <- newIORef tbl
	weak_tbl <- mkWeakKey tbl_ref tbl $ Just $ table_finalizer tbl
	return $ WeakTable (tbl_ref :!: weak_tbl)

-- | creates a new weak table that is uniquely identified by an argument value @a@
newFor :: (Eq k,Hashable k) => a -> IO (WeakTable k v)
newFor a = do
	tbl <- HashIO.new
	tbl_ref <- newIORef tbl
	let (MkWeak mkWeak) = MkWeak (mkWeakKey tbl_ref) `orMkWeak` MkWeak (Weak.mkWeak a)
	weak_tbl <- mkWeak tbl $ Just $ table_finalizer tbl
	return $ WeakTable (tbl_ref :!: weak_tbl)

newForMkWeak :: (Eq k,Hashable k) => MkWeak -> IO (WeakTable k v)
newForMkWeak (MkWeak mkWeak) = do
	tbl <- HashIO.new
	tbl_ref <- newIORef tbl
	weak_tbl <- mkWeak tbl $ Just $ table_finalizer tbl
	return $ WeakTable (tbl_ref :!: weak_tbl)

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
insertWith w_tbl@(WeakTable (tbl_ref :!: weak_tbl)) a k v = do
	tbl <- readIORef tbl_ref
	weak <- Weak.mkWeak a v $ Just $ System.Mem.WeakTable.deleteFinalized' weak_tbl k
	HashIO.insert tbl k weak

insertWithMkWeak :: (MonadIO m,Eq k,Hashable k) => WeakTable k v -> MkWeak -> k -> v -> m ()
insertWithMkWeak w_tbl@(WeakTable (tbl_ref :!: weak_tbl)) (MkWeak mkWeak) k v = do
	tbl <- liftIO $ readIORef tbl_ref
	liftIO $ finalizeEntry w_tbl k -- we need to make sure that any old weak pointer is finalized, since it may be be kept alive by the same key
	weak <- liftIO $ mkWeak v $ Just $ System.Mem.WeakTable.deleteFinalized' weak_tbl k
	liftIO $ HashIO.insert tbl k weak

---- | @insertWith@ that uses a reference as key
--insertWithRefKey :: (Eq k,Hashable k,WeakRef r) => WeakTable k v -> r a -> k -> v -> IO ()
--insertWithRefKey w_tbl@(WeakTable (tbl_ref :!: weak_tbl)) a k v = do
--	tbl <- readIORef tbl_ref
--	weak <- WeakKey.mkWeakRefKey a v $ Just $ System.Mem.WeakTable.deleteFinalized' weak_tbl k
--	HashIO.insert tbl k weak

finalizeEntry :: (Eq k,Hashable k) => WeakTable k b -> k -> IO ()
finalizeEntry (WeakTable (_ :!: weak_tbl)) k = do
	r <- Weak.deRefWeak weak_tbl
	case r of
		Nothing -> return ()
		Just tbl -> do
			mb <- HashIO.lookup tbl k
			case mb of
				Nothing -> return ()
				Just b -> Weak.finalize b

deleteFinalized :: (Eq k,Hashable k) => WeakTable k b -> k -> IO ()
deleteFinalized (WeakTable (_ :!: weak_tbl)) = deleteFinalized' weak_tbl

deleteFinalized' :: (Eq k,Hashable k) => Weak (WeakTable' k b) -> k -> IO ()
deleteFinalized' weak_tbl k = do
	r <- Weak.deRefWeak weak_tbl
	case r of
		Nothing -> return ()
		Just tbl -> HashIO.delete tbl k

lookup :: (MonadIO m,Eq k,Hashable k) => WeakTable k v -> k -> m (Maybe v)
lookup (WeakTable (tbl_ref :!: weak_tbl)) k = do
	tbl <- liftIO $ readIORef tbl_ref
	w <- liftIO $ HashIO.lookup tbl k
	case w of
		Nothing -> return Nothing
		Just r -> liftIO $ Weak.deRefWeak r

delete :: (Eq k,Hashable k) => WeakTable k v -> k -> IO ()
delete (WeakTable (tbl_ref :!: _)) k = readIORef tbl_ref >>= \tbl -> HashIO.delete tbl k

{-# INLINE mapM_ #-}
mapM_ :: ((k,v) -> IO ()) -> WeakTable k v -> IO ()
mapM_ f (WeakTable (tbl_ref :!: weak_tbl)) = readIORef tbl_ref >>= \tbl -> HashIO.mapM_ g tbl where
	g (k,weak) = do
		mbv <- Weak.deRefWeak weak
		case mbv of
			Nothing -> return ()
			Just v -> f (k,v)

{-# INLINE mapMGeneric_ #-}
mapMGeneric_ :: (Hashable k,MonadIO m,Eq k) => ((k,v) -> m ()) -> WeakTable k v -> m ()
mapMGeneric_ = mapMGeneric__ liftIO

{-# INLINE mapMGeneric__ #-}
mapMGeneric__ :: (Monad m,Hashable k,Eq k) => (forall a . IO a -> m a) -> ((k,v) -> m ()) -> WeakTable k v -> m ()
mapMGeneric__ liftIO f (WeakTable (tbl_ref :!: weak_tbl)) = liftIO (readIORef tbl_ref) >>= \tbl -> liftIO (HashIO.toList tbl) >>= Prelude.mapM_ g where
	g (k,weak) = do
		mbv <- liftIO $ Weak.deRefWeak weak
		case mbv of
			Nothing -> return ()
			Just v -> f (k,v)
			
foldM :: (Hashable k,MonadIO m,Eq k) => (a -> (k,v) -> m a) -> a -> WeakTable k v -> m a
foldM f z (WeakTable (tbl_ref :!: weak_tbl)) = liftIO (readIORef tbl_ref) >>= \tbl -> liftIO (HashIO.toList tbl) >>= Control.Monad.foldM g z where
	g x (k,weak) = do
		mbv <- liftIO $ Weak.deRefWeak weak
		case mbv of
			Nothing -> return x
			Just v -> f x (k,v)

debugWeakTable :: (Show k,Show v) => WeakTable k v -> IO ()
debugWeakTable (WeakTable (tbl_ref :!: weak_tbl)) = do
	tbl <- readIORef tbl_ref
	let debugEntry (k,w) = do
		mb <- Weak.deRefWeak w
		case mb of
			Nothing -> return ()
			Just v -> putStrLn $ show k ++ " => " ++ show v
	putStrLn "WeakTable..."
	HashIO.mapM_ debugEntry tbl
	putStrLn "...WeakTable"

{-# NOINLINE stableName #-}
stableName :: a -> StableName a
stableName = unsafePerformIO . makeStableName