{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, BangPatterns, TypeOperators, ConstraintKinds, ScopedTypeVariables, FlexibleContexts, GeneralizedNewtypeDeriving, TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

module Control.Monad.Transactional.TxAdapton.Layers where

import Control.Monad.Incremental
import Control.Monad.Transactional
import Control.Monad.Incremental.Adapton
import Control.Monad.Reader (Reader(..),ReaderT(..),MonadReader(..))
import qualified Control.Monad.Reader as Reader
import Data.Time.Clock
import Control.Concurrent.MVar
import Control.Concurrent.Lock as Lock
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map
import Data.Set (Set(..))
import qualified Data.Set as Set
import System.IO.Unsafe
import Data.Foldable as Foldable
import Data.Concurrent.Deque.Class as Queue
import Data.Concurrent.Deque.Reference.DequeInstance
import Data.List as List
import Data.Global.Dynamic as Dyn
import Data.Global.TH as TH

import Control.Monad.Transactional.TxAdapton.Types
import Control.Applicative

import Data.Typeable
import Data.IORef
import qualified Data.HashTable.IO as HashIO
import qualified Data.HashTable.ST.Basic as HashST
import Control.Monad.IO.Class
import Control.Monad.Ref
import Control.Monad.Lazy
import System.Mem.WeakKey
import System.Mem.WeakTable as WeakTable
import Data.Unique
import Control.Monad
import Control.Monad.Trans
import Data.Strict.List as Strict
import Data.Strict.Maybe as Strict
import Data.Strict.Tuple
import qualified Unsafe.Coerce as Unsafe
import Control.Monad.Catch
import Control.Monad.Fix

import Debug

type TxInner = Inside TxAdapton
type TxOuter = Outside TxAdapton

type instance IncK TxAdapton a = (Typeable a,Eq a)

{-# INLINE topTxStack #-}
topTxStack :: TxLayer l r m => l TxAdapton r m (Maybe (TxStackElement r m))
topTxStack = do
	callstack <- readTxStack
	s <- inL $ liftIO $ readIORef callstack
	case s of
		SCons x xs -> return $ Just x
		SNil -> return Nothing

topThunkTxStack :: TxLayer l r m => l TxAdapton r m (Maybe (TxStackElement r m))
topThunkTxStack = do
	callstack <- readTxStack
	s <- inL $ liftIO $ readIORef callstack
	return $ topTxStackThunkElement s

{-# INLINE pushTxStack #-}
pushTxStack :: TxLayer l r m => TxStackElement r m -> l TxAdapton r m ()
pushTxStack = \x -> do
	callstack <- readTxStack
	inL $ liftIO $ modifyIORef' callstack (\xs -> SCons x xs)

{-# INLINE popTxStack #-}
popTxStack :: TxLayer l r m => l TxAdapton r m (TxStackElement r m)
popTxStack = do
	callstack <- readTxStack
	inL $ liftIO $ atomicModifyIORef' callstack (\(SCons x xs) -> (xs,x))

topTxStackThunkElement :: TxCallStack' r m -> Maybe (TxStackElement r m)
topTxStackThunkElement (SCons x xs) = if isThunkTxStackElement x then Just x else topTxStackThunkElement xs
topTxStackThunkElement SNil = Nothing

{-# INLINE isThunkTxStackElement #-}
isThunkTxStackElement :: TxStackElement r m -> Bool
isThunkTxStackElement (_ :!: (SJust _)) = True
isThunkTxStackElement (_ :!: SNothing) = False

-- registers a new transaction-local allocation
newTxMLog :: (Typeable a,Eq a,TxLayer l r m,TxLayer l1 r m) => TxM l1 TxAdapton r m a -> l TxAdapton r m ()
newTxMLog m = do
	txlogs <- readTxLog
	inL $ liftIO $ WeakTable.insertWithMkWeak (txLogBuff $ Strict.head txlogs) (MkWeak $ mkWeakRefKey $ dataTxM m) (idTxNM $ metaTxM m) $! DynTxM Nothing Nothing m New
newTxULog :: (Typeable a,Eq a,TxLayer l r m,TxLayer l1 r m) => TxU l1 TxAdapton r m a -> l TxAdapton r m ()
newTxULog u = do
	txlogs <- readTxLog	
	inL $ liftIO $ WeakTable.insertWithMkWeak (txLogBuff $ Strict.head txlogs) (MkWeak $ mkWeakRefKey $ dataTxU u) (idTxNM $ metaTxU u) $! DynTxU Nothing Nothing u New

-- reads a value from a transactional variable
-- uses @unsafeCoerce@ since we know that the types match
readTxMValue :: (Typeable a,Eq a,TxLayer l r m,TxLayer l1 r m) => TxM l1 TxAdapton r m a -> l TxAdapton r m a
readTxMValue m = do
	tbl <- readTxLog
	dyntxvar <- inL $ bufferTxM m (Read False) tbl
	case dyntxvar of
		DynTxM (Just (BuffTxM (v , _))) _ _ _ -> return $ coerce v
		DynTxM Nothing _ wm _ -> inL $ readRef (dataTxM m)

writeTxMValue :: (Typeable a,Eq a,TxLayer l r m,TxLayer l1 r m) => TxM l1 TxAdapton r m a -> a -> l TxAdapton r m ()
writeTxMValue m v' = do
	tbl <- readTxLog
	inL $ changeTxM m (Just v') tbl
	return ()

readTxUValue :: (Typeable a,Eq a,TxLayer l r m,TxLayer l1 r m) => TxU l1 TxAdapton r m a -> TxStatus -> l TxAdapton r m (TxUData l1 TxAdapton r m a)
readTxUValue u status = do
	tbl <- readTxLog
	dyntxvar <- inL $ bufferTxU u status tbl
	case dyntxvar of
		DynTxU (Just (BuffTxU (buff_dta , _))) _ _ _ -> do
			(dta,_) <- inL $ readRef buff_dta
			return $ coerce dta
		DynTxU Nothing _ wu _ -> inL $ readRef (dataTxU u)

writeTxUValue :: (Typeable a,Eq a,TxLayer l r m,TxLayer l1 r m) => TxU l1 TxAdapton r m a -> BuffTxUData l1 TxAdapton r m a -> TxStatus -> l TxAdapton r m ()
writeTxUValue t dta' status = do
	tbl <- readTxLog
	inL $ changeTxU t (Just $ Prelude.const $! return $! dta') status tbl
	return ()

-- ** Transactions

-- a list of the starting times of running transactions sorted from newest to oldest
-- we may have multiple transactions with the same start time
-- needs to be a @MVar@ because multiple txs may start concurrently on different threads
TH.declareMVar "runningTxs"  [t| [UTCTime] |] [e| [] |]

data DoneTxsID = DoneTxsID deriving (Eq,Typeable)
instance Hashable DoneTxsID where
	hashWithSalt i _ = i

-- a map with commit times of committed transactions and their performed changes
-- each commited TX should have a different commit time. since we get the current time after acquiring the @MVar@, no two parallel commiting txs can have the same time
doneTxs :: (Typeable m,Typeable r) => MVar (Map UTCTime (TxUnmemo r m,TxWrite))
doneTxs = Dyn.declareMVar DoneTxsID Map.empty

-- we need to acquire a lock, but this should be minimal
startTx :: IO UTCTime
startTx = getCurrentTime >>= \t -> addRunningTx t >> return t

deleteRunningTx time = modifyMVarMasked_ runningTxs (return . List.delete time)

-- insert a new time in a list sorted from newest to oldest
addRunningTx time = modifyMVarMasked_ runningTxs (\xs -> return $ List.insertBy (\x y -> compare y x) time xs)

updateRunningTx oldtime newtime = modifyMVarMasked_ runningTxs (\xs -> return $ List.insertBy (\x y -> compare y x) newtime $ List.delete oldtime xs)

--restartMemoTx :: TxLogs r m -> IO ()
--restartMemoTx = Strict.mapM_ restartMemoTx' where
--	restartMemoTx' (TxLog (uid :!: buff :!: memos)) = writeIORef memos []

resetTx :: TxLayer Outside r m => Outside TxAdapton r m a -> Outside TxAdapton r m a
resetTx m = do
	now <- inL $ liftIO startTx >>= newRef
	stack <- inL $ liftIO $ newIORef SNil
	tbl <- inL $ liftIO emptyTxLog
	Reader.local (\_ -> (now :!: stack :!: SCons tbl SNil)) m

-- updating a modifiable may propagate to multiple thunks; they are all logged
-- a mapping from identifiers to a boolean that indicates whether it has been written (True) or evaluated (False)
type TxWrite = Map Unique Bool

concatMaybesM :: Monad m => [Maybe (m a)] -> Maybe (m a)
concatMaybesM [] = Nothing
concatMaybesM (Nothing:xs) = concatMaybesM xs
concatMaybesM (Just chg:xs) = case concatMaybesM xs of
	Nothing -> Just chg
	Just chgs -> Just (chg >> chgs)

wakeUpWaits :: TxLayer Outside r m => TxNodeMeta r m -> m Wakes
wakeUpWaits meta = liftIO $ wakeQueue $ waitTxNM meta
  where
	wakeQueue q = do
		mb <- tryPopR q
		case mb of
			Just lck -> do
				!wakes <- wakeQueue q
				let idm = idTxNM meta
				return $! Map.insert idm lck wakes
			Nothing -> return $! Map.empty



