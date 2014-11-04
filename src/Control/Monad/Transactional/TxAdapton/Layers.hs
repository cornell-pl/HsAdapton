{-# LANGUAGE TypeOperators, ConstraintKinds, ScopedTypeVariables, FlexibleContexts, GeneralizedNewtypeDeriving, TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

module Control.Monad.Transactional.TxAdapton.Layers where

import Control.Monad.Incremental
import Control.Monad.Transactional
import Control.Monad.Incremental.Adapton
import Control.Monad.Reader (Reader(..),ReaderT(..),MonadReader(..))
import qualified Control.Monad.Reader as Reader
import Data.Time.Clock
import Control.Concurrent.MVar
import Control.Concurrent.Lock as Lock
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Set (Set(..))
import qualified Data.Set as Set
import System.IO.Unsafe
import Data.Foldable as Foldable
import Data.Concurrent.Deque.Class as Queue
import Data.Concurrent.Deque.Reference.DequeInstance

import Control.Monad.Transactional.TxAdapton.Types

import Data.Typeable
import Data.IORef
import qualified Data.HashTable.IO as HashIO
import qualified Data.HashTable.ST.Basic as HashST
import Control.Monad.IO.Class
import Control.Monad.Ref
import Control.Monad.Lazy
import System.Mem.WeakRef
import Data.Unique
import Control.Monad
import Control.Monad.Trans
import Data.Strict.List as Strict
import Data.Strict.Maybe as Strict
import Data.Strict.Tuple
import Unsafe.Coerce
import Control.Monad.Catch

type TxInner = Inside TxAdapton
type TxOuter = Outside TxAdapton

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

instance (MonadRef r m,WeakRef r,MonadIO m) => Incremental TxAdapton r m where
	
	-- a monad reader is enough since the callstack and hashtables are mutable
	newtype Outside TxAdapton r m a = TxOuter { runTxOuter :: ReaderT (TxEnv r m) m a } deriving (Monad,MonadIO,MonadRef r,MonadReader (TxEnv r m),MonadThrow,MonadCatch) 
	newtype Inside TxAdapton r m a = TxInner { runTxInner :: ReaderT (TxEnv r m) m a } deriving (Monad,MonadIO,MonadRef r,MonadReader (TxEnv r m),MonadThrow,MonadCatch)
	
	world = TxOuter . runTxInner
	{-# INLINE world #-}

	runIncremental m = do
		starttime <- liftIO $ startTx
		stack <- liftIO $ newIORef SNil
		tbl <- liftIO emptyTxLog
		Reader.runReaderT (runTxOuter m) (starttime,stack,[tbl])
	{-# INLINE runIncremental #-}

instance Monad m => InLayer Outside TxAdapton r m where
	inL m = TxOuter $ lift m
instance Monad m => InLayer Inside TxAdapton r m where
	inL m = TxInner $ lift m

-- registers a new transaction-local allocation
newTxMLog :: (Eq a,TxLayer l r m,TxLayer l1 r m) => TxM l1 TxAdapton r m a -> l TxAdapton r m ()
newTxMLog m = do
	txlogs <- readTxLog
	inL $ liftIO $ HashIO.insert (txLogBuff $ head txlogs) (idTxNM $ metaTxM m) (DynTxM Nothing m New)
newTxULog :: (Eq a,TxLayer l r m,TxLayer l1 r m) => TxU l1 TxAdapton r m a -> l TxAdapton r m ()
newTxULog m = do
	txlogs <- readTxLog
	inL $ liftIO $ HashIO.insert (txLogBuff $ head txlogs) (idTxNM $ metaTxU m) (DynTxU Nothing m New)

-- reads a value from a transactional variable
-- uses @unsafeCoerce@ since we know that the types match
readTxMValue :: (Eq a,TxLayer l r m,TxLayer l1 r m) => TxM l1 TxAdapton r m a -> l TxAdapton r m a
readTxMValue m = do
	tbl <- readTxLog
	dyntxvar <- inL $ bufferTxM m Read tbl
	case dyntxvar of
		DynTxM (Just (BuffTxM (v,_))) _ _ -> return $ unsafeCoerce v
		DynTxM Nothing (TxM (r,_)) _ -> do
			v <- inL $ readRef r
			return $ unsafeCoerce v

writeTxMValue :: (Eq a,TxLayer l r m,TxLayer l1 r m) => TxM l1 TxAdapton r m a -> a -> l TxAdapton r m ()
writeTxMValue m v' = do
	tbl <- readTxLog
	inL $ changeTxM m (Just v') tbl
	return ()

readTxUValue :: (Eq a,TxLayer l r m,TxLayer l1 r m) => TxU l1 TxAdapton r m a -> l TxAdapton r m (TxUData l1 TxAdapton r m a)
readTxUValue u = do
	tbl <- readTxLog
	dyntxvar <- inL $ bufferTxU u Read tbl
	case dyntxvar of
		DynTxM (Just (BuffTxM (v,_))) _ _ -> return $ unsafeCoerce v
		DynTxM Nothing (TxM (r,_)) _ -> do
			v <- inL $ readRef r
			return $ unsafeCoerce v

evalTxUValue :: (Eq a,TxLayer l r m,TxLayer l1 r m) => TxU l1 TxAdapton r m a -> l TxAdapton r m (TxUData l1 TxAdapton r m a)
evalTxUValue u = do
	tbl <- readTxLog
	dyntxvar <- inL $ bufferTxU u Eval tbl
	case dyntxvar of
		DynTxM (Just (BuffTxM (v,_))) _ _ -> return $ unsafeCoerce v
		DynTxM Nothing (TxM (r,_)) _ -> do
			v <- inL $ readRef r
			return $ unsafeCoerce v

writeTxUValue :: (Eq a,TxLayer l r m,TxLayer l1 r m) => TxU l1 TxAdapton r m a -> TxUData l1 TxAdapton r m a -> TxStatus -> l TxAdapton r m ()
writeTxUValue t dta' status = do
	tbl <- readTxLog
	inL $ changeTxU t (Just $ \dta -> return dta') status tbl
	return ()

readTxStack :: TxLayer l r m => l TxAdapton r m (TxCallStack r m)
readTxStack = liftM (\(x,y,z) -> y) $ Reader.ask

readTxLog :: TxLayer l r m => l TxAdapton r m [TxLog r m]
readTxLog = liftM (\(x,y,z) -> z) $ Reader.ask

readTxTime :: TxLayer l r m => l TxAdapton r m (UTCTime)
readTxTime = liftM (\(x,y,z) -> x) $ Reader.ask

-- ** Transactions

-- a list of the starting times of running transactions sorted from newest to oldest
-- needs to be a @MVar@ because multiple txs may start concurrently on different threads
{-# NOINLINE runningTxs #-}
runningTxs :: MVar [UTCTime]
runningTxs = unsafePerformIO $ newMVar []

{-# NOINLINE txLock #-}
txLock :: Lock
txLock = unsafePerformIO $ Lock.new

-- a map with commit times of committed transactions and their performed changes
{-# NOINLINE doneTxs #-}
doneTxs :: MVar (Map UTCTime (TxWrite r m))
doneTxs = unsafePerformIO $ newMVar Map.empty

-- we need to acquire a lock, but this should be minimal
startTx :: IO UTCTime
startTx = modifyMVar runningTxs (\xs -> getCurrentTime >>= \t -> return (t:xs,t))

-- updating a modifiable may propagate to multiple thunks; they are all logged
type TxWrite r m = (Set Unique,Outside TxAdapton r m ())
type TxWrite' r m = Outside TxAdapton r m ()

-- Nothing = success
-- Just = some conflicting changes that happened simultaneously to the current tx
checkTx :: Monad m => TxLog r m -> [TxWrite r m] -> IO (Maybe (TxWrite' r m))
checkTx txlog wrts = liftM concatMaybesM $ Prelude.mapM (checkTx' txlog) wrts

-- checks if two logs of modifications are consistent
checkTx' :: TxLog r m -> TxWrite r m -> IO (Maybe (TxWrite' r m))
checkTx' txlog (writtenIDs,writeOp) = do
	ok <- Foldable.foldlM (\b uid -> liftM (b &&) $ checkTx'' txlog uid) True writtenIDs
	if ok then return Nothing else return $ Just writeOp

-- if the current tx uses a variable that has been written to before, there is a conflict
-- note that we also consider write-write conflicts, since we don't log written but not read variables differently from read and written ones
-- True = no conflict
checkTx'' :: TxLog r m -> Unique -> IO Bool
checkTx'' txlog uid = do
	mb <- HashIO.lookup (txLogBuff txlog) uid
	case mb of
		Nothing -> return True
		-- news should never appear!
		Just (DynTxM buff m New) -> error "new variables should't have been written to!"
		Just (DynTxU buff m New) -> error "new variables should't have been written to!"
		-- reads, evals and writes are conflicting
		Just tvar -> return False

concatMaybesM :: Monad m => [Maybe (m a)] -> Maybe (m a)
concatMaybesM [] = Nothing
concatMaybesM (Nothing:xs) = concatMaybesM xs
concatMaybesM (Just chg:xs) = case concatMaybesM xs of
	Nothing -> Just chg
	Just chgs -> Just (chg >> chgs)


wakeUpWaits :: TxLayer Outside r m => Outside TxAdapton r m () -> TxNodeMeta r m -> m (TxWake m)
wakeUpWaits upd meta = liftIO $ wakeQueue $ waitTxNM meta
  where
	wakeQueue q = do
		mb <- tryPopR q
		case mb of
			Just (txenv,lck) -> do
				(writes,wakes) <- wakeQueue q
				return (Reader.runReaderT (runTxOuter upd) txenv >> writes,tryRelease lck >> wakes)
			Nothing -> return (return (),return ())

