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
import Data.List as List

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
	newtype Outside TxAdapton r m a = TxOuter { runTxOuter :: ReaderT (TxEnv r m) m a }
		deriving (Functor,Applicative,MonadLazy,Monad,MonadRef r,MonadReader (TxEnv r m),MonadThrow,MonadCatch,MonadMask) 
	newtype Inside TxAdapton r m a = TxInner { runTxInner :: ReaderT (TxEnv r m) m a }
		deriving (Functor,Applicative,MonadLazy,Monad,MonadRef r,MonadReader (TxEnv r m),MonadThrow,MonadCatch,MonadMask)
	
	world = TxOuter . runTxInner
	{-# INLINE world #-}
	unsafeWorld = TxInner . runTxOuter
	{-# INLINE unsafeWorld #-}

	runIncremental m = do
		starttime <- liftIO startTx >>= newRef
		stack <- liftIO $ newIORef SNil
		tbl <- liftIO emptyTxLog
		Reader.runReaderT (runTxOuter m) (starttime :!: stack :!: SCons tbl SNil)
	{-# INLINE runIncremental #-}

instance Monad m => InLayer Outside TxAdapton r m where
	inL m = TxOuter $ lift m
instance Monad m => InLayer Inside TxAdapton r m where
	inL m = TxInner $ lift m

-- registers a new transaction-local allocation
newTxMLog :: (Eq a,TxLayer l r m,TxLayer l1 r m) => TxM l1 TxAdapton r m a -> l TxAdapton r m ()
newTxMLog m = do
	txlogs <- readTxLog
	inL $ liftIO $ WeakTable.insertWithMkWeak (txLogBuff $ Strict.head txlogs) (MkWeak $ mkWeakRefKey $ dataTxM m) (idTxNM $ metaTxM m) (DynTxM Nothing Nothing m New)
newTxULog :: (Eq a,TxLayer l r m,TxLayer l1 r m) => TxU l1 TxAdapton r m a -> l TxAdapton r m ()
newTxULog u = do
	txlogs <- readTxLog	
	inL $ liftIO $ WeakTable.insertWithMkWeak (txLogBuff $ Strict.head txlogs) (MkWeak $ mkWeakRefKey $ dataTxU u) (idTxNM $ metaTxU u) (DynTxU Nothing Nothing u New)

-- reads a value from a transactional variable
-- uses @unsafeCoerce@ since we know that the types match
readTxMValue :: (Eq a,TxLayer l r m,TxLayer l1 r m) => TxM l1 TxAdapton r m a -> l TxAdapton r m a
readTxMValue m@(TxM (r,_)) = do
	tbl <- readTxLog
	dyntxvar <- inL $ bufferTxM m Read tbl
	case dyntxvar of
		DynTxM (Just (BuffTxM (v,_))) _ _ _ -> return $ coerce v
		DynTxM Nothing _ wm _ -> do
			v <- inL $ readRef r
			return $ coerce v

writeTxMValue :: (Eq a,TxLayer l r m,TxLayer l1 r m) => TxM l1 TxAdapton r m a -> a -> l TxAdapton r m ()
writeTxMValue m v' = do
	tbl <- readTxLog
	inL $ changeTxM m (Just v') tbl
	return ()

readTxUValue :: (Eq a,TxLayer l r m,TxLayer l1 r m) => TxU l1 TxAdapton r m a -> TxStatus -> l TxAdapton r m (TxUData l1 TxAdapton r m a)
readTxUValue u@(TxU (r,_)) status = do
	tbl <- readTxLog
	dyntxvar <- inL $ bufferTxU u status tbl
	case dyntxvar of
		DynTxU (Just (BuffTxU (v,_))) _ _ _ -> return $ coerce v
		DynTxU Nothing _ wu _ -> do
			v <- inL $ readRef r
			return $ coerce v

writeTxUValue :: (Eq a,TxLayer l r m,TxLayer l1 r m) => TxU l1 TxAdapton r m a -> TxUData l1 TxAdapton r m a -> TxStatus -> l TxAdapton r m ()
writeTxUValue t dta' status = do
	tbl <- readTxLog
	inL $ changeTxU t (Just $ \dta -> return dta') status tbl
	return ()

-- ** Transactions

-- a list of the starting times of running transactions sorted from newest to oldest
-- we may have multiple transactions with the same start time
-- needs to be a @MVar@ because multiple txs may start concurrently on different threads
{-# NOINLINE runningTxs #-}
runningTxs :: MVar [UTCTime]
runningTxs = unsafePerformIO $ newMVar []

-- a global lock to support atomic validate&commit&retry operations
{-# NOINLINE txLock #-}
txLock :: Lock
txLock = unsafePerformIO $ Lock.new

-- a map with commit times of committed transactions and their performed changes
-- each commited TX should have a different commit time. since we get the current time after acquiring the @MVar@, no two parallel commiting txs can have the same time
{-# NOINLINE doneTxs #-}
doneTxs :: MVar (Map UTCTime (TxUnmemo r m,TxWrite r m))
doneTxs = unsafePerformIO $ newMVar Map.empty

-- we need to acquire a lock, but this should be minimal
startTx :: IO UTCTime
startTx = getCurrentTime >>= \t -> addRunningTx t >> return t

deleteRunningTx time = modifyMVarMasked_ runningTxs (return . List.delete time)

-- insert a new time in a list sorted from newest to oldest
addRunningTx time = modifyMVarMasked_ runningTxs (\xs -> return $ List.insertBy (\x y -> compare y x) time xs)

updateRunningTx oldtime newtime = modifyMVarMasked_ runningTxs (\xs -> return $ List.insertBy (\x y -> compare y x) newtime $ List.delete oldtime xs)

-- restarts a tx with a new starting time
-- note that the time has already been added to the @runningTxs@
restartTx :: TxLayer Outside r m => UTCTime -> Outside TxAdapton r m a -> Outside TxAdapton r m a
restartTx newtime m = do
 	(timeref :!: stack :!: logs) <- Reader.ask
	writeRef timeref newtime
	m

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
type TxWrite r m = (Map Unique Bool,TxRepair' r m)

-- checks if the current txlog is consistent with a sequence of concurrent modifications
-- Nothing = success
-- Just = some conflicting changes happened simultaneously to the current tx
-- if the current tx uses a variable that has been written to before, there is a conflict
-- note that we also consider write-write conflicts, since we don't log written but not read variables differently from read-then-written ones
checkTx :: MonadIO m => TxLog r m -> [(UTCTime,(TxUnmemo r m,TxWrite r m))] -> m (Maybe (TxRepair' r m))
checkTx txlog wrts = liftM concatMaybesM $ Prelude.mapM (checkTx' txlog) wrts where
	checkTx' :: MonadIO m => TxLog r m -> (UTCTime,(TxUnmemo r m,TxWrite r m)) -> m (Maybe (TxRepair' r m))
	checkTx' txlog (txtime,(unmemo,(writtenIDs,repair))) = do
		ok <- Map.foldrWithKey (\uid isWrite mb1 -> checkTxWrite txlog uid isWrite >>= \b2 -> liftM (&& b2) mb1) (return True) writtenIDs
		if ok
			then return Nothing
			else return $ Just (inL (liftIO $ unmemo txlog) >> repair)
	-- True = no conflict
	checkTxWrite txlog uid isWrite = do
		mb <- liftIO $ WeakTable.lookup (txLogBuff txlog) uid
		case mb of
			Nothing -> return True
			Just tvar -> do
				-- if there was a previous @Eval@ or @Write@ on a buffered variable we discard buffered content but keep buffered dependents
				unbufferDynTxVar False txlog tvar
				-- Write-XXX are always conflicts
				-- Eval-Eval and Eval-Read are not conflicts
				return $ (not isWrite) && (dynTxStatus tvar /= Write)

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
				wakes <- wakeQueue q
				return $ Map.insert (idTxNM meta) lck wakes
			Nothing -> return Map.empty



