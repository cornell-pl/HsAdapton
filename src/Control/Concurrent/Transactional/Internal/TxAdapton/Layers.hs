{-# LANGUAGE ViewPatterns, UndecidableInstances, DataKinds, DeriveDataTypeable, TemplateHaskell, BangPatterns, TypeOperators, ConstraintKinds, ScopedTypeVariables, FlexibleContexts, GeneralizedNewtypeDeriving, TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

module Control.Concurrent.Transactional.Internal.TxAdapton.Layers (
	  module Control.Monad.Incremental
	, topTxStack, topThunkTxStack, pushTxStack, popTxStack, topTxStackThunkElement, isThunkTxStackElement
	, newTxMLog, newTxULog
	, readTxMValue, writeTxMValue, readTxUValue, writeTxUValue
	, runningTxs, doneTxs, deleteRunningTx, addRunningTx, updateRunningTx, wakeUpWaits, liveTxs, addLiveTx, deleteLiveTx, findLiveTx
	, IncParams(..)
	, Outside(..)
	, Inside(..)
	, TxAdaptonE, TxAdaptonC, TxME, TxMC, TxUE, TxUC, proxyTxAdaptonE, proxyTxAdaptonC
	, mergeFutureTxLog
	) where

import qualified Control.Concurrent.Map as CMap
import Control.Concurrent hiding (readMVar)
import Control.Concurrent.MVar.Exts
import System.Mem.MemoTable (MemoTable(..))
import qualified System.Mem.MemoTable as MemoTable
import Control.DeepSeq as Seq
import {-# SOURCE #-} Control.Concurrent.Transactional.Internal.TxAdapton.Algorithm
import Control.Monad.Incremental
import Control.Concurrent.Transactional
import Control.Monad.Incremental.Adapton
import Control.Monad.Reader (Reader(..),ReaderT(..),MonadReader(..))
import qualified Control.Monad.Reader as Reader
import Data.Time.Clock
import qualified System.Mem.WeakMap as WeakMap
import System.Mem.Weak.Exts as Weak
import Control.Concurrent.Lock.Exts as Lock
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map
import Data.Set (Set(..))
import qualified Data.Set as Set
import System.IO.Unsafe
import Data.Foldable as Foldable
import Data.Concurrent.Deque.Class as Queue
import Data.Concurrent.Deque.Reference.DequeInstance
import Data.List as List
import Data.Global.TH as TH
import Data.Monoid

import Control.Concurrent.Transactional.Internal.TxAdapton.Types
import Control.Applicative

import Data.Typeable
import Data.IORef.Exts
import Control.Monad.IO.Class


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

{-# INLINE topTxStack #-}
topTxStack :: TxLayer l c => l (TxAdapton c) (Maybe (TxStackElement c))
topTxStack = do
	callstack <- readTxStack
	s <- unsafeIOToInc $ readIORef callstack
	case s of
		SCons x xs -> return $! Just x
		SNil -> return Nothing

{-# INLINE topThunkTxStack #-}
topThunkTxStack :: TxLayer l c => l (TxAdapton c) (Maybe (TxStackElement c))
topThunkTxStack = do
	callstack <- readTxStack
	s <- unsafeIOToInc $ readIORef callstack
	return $ topTxStackThunkElement s

{-# INLINE pushTxStack #-}
pushTxStack :: TxLayer l c => TxStackElement c -> l (TxAdapton c) ()
pushTxStack = \x -> do
	callstack <- readTxStack
	unsafeIOToInc $ atomicModifyIORef' callstack (\xs -> (SCons x xs,()))

{-# INLINE popTxStack #-}
popTxStack :: TxLayer l c => l (TxAdapton c) (TxStackElement c)
popTxStack = do
	callstack <- readTxStack
	unsafeIOToInc $ atomicModifyIORef' callstack (\(SCons x xs) -> (xs,x))

{-# INLINE topTxStackThunkElement #-}
topTxStackThunkElement :: TxCallStack' c -> Maybe (TxStackElement c)
topTxStackThunkElement (SCons x xs) = if isThunkTxStackElement x then Just x else topTxStackThunkElement xs
topTxStackThunkElement SNil = Nothing

{-# INLINE isThunkTxStackElement #-}
isThunkTxStackElement :: TxStackElement c -> Bool
isThunkTxStackElement (_ :!: (SJust _) :!: _) = True
isThunkTxStackElement (_ :!: SNothing :!: _) = False

-- registers a new transaction-local allocation
{-# INLINE newTxMLog #-}
newTxMLog :: (IsolationKind i,IncK (TxAdapton c) a,TxLayer l c,TxLayer l1 c,TxLayer Outside c) => TxM i c l1 (TxAdapton c) a -> l (TxAdapton c) ()
newTxMLog !m = do
	!txlogs <- readTxLogs
	!txid <- readTxIdRef
	unsafeIOToInc $! do
		!thread <- myThreadId
		buff_dta <- blankBuffTxM
		txdeps <- WeakMap.new'
		txstat <- newIORef' $ TxStatus (New False :!: False)
		addTxLogEntryUp thread txlogs (idTxNM $! metaTxM m) $! DynTxM buff_dta txdeps m txstat
{-# INLINE newTxULog #-}
newTxULog :: (IncK (TxAdapton c) a,TxLayer l c,TxLayer l1 c,TxLayer Outside c) => TxU c l1 (TxAdapton c) a -> l (TxAdapton c) ()
newTxULog !u = do
	!txlogs <- readTxLogs	
	!txid <- readTxIdRef
	unsafeIOToInc $! do
		!thread <- myThreadId
		buff_dta <- blankBuffTxU
		txdeps <- WeakMap.new'
		txstat <- newIORef' $ TxStatus (New False :!: False)
		addTxLogEntryUp thread txlogs (idTxNM $! metaTxU u) $! DynTxU buff_dta txdeps u txstat

-- reads a value from a transactional variable
-- uses @unsafeCoerce@ since we know that the types match
{-# INLINE readTxMValue #-}
readTxMValue :: (IsolationKind i,IncK (TxAdapton c) a,TxLayer l c,TxLayer l1 c,TxLayer Outside c) => TxM i c l1 (TxAdapton c) a -> l (TxAdapton c) (a,TxStatus)
readTxMValue m = {-# SCC readTxMValue #-} do
	!tbl <- readTxLogs
	!txid <- readTxIdRef
	(isFuture,rootThread) <- readRootTx
	unsafeIOToInc $ do
		!thread <- myThreadId
		tvar <- bufferTxM isFuture rootThread thread m (TxStatus (Read Nothing :!: False)) tbl
		dynTxMValue tvar

{-# INLINE writeTxMValue #-}
writeTxMValue :: (IsolationKind i,IncK (TxAdapton c) a,TxLayer l c,TxLayer l1 c,TxLayer Outside c) => TxM i c l1 (TxAdapton c) a -> a -> l (TxAdapton c) TxStatus
writeTxMValue m v' = do
	!tbl <- readTxLogs
	!txid <- readTxIdRef
	(isFuture,rootThread) <- readRootTx
	unsafeIOToInc $ do
		thread <- myThreadId
		!tvar <- changeTxM isFuture rootThread thread m (Just v') (TxStatus (Write Nothing :!: False)) tbl
		readIORef (dynTxStatus tvar)

{-# INLINE readTxUValue #-}
readTxUValue :: (IncK (TxAdapton c) a,TxLayer l c,TxLayer l1 c,TxLayer Outside c) => TxU c l1 (TxAdapton c) a -> TxStatus -> l (TxAdapton c) (TxUData c l1 (TxAdapton c) a,TxStatus)
readTxUValue u status = do
	!tbl <- readTxLogs
	!txid <- readTxIdRef
	(isFuture,rootThread) <- readRootTx
	unsafeIOToInc $ do
		!thread <- myThreadId
		DynTxU (BuffTxU buff_dta) txdeps u txstat <- bufferTxU isFuture rootThread thread u status tbl
		!stat <- readIORef txstat
		case stat of
			(isEvalOrWrite -> Just _) -> do
				!(dta,_) <- readIORef $ coerceIORefBuffTxUData buff_dta
				return $! (dta,stat)
			otherwise -> do
				!v <- readIORef $ coerceIORefTxUData $ dataTxU u
				return $! (v,stat)

{-# INLINE writeTxUValue #-}
writeTxUValue :: (IncK (TxAdapton c) a,TxLayer l c,TxLayer l1 c,TxLayer Outside c) => TxU c l1 (TxAdapton c) a -> TxUData c l1 (TxAdapton c) a -> TxStatus -> l (TxAdapton c) TxStatus
writeTxUValue t dta' status = do
	!tbl <- readTxLogs
	!txid <- readTxIdRef
	(isFuture,rootThread) <- readRootTx
	unsafeIOToInc $ do
		let chg (_,ori) = do
			ori' <- case ori of
				Left deps -> liftM Right $ mkWeakRefKey deps deps Nothing
				Right wdeps -> return $ Right wdeps
			return (dta',ori')
		!thread <- myThreadId
		tvar <- changeTxU isFuture rootThread thread t (Just chg) status tbl
		readIORef (dynTxStatus tvar)

-- ** Transactions

-- ** for commit-time conflicts
-- a list of the starting times of running transactions sorted from newest to oldest
-- we may have multiple transactions with the same start time
-- needs to be a @MVar@ because multiple txs may start concurrently on different threads
TH.declareMVar "runningTxs" [t| [UTCTime] |] [e| [] |]

{-# INLINE deleteRunningTx #-}
deleteRunningTx time = modifyMVarMasked_' "deleteRunningTx" runningTxs (return . List.delete time)
-- insert a new time in a list sorted from newest to oldest
{-# INLINE addRunningTx #-}
addRunningTx time = modifyMVarMasked_' "addRunningTx" runningTxs (\xs -> return $ List.insertBy (\x y -> compare y x) time xs)
{-# INLINE updateRunningTx #-}
updateRunningTx oldtime newtime = modifyMVarMasked_' "updateRunningTx" runningTxs (\xs -> return $ List.insertBy (\x y -> compare y x) newtime $ List.delete oldtime xs)

-- ** for early conflicts
-- txs log their running periods to ensure that they wait for concurrent exceptions
TH.declareMVar "liveTxs" [t| Map ThreadId Lock |] [e| Map.empty |]

{-# INLINE addLiveTx #-}
addLiveTx tid = do
	lck <- Lock.new
	modifyMVarMasked_' "addLiveTx" liveTxs (return . Map.insert tid lck)

{-# INLINE findLiveTx #-}
findLiveTx tid = liftM (Map.lookup tid) (readMVar "findLiveTx" liveTxs)

-- if the current thread's id is not found in the list, then it is because another thread is waiting to send it an InvalidTx exception)
{-# INLINE deleteLiveTx #-}
deleteLiveTx tid = do
	Just lck <- findLiveTx tid
	Lock.wait "deleteLiveTx" lck
	modifyMVarMasked_' "deleteLiveTx" liveTxs (return . Map.delete tid)

-- a map with commit times of committed transactions and their performed changes
-- each commited TX should have a different commit time. since we get the current time after acquiring the @MVar@, no two parallel commiting txs can have the same time
TH.declareMVar "doneTxs"  [t| Map UTCTime (TxUnmemo CommitConflict,RepairDynTxVar CommitConflict) |] [e| Map.empty |]

{-# INLINE wakeUpWaits #-}
wakeUpWaits :: TxLayer Outside c => TxNodeMeta c -> IO Wakes
wakeUpWaits meta = wakeQueue $ waitTxNM meta
  where
	wakeQueue q = do
		mb <- tryPopR q
		case mb of
			Just lck -> do
				!wakes <- wakeQueue q
				let idm = idTxNM meta
				return $! Map.insert idm lck wakes
			Nothing -> return $! Map.empty


instance (TxLayer Inside c,TxLayer Outside c) => Incremental (TxAdapton c) where
	
	-- given the cost of rehasing for the hashtables we use, the initial memo table size can greatly affect performance
	data IncParams (TxAdapton c) = TxAdaptonParams { txAdaptonMemoSize :: Int, txAdaptonMemoPolicy :: MemoPolicy , txAdaptonRepair :: Bool }
	defaultIncParams = TxAdaptonParams { txAdaptonMemoSize = 10^(3::Int), txAdaptonMemoPolicy = MemoLinear, txAdaptonRepair = True }
	
	-- a monad reader is enough since the callstack and hashtables are mutable
	newtype Outside (TxAdapton c) a = TxOuter { runTxOuter :: ReaderT (TxEnv c) IO a }
		deriving (Functor,Applicative,Monad) 
	newtype Inside (TxAdapton c) a = TxInner { runTxInner :: ReaderT (TxEnv c) IO a }
		deriving (Functor,Applicative,Monad)
	
	displayK x = return $! Seq.force $ show x
	
	world = TxOuter . runTxInner
	{-# INLINE world #-}
	unsafeWorld = TxInner . runTxOuter
	{-# INLINE unsafeWorld #-}

	runIncremental = atomicallyTx defaultIncParams ""
	runIncrementalWithParams params = atomicallyTx params ""
	{-# INLINE runIncremental #-}
	{-# INLINE runIncrementalWithParams #-}
	
	unsafeIOToInc = inside . TxInner . lift
	{-# INLINE unsafeIOToInc #-}

instance TxLayerImpl Inside where
	unTxLayer _ = runTxInner
	txLayer _ = TxInner
	{-# INLINE unTxLayer #-}
	{-# INLINE txLayer #-}
instance TxLayerImpl Outside where
	unTxLayer _ = runTxOuter
	txLayer _ = TxOuter
	{-# INLINE unTxLayer #-}
	{-# INLINE txLayer #-}

{-# SPECIALIZE txLayer :: Proxy Inside -> ReaderT TxEnvE IO a -> Inside TxAdaptonE a #-}
{-# SPECIALIZE txLayer :: Proxy Outside -> ReaderT TxEnvE IO a -> Outside TxAdaptonE a #-}
{-# SPECIALIZE txLayer :: Proxy Inside -> ReaderT TxEnvC IO a -> Inside TxAdaptonC a #-}
{-# SPECIALIZE txLayer :: Proxy Outside -> ReaderT TxEnvC IO a -> Outside TxAdaptonC a #-}
{-# SPECIALIZE unTxLayer :: Proxy Inside -> Inside TxAdaptonE a -> ReaderT TxEnvE IO a #-}
{-# SPECIALIZE unTxLayer :: Proxy Outside -> Outside TxAdaptonE a -> ReaderT TxEnvE IO a #-}
{-# SPECIALIZE unTxLayer :: Proxy Inside -> Inside TxAdaptonC a -> ReaderT TxEnvC IO a #-}
{-# SPECIALIZE unTxLayer :: Proxy Outside -> Outside TxAdaptonC a -> ReaderT TxEnvC IO a #-}

instance MonadReader TxEnvE (Outside (TxAdapton EarlyConflict)) where
	ask = TxOuter $ ask
	local f (TxOuter m) = TxOuter $ local f m
	{-# INLINE ask #-}
	{-# INLINE local #-}
instance MonadReader TxEnvC (Outside (TxAdapton CommitConflict)) where
	ask = TxOuter $ ask
	local f (TxOuter m) = TxOuter $ local f m
	{-# INLINE ask #-}
	{-# INLINE local #-}
instance MonadReader TxEnvE (Inside (TxAdapton EarlyConflict)) where
	ask = TxInner $ ask
	local f (TxInner m) = TxInner $ local f m
	{-# INLINE ask #-}
	{-# INLINE local #-}
instance MonadReader TxEnvC (Inside (TxAdapton CommitConflict)) where
	ask = TxInner $ ask
	local f (TxInner m) = TxInner $ local f m
	{-# INLINE ask #-}
	{-# INLINE local #-}

type TxAdaptonE = TxAdapton EarlyConflict
type TxAdaptonC = TxAdapton CommitConflict

type TxME i = TxM i EarlyConflict
type TxMC i = TxM i CommitConflict

type TxUE = TxU EarlyConflict
type TxUC = TxU CommitConflict

proxyTxAdaptonE :: Proxy (TxAdapton EarlyConflict)
proxyTxAdaptonE = Proxy
{-# INLINE proxyTxAdaptonE #-}

proxyTxAdaptonC :: Proxy (TxAdapton CommitConflict)
proxyTxAdaptonC = Proxy
{-# INLINE proxyTxAdaptonC #-}



mergeFutureTxLog :: TxLayer Outside c => Bool -> Bool -> ThreadId -> ThreadId -> TxLogs c -> TxEnv c -> IO ()
mergeFutureTxLog doWrites !parent_isFuture parent_thread child_thread parent_txlogs@(SCons parent_txlog _) child_env@(txEnvLogs -> child_txlogs@(SCons child_txlog1 child_txlogs2@(SCons child_txlog2 _))) = do
	-- add a new block to the parent, to prevent existing parent snapshots
	addBlockTxLogs (txAdaptonMemoSize $ txEnvParams child_env) parent_txlogs
	child_block <- flattenTxLogBlocks child_txlog1
	
	-- compute the parent differences since the fork
	diff_parent_txlogs <- liftM (flip SCons SNil) $ diffTxLog parent_txlog child_txlog2
	
	let updEntry acc@(dirties,commits) (uid,child_tvar) = do
		case child_tvar of
			DynTxM (BuffTxM buff_dta) _ (m :: TxM i c l (TxAdapton c) a) child_stat -> do
				let i = Proxy :: Proxy i
				c_stat <- readIORef' child_stat
				child <- liftM Prelude.fst $ dynTxMValue child_tvar
				
				mb_parent <- findTxContentEntry parent_isFuture parent_thread diff_parent_txlogs (metaTxM m)
				case mb_parent of
					Nothing -> do
						let dirty = unless doWrites $ unbufferDynTxVar'' True child_thread child_txlogs True child_tvar
						return (dirties >> dirty,commits)
					Just (parent_tvar,_) -> do
						p_stat <- readIORef' $ dynTxStatus parent_tvar
						case (p_stat,doWrites,c_stat) of
							(isWriteOrNewTrue -> True,True,isWriteOrNewTrue -> True) -> case toIsolation i of
								Versioned -> do -- write child, dirty parent
									mergeEntries True False False acc parent_txlogs child_txlogs parent_tvar child_tvar
								Forgetful -> do -- keep parent, dirty child
									mergeEntries False True True acc parent_txlogs child_txlogs parent_tvar child_tvar
								Cumulative -> do -- resolve conflict, dirty both
									new_value <- do
										original <- findTxMLogValue child_txlogs2 m
										(parent :: a) <- liftM (coerce . Prelude.fst) $ dynTxMValue parent_tvar
										let resolve :: a -> a -> a -> Inside (TxAdapton c) a = coerce $ resolveTxM m
										Reader.runReaderT (runTxInner $ resolve original parent child) child_env
									(dirties',commits') <- mergeEntries True True False acc parent_txlogs child_txlogs parent_tvar child_tvar
									let commit = do
										case c_stat of
											TxStatus (Write _ :!: _) -> writeIORef' buff_dta new_value
											TxStatus (New _ :!: _) -> writeIORef' (dataTxM m) new_value
									return (dirties',commits' >> commit)
							(isWriteOrNewTrue -> True,_,_) -> do -- keep parent entry, dirty child entry
								mergeEntries False True True acc parent_txlogs child_txlogs parent_tvar child_tvar
							(_,_,isWriteOrNewTrue -> True) -> do -- write child entry, dirty parent entry
								mergeEntries True False False acc parent_txlogs child_txlogs parent_tvar child_tvar
							otherwise -> do -- write child entry
								mergeEntries False False False acc parent_txlogs child_txlogs parent_tvar child_tvar
			otherwise -> do
				mb_parent <- findTxContentEntry parent_isFuture parent_thread diff_parent_txlogs (dynTxMeta child_tvar)
				case mb_parent of
					Nothing -> do
						let dirties' = if doWrites then dirties else
							dirties >> unbufferDynTxVar'' True child_thread child_txlogs True child_tvar
						return (dirties',commits)
					Just (parent_tvar,_) -> do
						let dirties' = if doWrites then dirties else
							dirties >> unbufferDynTxVar'' True child_thread child_txlogs True child_tvar
						let commit = mergeDynTxVar False child_tvar parent_tvar
						return (dirties',commits >> commit)	
	
	-- identify conflicts and compute resolve functions
	-- does not yet change the parent nor the child state, so that that resolve functions run over an unmodified child state
	(dirty,commit) <- MemoTable.foldM updEntry mempty child_block
	-- dirty buffered parent and child entries and commit changes
	dirty >> commit
	
	let addEntry (uid,wentry) = do
		mb <- Weak.deRefWeak wentry
		case mb of
			Nothing -> return ()
			Just entry -> do
				addTxLogEntryDown parent_thread parent_txlogs uid wentry entry
				CMap.delete child_thread (contentTxNM $ dynTxMeta entry)
	
	MemoTable.mapWeakM_ addEntry child_block

  where
	mergeEntries :: TxLayer Outside c => Bool -> Bool -> Bool -> (IO (),IO ()) -> TxLogs c -> TxLogs c -> DynTxVar c -> DynTxVar c -> IO (IO (),IO ())
	mergeEntries dirtyParent dirtyChild keepParent (dirties,commits) parent_txlogs child_txlogs parent_tvar child_tvar = do
		let dirty = do
			when dirtyParent $ dirtyBufferedDynTxVar parent_isFuture parent_thread parent_txlogs parent_tvar
			if doWrites
				then when dirtyChild $ dirtyBufferedDynTxVar True child_thread child_txlogs child_tvar
				else unbufferDynTxVar'' True child_thread child_txlogs True child_tvar
		let commit = mergeDynTxVar keepParent child_tvar parent_tvar
		return (dirties >> dirty,commits >> commit)





