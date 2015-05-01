{-# LANGUAGE TemplateHaskell, ConstraintKinds, UndecidableInstances, Rank2Types, BangPatterns, FunctionalDependencies, MultiParamTypeClasses, MagicHash, ScopedTypeVariables, GADTs, FlexibleContexts, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module Control.Concurrent.Transactional.Internal.TxAdapton.Memo where

import Data.Time.Clock

import Control.Concurrent.MVar.Exts
import Data.Foldable as Foldable
import Data.Monoid as Monoid
import qualified System.Mem.MemoTable as MemoTable
import System.Mem.MemoTable (MemoTable(..))
import Data.Hashable
import Control.Concurrent.Transactional.Internal.TxAdapton.Types
import Control.Concurrent.Transactional.Internal.TxAdapton.Layers
import Control.Monad.Incremental.Internal.Adapton.Memo
import Data.Unique
import Control.Monad.IO.Class
import Debug
import Control.Concurrent
import System.Mem.Weak.Exts
import Control.Monad.Incremental

import Control.Monad.Trans
import System.IO.Unsafe
import Data.Typeable
import Data.WithClass.MData
import Control.Monad
import Data.Strict.List as Strict
import Data.Maybe
import Data.Dynamic
import Control.Concurrent.Map.Exts as CMap
import Data.WithClass.MGenerics.Aliases
import Data.IORef.Exts
import Data.Strict.Tuple
import System.Mem.Weak as Weak
import Control.Monad.Catch as Catch
import Data.Global.Dynamic as Dyn
import Data.Derive.Memo
import System.Mem.StableName.Exts
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map


--- * Generic memoization

memoNonRecTxU :: (Typeable b,Eq b,Memo a,TxLayer Inside c) => Int -> MemoPolicy -> (a -> Inside (TxAdapton c) (TxU c Inside (TxAdapton c) b)) -> a -> Inside (TxAdapton c) (TxU c Inside (TxAdapton c) b)
memoNonRecTxU memosize mode f =
	let !key = stableName f
	    ori_tbl = Dyn.declareWeakBasicHashTable memosize key
	    buff_tbls = Dyn.declareCMap f key
	in  memoNonRecTxU' memosize mode f (ori_tbl :!: buff_tbls)

memoNonRecTxUAs :: (Memo name,Typeable b,Eq b,Memo a,TxLayer Inside c) => Int -> MemoPolicy -> name -> (a -> Inside (TxAdapton c) (TxU c Inside (TxAdapton c) b)) -> a -> Inside (TxAdapton c) (TxU c Inside (TxAdapton c) b)
memoNonRecTxUAs memosize mode name f =
	let !key = memoKey name
	    ori_tbl = Dyn.declareWeakBasicHashTable memosize key
	    buff_tbls = Dyn.declareCMap f key
	in  memoNonRecTxU' memosize mode f (ori_tbl :!: buff_tbls)

memoNonRecTxU' :: (Eq b,Memo a,TxLayer Inside c) => Int -> MemoPolicy -> (a -> Inside (TxAdapton c) (TxU c Inside (TxAdapton c) b)) -> TxMemoTable c (Key a) b -> a -> Inside (TxAdapton c) (TxU c Inside (TxAdapton c) b)
memoNonRecTxU' memosize mode f tbls@(ori_tbl :!: buff_tbls) arg = do
		let (mkWeak,k) = (memoWeak $! arg,memoKey $! arg)
		lkp <- lookupMemoTx tbls k
		case lkp of
			Nothing -> do
				thunk <- f arg
				let thunkWeak = case mode of
					MemoLinear -> MkWeak (mkWeakKey (dataTxU thunk)) `andMkWeak` mkWeak -- while both the source and the target exist
					MemoSuperlinear -> mkWeak -- while the source exists
				insertMemoTx memosize tbls thunkWeak k thunk
				return thunk
			Just thunk -> do
				return thunk

-- looks up a value in a transactional memotable by searching for it in the current and enclosing buffered memotables
lookupMemoTx :: (Eq k,Hashable k,TxLayer Inside c) => TxMemoTable c k b -> k -> Inside (TxAdapton c) (Maybe (TxU c Inside (TxAdapton c) b))
lookupMemoTx tbls@(ori_tbl :!: buff_tbls) k = do
	!txlogs <- readTxLogs
	unsafeIOToInc $ do
		!thread <- myThreadId			
		!mb_buffs <- CMap.lookup thread buff_tbls
		!buffs <- maybe (return Map.empty) readIORef' mb_buffs
		lookupMemoTx' ori_tbl buffs k txlogs
  where
	lookupMemoTx' ori_tbl buffs k SNil = MemoTable.lookup ori_tbl k
	lookupMemoTx' ori_tbl buffs k (SCons txlog txlogs) = do
		case Map.lookup (txLogId txlog) buffs of
			Just buff_tbl -> do
				mb <- lookupMemoTx'' buff_tbl k
				case mb of
					Just thunk -> return mb
					Nothing -> lookupMemoTx' ori_tbl buffs k txlogs
			Nothing -> lookupMemoTx' ori_tbl buffs k txlogs
	lookupMemoTx'' [] k = return Nothing
	lookupMemoTx'' (block:blocks) k = do
		mb <- MemoTable.lookup block k
		case mb of
			Just (_ :!: thunk) -> return $! Just thunk
			Nothing -> lookupMemoTx'' blocks k

insertMemoTx :: (Eq k,Hashable k,TxLayer Inside c) => Int -> TxMemoTable c k b -> MkWeak -> k -> TxU c Inside (TxAdapton c) b -> Inside (TxAdapton c) ()
insertMemoTx memosize tbls@(ori_tbl :!: buff_tbls) thunkWeak k thunk = doBlockTx $ do
	txlogs@(SCons txlog _) <- readTxLogs
	-- add the thunk to the buffered memotable
	(_,rootThread) <- readRootTx
	unsafeIOToInc $ do
		!thread <- myThreadId
		-- find an entry for this thread or create a new one
		buffsr <- CMap.lookupOrInsert thread (return . Just) (newIORef' Map.empty >>= \x -> return (x,x)) (\_ -> return ()) buff_tbls
		buffs <- readIORef' buffsr
		buff_tbl <- case Map.lookup (txLogId txlog) buffs of
			-- if there is already a buffered entry for the log, then the table is also already in the log's record
			Just (buff_tbl:_) -> return buff_tbl
			Nothing -> do
				-- the buffered memo table lives as long as the original memo table lives ( it does not depend on the txlog itself because it may be reuased across txlogs)
				let !memoMkWeak = MkWeak $ mkWeakKey ori_tbl
				buff_tbl <- MemoTable.newSizedWithMkWeak memosize memoMkWeak
				writeIORef' buffsr $ Map.insert (txLogId txlog) [buff_tbl] buffs
				-- add the buffered memotable to the txlog's list
				modifyIORef' (txLogMemo txlog) (DynTxMemoTable tbls :)
				return buff_tbl
		weak <- unMkWeak thunkWeak (thunkWeak :!: thunk) Nothing
		MemoTable.insertWeak buff_tbl k weak
		modifyMVarMasked_' "insertMemoTx" (unmemoTxNM $ metaTxU thunk) (\(ori :!: buff) -> return (ori :!: Map.insertWith (>>) rootThread (Weak.finalize weak) buff))

-- commits the buffered memotable entries for a given txlog to the persistent memotable
-- returns a function that can be used to remove the newly commited entries from other concurrent buffered memo tables
{-# INLINE commitTxLogMemoTables #-}
commitTxLogMemoTables :: Bool -> ThreadId -> TxLog c -> IO (TxUnmemo c)
commitTxLogMemoTables doCommit thread txlog = {-# SCC commitTxLogMemoTables #-} do
	txmemos <- readIORef (txLogMemo txlog)
	let add unmemos (DynTxMemoTable txmemo) = do
		!unmemo <- commitTxMemoTable doCommit thread txmemo txlog
		return $! mappend unmemos unmemo 
	Control.Monad.foldM add mempty txmemos

commitTxMemoTable :: (Eq k,Hashable k) => Bool -> ThreadId -> TxMemoTable c k b -> TxLog c -> IO (TxUnmemo c)
commitTxMemoTable doCommit thread (ori_tbl :!: buff_tbls) txlog = do
	mb <- CMap.lookup thread buff_tbls
	case mb of
		Nothing -> return mempty
		Just buffsr -> do
			buffs <- readIORef' buffsr
			unmemos <- case Map.lookup (txLogId txlog) buffs of
				Nothing -> return mempty
				Just buff_tbl' -> do
					buff_tbl <- flattenMemoTables buff_tbl'
					-- commit the buffered entries over the original memotable
					let addEntry xs (k,(mkWeak :!: u)) = do
						weak <- unMkWeak mkWeak u Nothing
						MemoTable.insertWeak ori_tbl k weak
						modifyMVarMasked_' "commitTxMemoTable" (unmemoTxNM $ metaTxU u) (\(ori :!: buff) -> return ((ori >> Weak.finalize weak) :!: buff))
						return (SCons k xs)
					!entries <- if doCommit
						then MemoTable.foldM addEntry SNil buff_tbl -- strict list
						else return SNil
					-- finalize the thread-local memotable
					MemoTable.finalize buff_tbl
					-- function that unmemoizes buffered memo entries from another concurrent thread
					let unmemo other_thread = do
						mb <- CMap.lookup other_thread buff_tbls
						case mb of
							Nothing -> return ()
							Just other_buffsr -> do
								other_buffs <- readIORef' other_buffsr
								let unmemolog other_log other_tbl' rs = Foldable.mapM_ (\other_tbl -> Foldable.mapM_ (MemoTable.delete other_tbl) entries) other_tbl' >> rs
								Map.foldrWithKey unmemolog (return ()) other_buffs
					let unmemos = TxRepair $ unsafeIOToInc myThreadId >>= unsafeIOToInc . unmemo
					return unmemos
			-- remove all buffered memotables for the current thread
			CMap.delete thread buff_tbls
			return unmemos

-- merges the memo entries for a nested txlog into the memotables of its parent txlog, by overriding parent entries
mergeTxLogMemos :: ThreadId -> TxLog c -> TxLogs c -> IO ()
mergeTxLogMemos thread txlog_child txlogs_parent@(SCons txlog_parent _) = do
	
	txmemos <- readIORef' (txLogMemo txlog_child)
	let mergeMemo tbls@(DynTxMemoTable (_ :!: buff_tbls)) = do
		mb <- CMap.lookup thread buff_tbls
		case mb of
			-- no buffered memotables for the thread
			Nothing -> return ()
			Just buffsr -> do
				buffs <- readIORef' buffsr
				case Map.lookup (txLogId txlog_child) buffs of
					-- no buffered memotable for the child log
					Nothing -> return ()
					Just child_tbls -> do
						-- merge child entries
						writeIORef' buffsr $ Map.insertWith (++) (txLogId txlog_parent) child_tbls buffs
	Control.Monad.mapM_ mergeMemo txmemos

mergeFutureTxLogMemos :: ThreadId -> ThreadId -> TxLog c -> TxLogs c -> IO ()
mergeFutureTxLogMemos child_thread parent_thread txlog_child txlogs_parent@(SCons txlog_parent _) = do
	
	child_txmemos <- readIORef' (txLogMemo txlog_child)
	let mergeMemo tbls@(DynTxMemoTable (_ :!: buff_tbls)) = do
		mb <- CMap.lookup child_thread buff_tbls
		case mb of
			-- no buffered memotables for the thread
			Nothing -> return ()
			Just child_buffsr -> do
				child_buffs <- readIORef' child_buffsr
				case Map.lookup (txLogId txlog_child) child_buffs of
					-- no buffered memotable for the child log
					Nothing -> return ()
					Just child_tbls -> do
						parent_buffsr <- CMap.lookupOrInsert parent_thread (return . Just) (newIORef' Map.empty >>= \x -> return (x,x)) (\_ -> return ()) buff_tbls
						parent_buffs <- readIORef' parent_buffsr
						-- merge child entries
						writeIORef' parent_buffsr $ Map.insertWith (++) (txLogId txlog_parent) child_tbls parent_buffs
				CMap.delete child_thread buff_tbls
	Control.Monad.mapM_ mergeMemo child_txmemos

unbufferTxLogMemos :: ThreadId -> TxLogs c -> IO ()
unbufferTxLogMemos thread SNil = return ()
unbufferTxLogMemos thread (SCons txlog txlogs) = do
	readIORef (txLogMemo txlog) >>= Control.Monad.mapM_ (unbufferDynTxMemoTable thread)
	unbufferTxLogMemos thread txlogs

unbufferDynTxMemoTable :: ThreadId -> DynTxMemoTable c -> IO ()
unbufferDynTxMemoTable thread (DynTxMemoTable (ori_tbl :!: buff_tbls)) = CMap.delete thread buff_tbls

instance (Typeable i,Typeable c,Typeable l,Typeable inc,Typeable a) => Memo (TxM i c l inc a) where
	type Key (TxM i c l inc a) = Unique
	{-# INLINE memoKey #-}
	memoKey = idTxNM . metaTxM
	{-# INLINE memoWeak #-}
	memoWeak = \t -> MkWeak $ mkWeakKey $! dataTxM t
                                 
instance (Typeable c,Typeable l,Typeable inc,Typeable a) => Memo (TxU c l inc a) where
	type Key (TxU c l inc a) = Unique
	{-# INLINE memoKey #-}
	memoKey = idTxNM . metaTxU
	{-# INLINE memoWeak #-}
	memoWeak = \t -> MkWeak $ mkWeakKey $! dataTxU t

--instance WeakKey r => Memo (TxL l inc a) where
--	type Key (L l inc a) = Unique
--	{-# INLINE memoKey #-}
--	memoKey t = (MkWeak $ WeakKey.mkWeakKey (dataL t),idNM $ metaL t)

instance Hashable (TxU c l inc a) where
	hashWithSalt i u = hashWithSalt i (idTxNM $ metaTxU u)
instance Hashable (TxM i c l inc a) where
	hashWithSalt i m = hashWithSalt i (idTxNM $ metaTxM m)
--instance Hashable (TxL l inc a) where
--	hashWithSalt i l = hashWithSalt i (idNM $ metaL l)
