{-# LANGUAGE ConstraintKinds, UndecidableInstances, Rank2Types, BangPatterns, FunctionalDependencies, MultiParamTypeClasses, MagicHash, ScopedTypeVariables, GADTs, FlexibleContexts, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module Control.Monad.Transactional.TxAdapton.Memo (
	memoNonRecTxU, memoNonRecTxUAs, Memo(..), Hashable(..)
	) where

--	, GenericQMemoU(..),MemoCtx(..),Sat(..),gmemoNonRecU,proxyMemoCtx,NewGenericQ(..),NewGenericQMemo(..),NewGenericQMemoU(..)

import System.Mem.WeakKey as WeakKey
import System.Mem.WeakTable as WeakTable
import Data.Time.Clock
import System.Mem.MemoTable
import Data.Hashable
import Control.Monad.Transactional.TxAdapton.Types
import Control.Monad.Transactional.TxAdapton.Layers
import Control.Monad.Incremental.Adapton.Memo
import Data.Unique
import Control.Monad.IO.Class
import Debug
import Control.Concurrent
import System.Mem.Weak
import qualified System.Mem.WeakTable as WeakTable
import Control.Monad.Incremental
import Control.Monad.Ref
import Control.Monad.Trans
import System.IO.Unsafe
import Data.Typeable
import Data.WithClass.MData
import Control.Monad
import Data.Strict.List as Strict
import Data.Maybe
import Data.Dynamic
import Control.Concurrent.Map as CMap
import Data.WithClass.MGenerics.Aliases
import Data.IORef
import Data.Strict.Tuple
import Data.Global.Dynamic
import System.Mem.Weak as Weak
import Control.Monad.Catch as Catch

import qualified Data.HashTable.IO as HashIO
import qualified Data.HashTable.ST.Basic as HashST

--- * Generic memoization

memoNonRecTxU :: (Typeable b,Eq b,MonadRef r m,MonadIO m,Memo a,TxLayer Inside r m) => MemoMode -> (a -> Inside TxAdapton r m (TxU Inside TxAdapton r m b)) -> a -> Inside TxAdapton r m (TxU Inside TxAdapton r m b)
memoNonRecTxU mode f =
	let buff_tbls = declareCMap f (stableName f)
	    ori_tbl = declareWeakTable f (stableName f)
	in memoNonRecTxU' mode f $! (ori_tbl :!: buff_tbls)

memoNonRecTxUAs :: (Memo name,Typeable b,Eq b,MonadRef r m,MonadIO m,Memo a,TxLayer Inside r m) => MemoMode -> name -> (a -> Inside TxAdapton r m (TxU Inside TxAdapton r m b)) -> a -> Inside TxAdapton r m (TxU Inside TxAdapton r m b)
memoNonRecTxUAs mode name f =
	let key = memoKey name
	    buff_tbls = declareCMap f key
	    ori_tbl = declareWeakTable f key
	in memoNonRecTxU' mode f $! (ori_tbl :!: buff_tbls)

memoNonRecTxU' :: (Eq b,MonadRef r m,MonadIO m,Memo a,TxLayer Inside r m) => MemoMode -> (a -> Inside TxAdapton r m (TxU Inside TxAdapton r m b)) -> TxMemoTable r m (Key a) b -> a -> Inside TxAdapton r m (TxU Inside TxAdapton r m b)
memoNonRecTxU' mode f tbls@(ori_tbl :!: buff_tbls) arg = do
		starttime <- readTxTime
		let (mkWeak,k) = (memoWeak $! arg,memoKey $! arg)
		lkp <- lookupMemoTx starttime tbls k
		case lkp of
			Nothing -> do
				thunk <- f arg
				let thunkWeak = case mode of
					MemoLinear -> MkWeak (WeakKey.mkWeakRefKey (dataTxU thunk)) `andMkWeak` mkWeak -- while both the source and the target exist
					MemoSuperlinear -> mkWeak -- while the source exists
				insertMemoTx tbls thunkWeak k thunk
				return thunk
			Just thunk -> do
				return thunk

-- looks up a value in a transactional memotable by searching for it in the current and enclosing buffered memotables
lookupMemoTx :: (Eq k,Hashable k,TxLayer Inside r m) => UTCTime -> TxMemoTable r m k b -> k -> Inside TxAdapton r m (Maybe (TxU Inside TxAdapton r m b))
lookupMemoTx starttime tbls k = readTxLog >>= inL . liftIO . lookupMemoTx' tbls k
	where
	lookupMemoTx' :: (Eq k,Hashable k,TxLayer Inside r m) => TxMemoTable r m k b -> k -> TxLogs r m -> IO (Maybe (TxU Inside TxAdapton r m b))
	lookupMemoTx' tbls@(ori_tbl :!: buff_tbls) k SNil = WeakTable.lookup ori_tbl k
	lookupMemoTx' tbls@(ori_tbl :!: buff_tbls) k (SCons txlog txlogs) = do
		mb_buff_tbl <- CMap.lookup txlog buff_tbls
		case mb_buff_tbl of
			Just buff_tbl -> do
				mb <- WeakTable.lookup buff_tbl k
				case mb of
					Just (_,thunk) -> return $ Just thunk
					Nothing -> lookupMemoTx' tbls k txlogs
			Nothing -> lookupMemoTx' tbls k txlogs

insertMemoTx :: (Eq k,Hashable k,TxLayer Inside r m) => TxMemoTable r m k b -> MkWeak -> k -> TxU Inside TxAdapton r m b -> Inside TxAdapton r m ()
insertMemoTx tbls@(ori_tbl :!: buff_tbls) mkWeak k thunk = doBlock $ do
	txlogs <- readTxLog
	let txlog = Strict.head txlogs
	
	-- add the thunk to the buffered memotable
	mb <- inL $ liftIO $ CMap.lookup txlog buff_tbls
	inL $ liftIO $ case mb of
		-- if there is already a buffered entry for the log, then the table is also already in the log's record
		Just memo_tbl -> WeakTable.insertWithMkWeak memo_tbl mkWeak k (mkWeak,thunk)
		Nothing -> do
			memo_tbl <- WeakTable.new
			WeakTable.insertWithMkWeak memo_tbl mkWeak k (mkWeak,thunk)
			CMap.insert txlog memo_tbl buff_tbls

			-- add the buffered memotable to the txlog's list
			atomicModifyIORef' (txLogMemo txlog) (\xs -> (DynTxMemoTable tbls:xs,()))


instance (Typeable l,Typeable inc,Typeable r,Typeable m,Typeable a,WeakRef r) => Memo (TxM l inc r m a) where
	type Key (TxM l inc r m a) = Unique
	{-# INLINE memoKey #-}
	memoKey = idTxNM . metaTxM
	{-# INLINE memoWeak #-}
	memoWeak = \t -> MkWeak $ WeakKey.mkWeakRefKey (dataTxM t)
                                 
instance (Typeable l,Typeable inc,Typeable r,Typeable m,Typeable a,WeakRef r) => Memo (TxU l inc r m a) where
	type Key (TxU l inc r m a) = Unique
	{-# INLINE memoKey #-}
	memoKey = idTxNM . metaTxU
	{-# INLINE memoWeak #-}
	memoWeak = \t -> MkWeak $ WeakKey.mkWeakRefKey (dataTxU t)

--instance WeakKey r => Memo (TxL l inc r m a) where
--	type Key (L l inc r m a) = Unique
--	{-# INLINE memoKey #-}
--	memoKey t = (MkWeak $ WeakKey.mkWeakKey (dataL t),idNM $ metaL t)



instance Hashable (TxU l inc r m a) where
	hashWithSalt i u = hashWithSalt i (idTxNM $ metaTxU u)
instance Hashable (TxM l inc r m a) where
	hashWithSalt i m = hashWithSalt i (idTxNM $ metaTxM m)
--instance Hashable (TxL l inc r m a) where
--	hashWithSalt i l = hashWithSalt i (idNM $ metaL l)
