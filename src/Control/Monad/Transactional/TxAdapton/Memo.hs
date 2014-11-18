{-# LANGUAGE ConstraintKinds, UndecidableInstances, Rank2Types, BangPatterns, FunctionalDependencies, MultiParamTypeClasses, MagicHash, ScopedTypeVariables, GADTs, FlexibleContexts, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module Control.Monad.Transactional.TxAdapton.Memo (
	memoNonRecTxU, Memo(..), Hashable(..)
	) where

--	, GenericQMemoU(..),MemoCtx(..),Sat(..),gmemoNonRecU,proxyMemoCtx,NewGenericQ(..),NewGenericQMemo(..),NewGenericQMemoU(..)

import System.Mem.WeakRef as WeakRef
import System.Mem.WeakTable as WeakTable
import System.Mem.MemoTable
import Data.Hashable
import Control.Monad.Transactional.TxAdapton.Types
import Control.Monad.Transactional.TxAdapton.Layers
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

import qualified Data.HashTable.IO as HashIO
import qualified Data.HashTable.ST.Basic as HashST

--- * Generic memoization

--type GenericQMemoU ctx l inc r m b = GenericQMemo ctx U l inc r m b
--
---- | An encapsulated generic query
--newtype NewGenericQ ctx m b = NewGenericQ { unNewGenericQ :: GenericQ ctx m b }
--
--type NewGenericQMemo ctx (thunk :: (* -> (* -> *) -> (* -> *) -> * -> *) -> * -> (* -> *) -> (* -> *) -> * -> *) l inc r m b = NewGenericQ (MemoCtx ctx) (l inc r m) (thunk l inc r m b)
--type NewGenericQMemoU ctx l inc r m b = NewGenericQMemo ctx U l inc r m b
--
---- The Haskell type system is very reluctant to accept this type signature, so we need a newtype to work around it
--gmemoNonRecU :: (MonadRef r m,MonadIO m,Layer Inside inc r m) => Proxy ctx -> GenericQMemoU ctx Inside inc r m b -> GenericQMemoU ctx Inside inc r m b
--gmemoNonRecU ctx f = unNewGenericQ (newGmemoNonRecU ctx (NewGenericQ f)) where
--	newGmemoNonRecU ctx f = gmemoNonRecU' ctx f (unsafePerformIO $ debug "NewTable!!" $ WeakTable.newFor f)
--
---- | memoizes a generic function on values
--gmemoNonRecU' :: (MonadRef r m,MonadIO m,Layer Inside inc r m) => Proxy ctx -> NewGenericQMemoU ctx Inside inc r m b -> MemoTable (TypeRep,KeyDynamic) (U Inside inc r m b) -> NewGenericQMemoU ctx Inside inc r m b
--gmemoNonRecU' ctx (NewGenericQ f) tbl = NewGenericQ $ \arg -> do
--	let (mkWeak,k) = memoKeyCtx dict ctx $! arg
--	let tyk = (typeRepOf arg,keyDynamicCtx dict ctx (proxyOf arg) k)
--	lkp <- debug ("memo search "++show tyk) $ inL $ liftIO $ WeakTable.lookup tbl tyk
--	case lkp of
--		Nothing -> do
--			let finalizethunk = WeakTable.finalize tbl tyk
--			thunk <- f arg
--			let thunkmemo = addFinalizerU thunk (liftIO finalizethunk)
--			inL $ liftIO $ WeakTable.insertWithMkWeak tbl mkWeak tyk thunk
--			debug (show tyk ++" => "++show thunk) $ return thunkmemo
--		Just thunk -> debug ("memo hit "++show tyk ++ " " ++ show thunk) $ do
--			let finalizethunk = WeakTable.finalize tbl tyk
--			let thunkmemo = addFinalizerU thunk (liftIO finalizethunk)
--			return thunkmemo

-- *		

memoNonRecTxU :: (Eq b,MonadRef r m,MonadIO m,Memo a,TxLayer Inside r m) => (a -> Inside TxAdapton r m (TxU Inside TxAdapton r m b)) -> a -> Inside TxAdapton r m (TxU Inside TxAdapton r m b)
memoNonRecTxU f = do
	let !tbls = unsafePerformIO $ do
		!ori_tbl <- debug "newMemo!!!" $ WeakTable.newFor f
		!buff_tbls <- newCMapFor f
		return $! (ori_tbl,buff_tbls)
	memoNonRecTxU' f $! tbls

{-# NOINLINE newCMapFor #-}
newCMapFor :: a -> IO (CMap.Map k v)
newCMapFor x = CMap.empty

memoNonRecTxU' :: (Eq b,MonadRef r m,MonadIO m,Memo a,TxLayer Inside r m) => (a -> Inside TxAdapton r m (TxU Inside TxAdapton r m b)) -> TxMemoTable r m (Key a) b -> a -> Inside TxAdapton r m (TxU Inside TxAdapton r m b)
memoNonRecTxU' f tbls@(ori_tbl,buff_tbls) arg = do
		let (mkWeak,k) = memoKey $! arg
		lkp <- lookupMemoTx tbls k
		case lkp of
			Nothing -> do
				thunk <- f arg
				insertMemoTx tbls mkWeak k thunk
				-- mark the thunk as New in this log
--				newTxULog thunk
				return thunk
			Just thunk -> do
				-- mark the thunk as a Read in this log
--				readTxLog >>= inL . bufferTxU thunk Read
				return thunk

-- looks up a value in a transactional memotable by searching for it in the current and enclosing buffered memotables
lookupMemoTx :: (Eq k,Hashable k,TxLayer Inside r m) => TxMemoTable r m k b -> k -> Inside TxAdapton r m (Maybe (TxU Inside TxAdapton r m b))
lookupMemoTx tbls k = readTxLog >>= inL . liftIO . lookupMemoTx' tbls k
	where
	lookupMemoTx' :: (Eq k,Hashable k,TxLayer Inside r m) => TxMemoTable r m k b -> k -> TxLogs r m -> IO (Maybe (TxU Inside TxAdapton r m b))
	lookupMemoTx' tbls@(ori_tbl,buff_tbls) k SNil = WeakTable.lookup ori_tbl k
	lookupMemoTx' tbls@(ori_tbl,buff_tbls) k (SCons txlog txlogs) = do
		mb_buff_tbl <- CMap.lookup txlog buff_tbls
		case mb_buff_tbl of
			Just buff_tbl -> do
				mb <- WeakTable.lookup buff_tbl k
				case mb of
					Just (_,thunk) -> return $ Just thunk
					Nothing -> lookupMemoTx' tbls k txlogs
			Nothing -> lookupMemoTx' tbls k txlogs

insertMemoTx :: (Eq k,Hashable k,TxLayer Inside r m) => TxMemoTable r m k b -> MkWeak -> k -> TxU Inside TxAdapton r m b -> Inside TxAdapton r m ()
insertMemoTx tbls@(ori_tbl,buff_tbls) mkWeak k thunk = do
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
	
	

instance WeakRef r => Memo (TxM l inc r m a) where
	type Key (TxM l inc r m a) = Unique
	{-# INLINE memoKey #-}
	memoKey t = (MkWeak $ WeakRef.mkWeakWithRefKey (dataTxM t),idTxNM $ metaTxM t)
                                 
instance WeakRef r => Memo (TxU l inc r m a) where
	type Key (TxU l inc r m a) = Unique
	{-# INLINE memoKey #-}
	memoKey t = (MkWeak $ WeakRef.mkWeakWithRefKey (dataTxU t),idTxNM $ metaTxU t)

--instance WeakRef r => Memo (TxL l inc r m a) where
--	type Key (L l inc r m a) = Unique
--	{-# INLINE memoKey #-}
--	memoKey t = (MkWeak $ WeakRef.mkWeakWithRefKey (dataL t),idNM $ metaL t)



instance Hashable (TxU l inc r m a) where
	hashWithSalt i u = hashWithSalt i (idTxNM $ metaTxU u)
instance Hashable (TxM l inc r m a) where
	hashWithSalt i m = hashWithSalt i (idTxNM $ metaTxM m)
--instance Hashable (TxL l inc r m a) where
--	hashWithSalt i l = hashWithSalt i (idNM $ metaL l)
