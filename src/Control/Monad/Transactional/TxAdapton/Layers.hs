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
	inL $ liftIO $ atomicModifyIORef' callstack (\xs -> (SCons x xs,()))

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
isThunkTxStackElement (_ :!: (SJust _) :!: _) = True
isThunkTxStackElement (_ :!: SNothing :!: _) = False

-- registers a new transaction-local allocation
newTxMLog :: (IncK TxAdapton a,TxLayer l r m,TxLayer l1 r m) => TxM l1 TxAdapton r m a -> l TxAdapton r m ()
newTxMLog m = do
	txlogs <- readTxLog
	inL $ liftIO $ WeakTable.insertWithMkWeak (txLogBuff $ Strict.head txlogs) (MkWeak $ mkWeakRefKey $! dataTxM m) (idTxNM $ metaTxM m) $! DynTxM Nothing Nothing m (TxStatus (New False,False))
newTxULog :: (IncK TxAdapton a,TxLayer l r m,TxLayer l1 r m) => TxU l1 TxAdapton r m a -> l TxAdapton r m ()
newTxULog u = do
	txlogs <- readTxLog	
	inL $ liftIO $ WeakTable.insertWithMkWeak (txLogBuff $ Strict.head txlogs) (MkWeak $ mkWeakRefKey $! dataTxU u) (idTxNM $ metaTxU u) $! DynTxU Nothing Nothing u (TxStatus (New False,False))

-- reads a value from a transactional variable
-- uses @unsafeCoerce@ since we know that the types match
readTxMValue :: (IncK TxAdapton a,TxLayer l r m,TxLayer l1 r m) => TxM l1 TxAdapton r m a -> l TxAdapton r m (a,TxStatus)
readTxMValue m = do
	tbl <- readTxLog
	dyntxvar <- inL $ bufferTxM m (TxStatus (Read,False)) tbl
	case dyntxvar of
		DynTxM (Just (BuffTxM v)) _ _ st -> return (coerce v,st)
		DynTxM Nothing _ wm st -> do
			v <- inL $ readRef (dataTxM m)
			return (v,st)

writeTxMValue :: (IncK TxAdapton a,TxLayer l r m,TxLayer l1 r m) => TxM l1 TxAdapton r m a -> a -> l TxAdapton r m ()
writeTxMValue m v' = do
	tbl <- readTxLog
	inL $ changeTxM m (Just v') (TxStatus (Write,False)) tbl
	return ()

readTxUValue :: (IncK TxAdapton a,TxLayer l r m,TxLayer l1 r m) => TxU l1 TxAdapton r m a -> TxStatus -> l TxAdapton r m (TxUData l1 TxAdapton r m a,TxStatus)
readTxUValue u status = do
	tbl <- readTxLog
	dyntxvar <- inL $ bufferTxU u status tbl
	case dyntxvar of
		DynTxU (Just (BuffTxU buff_dta)) _ _ st -> do
			(dta,_) <- inL $ readRef buff_dta
			return (coerce dta,st)
		DynTxU Nothing _ wu st -> do
			v <- inL $ readRef (dataTxU u)
			return (v,st)

writeTxUValue :: (IncK TxAdapton a,TxLayer l r m,TxLayer l1 r m) => TxU l1 TxAdapton r m a -> TxUData l1 TxAdapton r m a -> TxStatus -> l TxAdapton r m TxStatus
writeTxUValue t dta' status = do
	tbl <- readTxLog
	let chg (_,ori) = do
		ori' <- case ori of
			Left deps -> liftM Right $ liftIO $ mkWeakRefKey deps deps Nothing
			Right wdeps -> return $ Right wdeps
		return (dta',ori')
	dyn <- inL $ changeTxU t (Just chg) status tbl
	return (dynTxStatus dyn)

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

mergeDoneTxs :: (TxUnmemo r m,TxWrite) -> (TxUnmemo r m,TxWrite) -> (TxUnmemo r m,TxWrite)
mergeDoneTxs (unmemo1,writes1) (unmemo2,writes2) = (\txlog -> unmemo1 txlog >> unmemo2 txlog,Map.unionWith max writes1 writes2)

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

resetTx :: TxLayer Outside r m => Outside TxAdapton r m a -> Outside TxAdapton r m a
resetTx m = do
	now <- inL $ liftIO startTx >>= newRef
	stack <- inL $ liftIO $ newIORef SNil
	tbl <- inL $ liftIO emptyTxLog
	Reader.local (\_ -> (now :!: stack :!: SCons tbl SNil)) m

-- updating a modifiable may propagate to multiple thunks; they are all logged
-- a map from written modifiables/thunks to a boolean indicating whether it is a node write (True) or a dependents write (False)
type TxWrite = Map Unique Bool

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



