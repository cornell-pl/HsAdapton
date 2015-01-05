{-# LANGUAGE TupleSections, StandaloneDeriving, BangPatterns, EmptyDataDecls, FlexibleContexts, TypeOperators, ConstraintKinds, MagicHash, ViewPatterns, KindSignatures, GADTs, ScopedTypeVariables, DeriveDataTypeable, TemplateHaskell #-}

module Control.Monad.Transactional.TxAdapton.Types where

import Control.Monad.Incremental
import Control.Concurrent
import Control.Monad.Transactional
import qualified Control.Concurrent.Map as CMap
import Control.Monad.Incremental.Adapton.Types
import Data.Concurrent.Deque.Class as Queue
import Data.Concurrent.Deque.Reference.DequeInstance
import Data.Time.Clock
import Control.Concurrent.Chan
import System.IO.Unsafe
import Control.Monad.Fix
import Data.Global.TH as TH

import Data.Maybe
import Data.Unique
import Data.Typeable
import Data.DeepTypeable
import Data.WithClass.Derive.DeepTypeable
import Data.DeriveTH
import Language.Haskell.TH.Syntax hiding (lift,Infix,Fixity)

import qualified Data.HashTable.IO as HashIO
import qualified Data.HashTable.ST.Basic as HashST
import Control.Monad.Ref
import Data.IORef
import Control.Monad.IO.Class
import System.Mem.Weak as Weak
import System.Mem.WeakSet as WeakSet
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map
import Data.Foldable as Foldable
import Data.Strict.Maybe as Strict
import Data.Strict.List as Strict
import Data.Strict.Tuple as Strict
import qualified Unsafe.Coerce as Unsafe
import Control.Monad.Reader (Reader(..),ReaderT(..),MonadReader(..))
import qualified Control.Monad.Reader as Reader
import Control.Monad.Catch
import Control.Concurrent.Lock as Lock
import Control.Concurrent.MVar
import Control.Monad
import System.Mem.WeakKey
import System.Mem.MemoTable
import Data.Hashable
import System.Mem.WeakTable as WeakTable
import Data.Set as Set
import Data.List as List
import System.Mem.Concurrent.WeakMap as CWeakMap
import Debug

data TxAdapton deriving Typeable

$( derive makeDeepTypeableAbstract ''TxAdapton )

data TxStatus = Read Bool | Eval | Write | New deriving (Eq,Show,Typeable)
-- read does not copy the original data, and just logs that it has been accessed (the boolean says whether we have buffered dependents or not)
-- eval is a non-conflicting write; we compute data that is consistent with the original state
-- write is a write to a modifiable or dirtying of a thunk
-- new is for new transaction allocations

isReadTrueOrEval (Read True) = True
isReadTrueOrEval Eval = True
isReadTrueOrEval _ = False

isEvalOrWrite Eval = True
isEvalOrWrite Write = True
isEvalOrWrite _ = False

instance Ord TxStatus where
	(Read False) <= s2 = True
	(Read True) <= Read False = False
	(Read True) <= s2 = True
	Eval <= (Read _) = False
	Eval <= s2 = True
	Write <= (Read _) = False
	Write <= Eval = False
	Write <= s2 = True
	New <= New = True
	New <= s2 = False

-- | A table mapping variable identifiers to buffered content, together with a list of memo tables that contain transient entries for this log
-- has a unique identifier so that it can be a key in a table of buffered memotables
-- this list is local to the tx (so no concurrent handling is needed) and used to know which tables to merge with the persistent memo tables at commit time
newtype TxLog r m = TxLog (Unique :!: WeakTable Unique (DynTxVar r m) :!: IORef [DynTxMemoTable r m]) deriving Typeable

instance Eq (TxLog r m) where
	(TxLog (id1 :!: _ :!: _)) == (TxLog (id2 :!: _ :!: _)) = id1 == id2
instance Hashable (TxLog r m) where
	hashWithSalt i (TxLog (id1 :!: _ :!: _)) = hashWithSalt i id1

finalizeTxLog :: MonadIO m => TxLog r m -> m ()
finalizeTxLog txlog = do
	liftIO $ WeakTable.finalize (txLogBuff txlog)
	

-- a tx environment contains (a tx start time,a callstack,a list of nested logs -- to support nested transactions)
type TxEnv r m = (r UTCTime :!: TxCallStack r m :!: TxLogs r m)

txLogId (TxLog (x :!: y :!: z)) = x
txLogBuff (TxLog (x :!: y :!: z)) = y
txLogMemo (TxLog (x :!: y :!: z)) = z

emptyTxLog :: IO (TxLog r m)
emptyTxLog = do
	uid <- newUnique
	memos <- newIORef []
	buff <- WeakTable.new
	return $ TxLog (uid :!: buff :!: memos)

-- a buffered thunk and an optional global thunk
-- we use weak pointers to the original data because we don't want to keep original thunks alive unecessarily; if the original data dies then the buffered data dies
-- read = Nothing original
-- eval = Just original
-- write = Just original
-- new = Nothing new
data DynTxVar r m where
	DynTxU :: (Typeable a,Eq a,TxLayer l r m) => Maybe (BuffTxU l r m a) -> Maybe (TxDependents r m) -> TxU l TxAdapton r m a -> TxStatus -> DynTxVar r m
	DynTxM :: (Typeable a,Eq a,TxLayer l r m) => Maybe (BuffTxM l r m a) -> Maybe (TxDependents r m) -> TxM l TxAdapton r m a -> TxStatus -> DynTxVar r m
--	DynTxL :: Maybe (BuffTxL l inc r m a) -> TxL l inc r m a -> TxStatus -> DynTxVar r m

isNewDynTxVar :: DynTxVar r m -> Bool
isNewDynTxVar t = dynTxStatus t == New

isWriteDynTxVar :: DynTxVar r m -> Bool
isWriteDynTxVar t = dynTxStatus t == Write

-- returns a set of locks for variables to which a txlog intends to write (@Eval@ and Write@) and to read from (@Read@)
txLocks :: TxLogs r m -> IO (Map Unique (Lock,Bool))
txLocks = Foldable.foldrM getLocks Map.empty where
	getLocks :: TxLog r m -> Map Unique (Lock,Bool) -> IO (Map Unique (Lock,Bool))
	getLocks txlog lcks = WeakTable.foldM add lcks (txLogBuff txlog) where
		add xs (uid,tvar)
		 	| isWriteLock tvar = return $ Map.insertWith merge uid (dynTxVarLock tvar,True) xs -- write
			| dynTxStatus tvar == New = return xs -- @New@ thread-allocated variables don't need locks
			| otherwise = return $ Map.insertWith merge uid (dynTxVarLock tvar,False) xs -- read
	merge (lck,b1) (_,b2) = (lck,max b1 b2)
	-- since dependents are stored in a concurrent map, @Read@s don't require locks
	isWriteLock :: DynTxVar r m -> Bool
	--isWriteLock (DynTxU _ (Just txdeps) _ Read) = True
	--isWriteLock (DynTxM _ (Just txdeps) _ Read) = True
	isWriteLock tvar = isEvalOrWrite (dynTxStatus tvar)

-- we use a lock-free map since multiple threads may be accessing it to register/unregister new txlogs
-- a persistent memo table and a family of buffered (transaction-local) memo tables indexed by a buffered tx log
-- note that added entries to the memotable are always New (hence we have TxU and not BuffTxU in buffered tables)
type TxMemoTable r m k b = (MemoTable k (TxU Inside TxAdapton r m b) :!: CMap.Map (TxLog r m) (TxBuffMemoTable r m k b))

-- commits the buffered memotable entries for a given txlog to the persistent memotable
-- returns a function that can be used to remove the newly commited entries from other concurrent buffered memo tables
commitTxLogMemoTables :: TxLog r m -> IO (TxUnmemo r m)
commitTxLogMemoTables txlog = do
	txmemos <- readIORef (txLogMemo txlog)
	Control.Monad.foldM (\unmemos (DynTxMemoTable txmemo) -> liftM (joinTxUnmemos unmemos) $ commitTxMemoTable txmemo txlog) emptyTxUnmemo txmemos

commitTxMemoTable :: (Eq k,Hashable k) => TxMemoTable r m k b -> TxLog r m -> IO (TxUnmemo r m)
commitTxMemoTable (ori_tbl :!: buff_tbls) txlog = do
	mb <- CMap.lookup txlog buff_tbls
	case mb of
		Just buff_tbl -> do
			-- adds buffered entries to the persistent memo table, overriding original entries
			let addEntry xs (k,(mkWeak,u)) = WeakTable.insertWithMkWeak ori_tbl mkWeak k u >> return (SCons k xs)
			!entries <- WeakTable.foldM addEntry SNil buff_tbl -- strict list
			CMap.delete txlog buff_tbls
			WeakTable.finalize buff_tbl
			-- function that unmemoizes buffered memo entries from a given log in the concurrent map
			let unmemo otherlog = do
				mb <- CMap.lookup otherlog buff_tbls
				case mb of
					Just other_tbl -> Foldable.mapM_ (WeakTable.finalizeEntry other_tbl) entries
					Nothing -> return ()
			return unmemo
		Nothing -> return emptyTxUnmemo

type TxBuffMemoTable r m k b = MemoTable k (MkWeak,TxU Inside TxAdapton r m b)
-- function that unmemoizes buffered entries from the buffered table of a specific txlog
type TxUnmemo r m = TxLog r m -> IO ()

emptyTxUnmemo txlog = return ()
joinTxUnmemos f g = \txlog -> f txlog >> g txlog

data DynTxMemoTable r m where
	DynTxMemoTable :: (Eq k,Hashable k) => TxMemoTable r m k b -> DynTxMemoTable r m

type TxLayer l r m = (Typeable l,Typeable m,Typeable r,MonadMask (l TxAdapton r m),MonadMask m,MonadCatch m,Layer l TxAdapton r m,MonadReader (TxEnv r m) (l TxAdapton r m),MonadIO m)

-- an IORef is just fine, because the stack is transaction-local
-- the dependencies references in the stack are always buffered copies
type TxCallStack (r :: * -> *) (m :: * -> *) = IORef (TxCallStack' r m)
type TxCallStack' (r :: * -> *) (m :: * -> *) = SList (TxStackElement r m)

type TxStackElement (r :: * -> *) (m :: * -> *) = (TxNodeMeta r m :!: SMaybe (TxDependencies r m))

-- commits dependencies
commitDependenciesTx :: (MonadIO m,WeakRef r,MonadRef r m) => TxDependencies r m -> Maybe (TxDependencies r m) -> m ()
commitDependenciesTx dependencies mb = do
	case mb of
		Nothing -> return ()
		Just oridependencies -> do
			liftIO $ mkWeakRefKey dependencies oridependencies Nothing >> return ()
			-- we finalize all the old dependencies, to prevent old original dependencies to remain buffered
--			clearDependenciesTx True oridependencies

markOriginalDependenciesTx :: MonadRef r m => TxDependencies r m -> m ()
markOriginalDependenciesTx dependencies = readRef dependencies >>= Foldable.mapM_ markDependencyTx where
	markDependencyTx (d,w) = writeRef (flagTxW d) True

-- a reference holding buffered data and maybe a pointer to the original dependencies reference
type BuffTxUData l inc r m a = (TxUData l inc r m a , Maybe (TxDependencies r m))

newtype BuffTxU (l :: * -> (* -> *) -> (* -> *) -> * -> *) (r :: * -> *) (m :: * -> *) a = BuffTxU { unBuffTxU :: (r (BuffTxUData l TxAdapton r m a) , TxDependents r m) }

newtype BuffTxM (l :: * -> (* -> *) -> (* -> *) -> * -> *) (r :: * -> *) (m :: * -> *) a = BuffTxM { unBuffTxM :: (a , TxDependents r m) }

type TxDependencies r m = r (SList (TxDependency r m,Weak (TxDependency r m)))
-- bool says whether the dependency is original or buffered
newtype TxDependency (r :: * -> *) (m :: * -> *) = TxDependency (
		TxNodeMeta r m -- the metadata of the source node
	,	r Bool -- dirty flag
	,	Inside TxAdapton r m Bool -- a checking condition with the previously seen value of the source node
	,	TxNodeMeta r m -- the metadata of the target node
	,   r Bool -- original/buffered flag
	,	MkWeak -- parent dependencies list
	)

deriving instance Typeable TxDependency

srcMetaTxW (TxDependency (srcMeta,dirty,check,tgtMeta,flag,dependencies)) = srcMeta
dirtyTxW (TxDependency (srcMeta,dirty,check,tgtMeta,flag,dependencies)) = dirty
checkTxW (TxDependency (srcMeta,dirty,check,tgtMeta,flag,dependencies)) = check
tgtMetaTxW (TxDependency (srcMeta,dirty,check,tgtMeta,flag,dependencies)) = tgtMeta
flagTxW (TxDependency (srcMeta,dirty,check,tgtMeta,flag,dependencies)) = flag
dependenciesTxW (TxDependency (srcMeta,dirty,check,tgtMeta,flag,dependencies)) = dependencies

type TxDependents (r :: * -> *) (m :: * -> *) = WeakMap Unique (TxDependent r m)
type TxDependent r m = TxDependency r m

newtype TxM (l :: * -> (* -> *) -> (* -> *) -> * -> *) inc (r :: * -> *) (m :: * -> *) a = TxM (
		r a -- a constant value
	,	TxNodeMeta r m
	) deriving (Typeable)

dataTxM (TxM (v,meta)) = v
metaTxM (TxM (v,meta)) = meta

instance Eq (TxM l inc r m a) where
	t1 == t2 = idTxNM (metaTxM t1) == idTxNM (metaTxM t2)

newtype TxU (l :: * -> (* -> *) -> (* -> *) -> * -> *) inc (r :: * -> *) (m :: * -> *) a = TxU (
		r (TxUData l inc r m a) -- data
	,	TxNodeMeta r m		-- metadata
	) deriving (Typeable)

dataTxU (TxU (dta,meta)) = dta
metaTxU (TxU (dta,meta)) = meta

data TxUData (l :: * -> (* -> *) -> (* -> *) -> * -> *) inc (r :: * -> *) (m :: * -> *) a =
	  TxValue !UBool !a !(l inc r m a) !(TxDependencies r m) -- a thunk that has been previously evaluated; invariant: the node is dirty if any of its dependencies is dirty; the dirty flag is just an optimization
	| TxThunk !(l inc r m a) -- a thunk that has never been evaluated
	| TxConst !a -- a constant value
  deriving Typeable

instance Eq (TxU l inc r m a) where
	t1 == t2 = idTxNM (metaTxU t1) == idTxNM (metaTxU t2)

newtype TxNodeMeta (r :: * -> *) (m :: * -> *) = TxNodeMeta (
		Unique
	,	TxDependents r m -- list of dependents; we purge the list at finalization time
	,	(TxLogs r m -> m ()) -- function that dirties the corresponding value (thunks only)
	,	(TxLogs r m -> m ()) -- function that forgets all the old cached thunk data (thunks only)
	,   (TxStatus -> TxLogs r m -> m (DynTxVar r m)) -- function that writes a buffered copy of the original data to a transaction log
	,   Maybe (TxCreator r m) -- the parent thunk under which the reference was created (modifiables only)
	,   WaitQueue -- a sequence of wake-up actions for txs that are waiting on further updates to this node (modifiables only)
	,   Lock -- a lock for writes to this variable
	)

-- (a new starttime,repairing actions over an invalid environment)
type Repair = (UTCTime,Set Unique)
type TxRepair r m = (UTCTime,TxRepair' r m)
type TxRepair' r m = Outside TxAdapton r m ()

type TxLogs r m = SList (TxLog r m)

idTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck)) = uid
dependentsTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck)) = deps
dirtyTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck)) = dirty
forgetTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck)) = forget
bufferTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck)) = buffer
creatorTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck)) = creator
waitTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck)) = wait
lockTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck)) = lck

-- a wait queue of tx locks and environments
-- it does not need to be thread-safe, because we only push and pop elements inside validation/commit phases
type WaitQueue = Deque Nonthreadsafe Nonthreadsafe SingleEnd SingleEnd Grow Safe Lock

-- | registers a new wait on modifications of a node when a lock is provided
enqueueWait :: MonadIO m => Lock -> TxNodeMeta r m -> m ()
enqueueWait lck meta = liftIO $ pushL (waitTxNM meta) lck

-- the same tx may depend on various modifiables, so we need to account for multiple wakeups
tryRelease :: Lock -> IO ()
tryRelease lck = do
	isLocked <- locked lck
	if isLocked then release lck else return ()

-- a tx wakeup pair (update buffered data,release locks)
--type TxWake m = (m (),IO ())
--type TxWake = IO ()
type Wakes = Map Unique Lock

type TxCreator r m = WTxNodeMeta r m
type WTxNodeMeta r m = Weak (TxNodeMeta r m)

dynTxVarLock :: DynTxVar r m -> Lock
dynTxVarLock (DynTxM _ _ m _) = lockTxNM $ metaTxM m
dynTxVarLock (DynTxU _ _ u _) = lockTxNM $ metaTxU u

dynVarTxCreator :: DynTxVar r m -> Maybe (TxCreator r m)
dynVarTxCreator (DynTxM _ _ m _) = creatorTxNM $ metaTxM m
dynVarTxCreator (DynTxU _ _ u _) = creatorTxNM $ metaTxU u

-- merge the original dependents on top of the buffered ones (without overriding buffered content)
dynTxVarDependents :: MonadIO m => DynTxVar r m -> m (TxDependents r m)
dynTxVarDependents (DynTxM (Just (BuffTxM (_ , deps))) _ _ _) = return deps
dynTxVarDependents (DynTxU (Just (BuffTxU (_ , deps))) _ _ _) = return deps
dynTxVarDependents (DynTxM Nothing mbtxdeps m _) = do
	case mbtxdeps of
		Just txdeps -> do
			extendTxDependents txdeps (dependentsTxNM $ metaTxM m)
			return txdeps
		Nothing -> return $ dependentsTxNM $ metaTxM m
dynTxVarDependents (DynTxU Nothing mbtxdeps u _) = do
	case mbtxdeps of
		Just txdeps -> do
			extendTxDependents txdeps (dependentsTxNM $ metaTxU u)
			return txdeps
		Nothing -> return $ dependentsTxNM $ metaTxU u

-- we overlap original buffered dependencies
extendTxDependents :: (MonadIO m,MonadRef r m) => TxDependents r m -> TxDependents r m -> m ()
extendTxDependents = CWeakMap.mergeWithKey (\oldd newd -> readRef (flagTxW oldd)) dependenciesTxW

-- returns only the buffered dependents of a buffered variable
dynTxVarBufferedDependents :: MonadIO m => DynTxVar r m -> m (Maybe (TxDependents r m))
dynTxVarBufferedDependents (DynTxM (Just (BuffTxM (_ , deps))) _ _ _) = return $ Just deps
dynTxVarBufferedDependents (DynTxU (Just (BuffTxU (_ , deps))) _ _ _) = return $ Just deps
dynTxVarBufferedDependents (DynTxM Nothing Nothing m New) = return $ Just $ dependentsTxNM $ metaTxM m
dynTxVarBufferedDependents (DynTxU Nothing Nothing u New) = return $ Just $ dependentsTxNM $ metaTxU u
dynTxVarBufferedDependents (DynTxM Nothing mbtxdeps m _) = return mbtxdeps
dynTxVarBufferedDependents (DynTxU Nothing mbtxdeps u _) = return mbtxdeps

dynTxVarDependencies :: (MonadIO m,MonadRef r m) => DynTxVar r m -> m (Maybe (TxDependencies r m))
dynTxVarDependencies (DynTxU (Just (BuffTxU (buff_dta , _))) _ _ _) = do
	(dta,_) <- readRef buff_dta
	case dta of
		TxValue dirty value force dependencies -> return $ Just dependencies
		otherwise -> return Nothing
dynTxVarDependencies (DynTxU _ _ u _) = do
	readRef (dataTxU u) >>= \dta -> case dta of
		TxValue dirty value force dependencies -> return $ Just dependencies
		otherwise -> return Nothing
dynTxVarDependencies _ = return Nothing

-- stores a modifiable in a buffer with a minimal status and returns a buffered copy
-- Read = an entry is enough
-- Eval = a consistent entry is enough
-- Write = creates a new or inconsistent entry
bufferTxM :: (Typeable a,Eq a,TxLayer l r m,MonadRef r m,MonadIO m) => TxM l TxAdapton r m a -> TxStatus -> TxLogs r m -> m (DynTxVar r m)
bufferTxM m (Read False) txlogs = do
	let !idm = idTxNM $ metaTxM m
	mb <- liftIO $ findTxLogEntry txlogs idm
	case mb of
		Just tvar -> return tvar
		Nothing -> do
			let !tvar = DynTxM Nothing Nothing m $ Read False
			liftIO $ WeakTable.insertWithMkWeak (txLogBuff $ Strict.head txlogs) (MkWeak $ mkWeakRefKey $ dataTxM m) idm tvar
			return tvar
bufferTxM m (isReadTrueOrEval -> True) txlogs = do -- evaluating a strict modifiable can be treated as a @Read@ with buffered dependents
	let !idm = idTxNM $ metaTxM m
	mb <- liftIO $ findTxLogEntry txlogs idm
	let new_entry mbtxdeps = do
		!buff_deps <- case mbtxdeps of
			Just txdeps -> return txdeps
			Nothing -> liftIO $ CWeakMap.new'
		!buff_value <- readRef (dataTxM m)
		let !tvar = DynTxM Nothing (Just buff_deps) m $ Read True
		liftIO $ WeakTable.insertWithMkWeak (txLogBuff $ Strict.head txlogs) (MkWeak $ mkWeakRefKey $ dataTxM m) idm tvar
		return tvar
	case mb of
		Just (DynTxM Nothing mbtxdeps _ (Read _)) -> new_entry mbtxdeps
		Just tvar -> return tvar
		Nothing -> new_entry Nothing
bufferTxM m Write txlogs = changeTxM m Nothing txlogs

-- changes a modifiable, or just buffers it
changeTxM :: (Typeable a,Eq a,TxLayer l r m,MonadRef r m,MonadIO m) => TxM l TxAdapton r m a -> Maybe a -> TxLogs r m -> m (DynTxVar r m)
changeTxM m mbv' txlogs = do
	let !idm = idTxNM $ metaTxM m
	mb <- liftIO $ findTxLogEntry txlogs idm
	let new_entry mbtxdeps = do
		!buff_deps <- case mbtxdeps of
			Just txdeps -> extendTxDependents txdeps (dependentsTxNM $ metaTxM m) >> return txdeps
			Nothing -> CWeakMap.copyWithKey dependenciesTxW (dependentsTxNM $ metaTxM m)
		!old_value <- readRef (dataTxM m)
		let !v' = maybe old_value id mbv'
		let !tvar = DynTxM (Just (BuffTxM (v' , buff_deps))) Nothing m Write
		liftIO $ WeakTable.insertWithMkWeak (txLogBuff $ Strict.head txlogs) (MkWeak $ mkWeakRefKey $ dataTxM m) idm tvar
		return tvar
	case mb of
		Just (DynTxM Nothing mbtxdeps _ (Read _)) -> new_entry mbtxdeps
		Just (DynTxM (Just (BuffTxM (buff_value , buff_deps))) _ _ (isEvalOrWrite -> True)) -> do
			let !v' = maybe (coerce buff_value) id mbv'
			let !tvar = DynTxM (Just $ BuffTxM (v' , buff_deps)) Nothing m Write
			liftIO $ WeakTable.insertWithMkWeak (txLogBuff $ Strict.head txlogs) (MkWeak $ mkWeakRefKey $ dataTxM m) idm tvar
			return tvar
		Just tvar@(DynTxM Nothing _ _ New) -> case mbv' of
			Nothing -> return tvar
			Just v' -> do
				let !datam = dataTxM m
				writeRef datam v'
				return tvar
		Nothing -> new_entry Nothing

bufferTxU :: (Typeable a,Eq a,TxLayer l r m,MonadRef r m,MonadIO m) => TxU l TxAdapton r m a -> TxStatus -> TxLogs r m -> m (DynTxVar r m)
bufferTxU u (Read False) txlogs = do 
	let !idu = idTxNM $ metaTxU u
	mb <- liftIO $ findTxLogEntry txlogs idu
	case mb of
		Just tvar -> return tvar
		Nothing -> do
			let !tvar = DynTxU Nothing Nothing u (Read False)
			liftIO $ WeakTable.insertWithMkWeak (txLogBuff $ Strict.head txlogs) (MkWeak $ mkWeakRefKey $ dataTxU u) idu tvar
			return tvar
bufferTxU u (Read True) txlogs = do
	let !idu = idTxNM $ metaTxU u
	mb <- liftIO $ findTxLogEntry txlogs idu
	let new_entry mbtxdeps = do
		-- the copies point to the original dependencies reference
		!buff_deps <- case mbtxdeps of
			Just txdeps -> return txdeps
			Nothing -> liftIO $ CWeakMap.new'
		let !tvar = DynTxU Nothing (Just buff_deps) u (Read True)
		liftIO $ WeakTable.insertWithMkWeak (txLogBuff $ Strict.head txlogs) (MkWeak $ mkWeakRefKey $ dataTxU u) idu tvar
		return tvar
	case mb of
		Just (DynTxU Nothing mbtxdeps _ (Read _)) -> new_entry mbtxdeps
		Just tvar -> return tvar
		Nothing -> new_entry Nothing
bufferTxU u Eval txlogs = do
	let !idu = idTxNM $ metaTxU u
	mb <- liftIO $ findTxLogEntry txlogs idu
	let new_entry mbtxdeps = do
		-- the copies point to the original dependencies reference
		!buff_deps <- case mbtxdeps of
			Just txdeps -> extendTxDependents txdeps (dependentsTxNM $ metaTxU u) >> return txdeps
			Nothing -> CWeakMap.copyWithKey dependenciesTxW (dependentsTxNM $ metaTxU u)
		!buff_dta <- readRef (dataTxU u) >>= copyTxUData
		let !tvar = DynTxU (Just $ BuffTxU (buff_dta , buff_deps)) Nothing u Eval
		liftIO $ WeakTable.insertWithMkWeak (txLogBuff $ Strict.head txlogs) (MkWeak (mkWeakRefKey $ dataTxU u)) idu tvar
		return tvar
	case mb of
		Just (DynTxU Nothing mbtxdeps _ (Read _)) -> new_entry mbtxdeps
		Just tvar -> return tvar
		Nothing -> new_entry Nothing
bufferTxU u Write txlogs = {-debugM "bufferTxU4" $ -}changeTxU u Nothing Write txlogs

-- changes a thunk, or just buffers it
changeTxU :: (Typeable a,Eq a,TxLayer l r m,MonadRef r m,MonadIO m) => TxU l TxAdapton r m a -> Maybe (BuffTxUData l TxAdapton r m a -> m (BuffTxUData l TxAdapton r m a)) -> TxStatus -> TxLogs r m -> m (DynTxVar r m)
changeTxU u mbChgDta status txlogs = do
	let !idu = idTxNM $ metaTxU u
	mb <- liftIO $ findTxLogEntry txlogs idu
	let new_entry mbtxdeps = do
		!buff_deps <- case mbtxdeps of
			Just txdeps -> extendTxDependents txdeps (dependentsTxNM $ metaTxU u) >> return txdeps
			Nothing -> CWeakMap.copyWithKey dependenciesTxW (dependentsTxNM $ metaTxU u)
		!buff_dta <- readRef (dataTxU u) >>= copyTxUData
		mapRefM_ (maybe return id mbChgDta) buff_dta
		let !tvar = DynTxU (Just $ BuffTxU (buff_dta , buff_deps)) Nothing u status
		liftIO $ WeakTable.insertWithMkWeak (txLogBuff $ Strict.head txlogs) (MkWeak (mkWeakRefKey $ dataTxU u)) idu tvar
		return tvar
	case mb of
		Just (DynTxU Nothing mbtxdeps _ (Read _)) -> new_entry mbtxdeps
		Just (DynTxU (Just (BuffTxU (buff_dta , buff_deps))) _ _ buff_status@(isEvalOrWrite -> True)) -> do
			let chg = maybe return id mbChgDta
			mapRefM_ chg (coerce buff_dta)
			let !tvar = DynTxU (Just $ BuffTxU (coerce buff_dta , buff_deps)) Nothing u (max buff_status status)
			-- all changes are logged on the nested transaction's log
			liftIO $ WeakTable.insertWithMkWeak (txLogBuff $ Strict.head txlogs) (MkWeak (mkWeakRefKey $ dataTxU u)) idu tvar
			return tvar
		Just tvar@(DynTxU Nothing _ _ New) -> do
			case mbChgDta of
				Nothing -> return ()
				Just chgDta -> mapRefM_ (liftM Prelude.fst . chgDta . (,Nothing)) (dataTxU u)
			return tvar
		Nothing -> new_entry Nothing

-- remembers the original dependencies reference
copyTxUData :: MonadRef r m => TxUData l TxAdapton r m a -> m (r (BuffTxUData l TxAdapton r m a))
copyTxUData dta@(TxValue dirty value force dependencies) = do
	buff_dependencies <- readRef dependencies >>= newRef
	buff_dta <- newRef (TxValue dirty value force buff_dependencies,Just dependencies)
	return buff_dta
copyTxUData dta = newRef (dta,Nothing)

--{-# INLINE coerce #-}
--coerce :: a -> b
--coerce x = Unsafe.unsafeCoerce x

coerce :: (Typeable a,Typeable b) => a -> b
coerce x = case cast x of
	Nothing -> error "failed coerce"
	Just y -> y

-- finds a buffered entry in any depth of a nested tx log
findTxLogEntry :: TxLogs r m -> Unique -> IO (Maybe (DynTxVar r m))
findTxLogEntry txlogs uid = Foldable.foldlM (\mb txlog -> maybe (WeakTable.lookup (txLogBuff txlog) uid) (return . Just) mb) Nothing txlogs

-- merges a nested txlog with its parent txlog, by overriding parent entries
mergeTxLog :: MonadIO m => TxLog r m -> TxLog r m -> m ()
mergeTxLog txlog1 txlog2 = liftIO $ do
	WeakTable.mapM_ (\(uid,entry) -> WeakTable.insertWithMkWeak (txLogBuff txlog2) (dynTxMkWeak entry) uid entry) (txLogBuff txlog1)

-- merges the memo entries for a nested txlog into the memotables of its parent txlog, by overriding parent entries
mergeTxLogMemos :: MonadIO m => TxLog r m -> TxLog r m -> m ()
mergeTxLogMemos txlog_child txlog_parent = liftIO $ do
	txmemos <- readIORef (txLogMemo txlog_child)
	let mergeMemo (DynTxMemoTable (ori_tbl :!: buff_tbls)) = do
		mb_child <- CMap.lookup txlog_child buff_tbls
		case mb_child of
			Just buff_tbl_child -> do
				mb_parent <- CMap.lookup txlog_parent buff_tbls
				case mb_parent of
					Just buff_tbl_parent -> do
						WeakTable.mapM_ (\(k,(mkWeak,v)) -> WeakTable.insertWithMkWeak buff_tbl_parent mkWeak k (mkWeak,v)) buff_tbl_child
						WeakTable.finalize buff_tbl_child
					Nothing -> CMap.insert txlog_parent buff_tbl_child buff_tbls
				CMap.delete txlog_child buff_tbls
			Nothing -> return ()
	Control.Monad.mapM_ mergeMemo txmemos

-- ** dirtying

dirtyBufferedDynTxVar :: (WeakRef r,MonadIO m,MonadRef r m) => TxLog r m -> DynTxVar r m -> m ()
dirtyBufferedDynTxVar txlog tvar = do
	dirtyCreatorBufferedTx txlog (dynVarTxCreator tvar)
	dirtyRecursivelyBufferedDynTxVar txlog tvar

dirtyCreatorBufferedTx :: (MonadIO m,MonadRef r m,WeakRef r) => TxLog r m -> Maybe (TxCreator r m) -> m ()
dirtyCreatorBufferedTx txlog Nothing = return ()
dirtyCreatorBufferedTx txlog (Just wcreator) = do
	mb <- liftIO $ deRefWeak wcreator
	case mb of
		Just creatorMeta -> do
			mb <- WeakTable.lookup (txLogBuff txlog) (idTxNM creatorMeta)
			case mb of
				Nothing -> return ()
				Just creator_var -> do
					forgetBufferedTxData creator_var
					dirtyRecursivelyBufferedDynTxVar txlog creator_var
		Nothing -> return ()

-- dirties only buffered dependents in a txlog, without changing the status of their variables
dirtyRecursivelyBufferedDynTxVar :: (MonadIO m,MonadRef r m) => TxLog r m -> DynTxVar r m -> m ()
dirtyRecursivelyBufferedDynTxVar txlog tvar = do
	mb <- dynTxVarBufferedDependents tvar
	case mb of
		Nothing -> return () -- stop when there are no buffered dependents
		Just buff_dependents -> do
			dependents <- CWeakMap.toMap buff_dependents
			Foldable.mapM_ dirtyTx' dependents
  where
	dirtyTx' = \w -> do
		mb <- liftIO $ Weak.deRefWeak w
		case mb of
			Nothing -> return ()
			Just d -> do
				isDirty <- readRef (dirtyTxW d)
				isOriginal <- readRef (flagTxW d)
				unless (isDirty || isOriginal) $ do -- stop when the dependency is original or already dirty
					writeRef (dirtyTxW d) True -- dirty the buffered dependency
					mb <- WeakTable.lookup (txLogBuff txlog) (idTxNM $ tgtMetaTxW d)
					case mb of
						Nothing -> return () -- stop when the dependent variable is not buffered
						Just tgt_tvar -> do
							dirtyBufferedTxData tgt_tvar
							dirtyRecursivelyBufferedDynTxVar txlog tgt_tvar

-- dirties a buffered variable in-place without changing its status
dirtyBufferedTxData :: MonadRef r m => DynTxVar r m -> m ()
dirtyBufferedTxData (DynTxU Nothing _ u New) = mapRef chgDirty (dataTxU u) where
	chgDirty (TxValue _ value force dependencies) = TxValue 1# value force dependencies
	chgDirty dta = dta
dirtyBufferedTxData (DynTxU (Just (BuffTxU (buff_dta , txrdependents))) _ _ _) = mapRef chgDirty buff_dta where
	chgDirty (TxValue _ value force dependencies,ori) = (TxValue 1# value force dependencies,ori)
	chgDirty dta = dta
dirtyBufferedTxData _ = return ()

forgetBufferedTxData :: MonadRef r m => DynTxVar r m -> m ()
forgetBufferedTxData (DynTxU Nothing _ u New) = mapRef forget (dataTxU u) where
	forget (TxValue _ value force dependencies) = TxThunk force
	forget dta = dta
forgetBufferedTxData (DynTxU (Just (BuffTxU (buff_dta , txrdependents))) _ _ _) = mapRef forget buff_dta where
	forget (TxValue _ value force dependencies,ori) = (TxThunk force,Nothing)
	forget dta = dta
forgetBufferedTxData _ = return ()

-- ** unbuffering

-- unbuffers buffered content
unbufferTxLogs :: MonadIO m => Bool -> TxLogs r m -> m ()
unbufferTxLogs onlyWrites txlogs = Foldable.mapM_ (unbufferTxLog onlyWrites) txlogs

unbufferTxLog :: MonadIO m => Bool -> TxLog r m -> m ()
unbufferTxLog onlyWrites txlog = WeakTable.mapMGeneric_ (unbufferDynTxVar' True onlyWrites txlog) $ txLogBuff txlog

unbufferDynTxVar :: MonadIO m => Bool -> Bool -> TxLog r m -> DynTxVar r m -> m ()
unbufferDynTxVar doDirty onlyWrites txlog entry = unbufferDynTxVar' doDirty onlyWrites txlog (dynTxId entry,entry)

unbufferTxVar :: MonadIO m => Bool -> Bool -> TxLog r m -> Unique -> m ()
unbufferTxVar doDirty onlyWrites txlog uid = do
	mb <- WeakTable.lookup (txLogBuff txlog) uid
	case mb of
		Just tvar -> unbufferDynTxVar' doDirty onlyWrites txlog (uid,tvar)
		Nothing -> return ()

unbufferDynTxVar' :: MonadIO m => Bool -> Bool -> TxLog r m -> (Unique,DynTxVar r m) -> m ()
unbufferDynTxVar' doDirty onlyWrites txlog (uid,entry) = when (onlyWrites <= (dynTxStatus entry == Write)) $ 
		WeakTable.insertWithMkWeak (txLogBuff txlog) (dynTxMkWeak entry) (dynTxId entry) =<< unbufferDynTxVar'' doDirty txlog onlyWrites entry

-- we need to clear the dependencies of unbuffered variables
-- buffered dependents are always preserved, but dirtied in case we discard the buffered data
unbufferDynTxVar'' :: MonadIO m => Bool -> TxLog r m -> Bool -> DynTxVar r m -> m (DynTxVar r m)
unbufferDynTxVar'' doDirty txlog onlyWrites tvar@(DynTxU _ _ u New) = return tvar
unbufferDynTxVar'' doDirty txlog onlyWrites tvar@(DynTxU _ _ u stat) = if (if onlyWrites then (==Write) else isEvalOrWrite) stat
	then do
		dependents <- dynTxVarDependents tvar
		dynTxVarDependencies tvar >>= maybe (return ()) (clearDependenciesTx False)
		when doDirty $ dirtyBufferedDynTxVar txlog tvar
		return $ DynTxU Nothing (Just dependents) u $ Read True
	else return tvar
unbufferDynTxVar'' doDirty txlog onlyWrites tvar@(DynTxM _ _ m stat) = if (if onlyWrites then (==Write) else isEvalOrWrite) stat
	then do
		dependents <- dynTxVarDependents tvar
		when doDirty $ dirtyBufferedDynTxVar txlog tvar
		return $ DynTxM Nothing (Just dependents) m $ Read True
	else return tvar

{-# INLINE clearDependenciesTx #-}
-- clear only buffered dependencies
clearDependenciesTx :: (MonadRef r m,MonadIO m) => Bool -> TxDependencies r m -> m ()
clearDependenciesTx doAll = \r -> readRef r >>= Foldable.mapM_ clearDependency where
	clearDependency (d,w) = if doAll
		then liftIO $ Weak.finalize w
		else do
			isOriginal <- readRef (flagTxW d)
			unless isOriginal $ liftIO $ Weak.finalize w
	
-- merges a nested txlog with a parent txlog, but treating nested writes as reads (so that a parent retry will still wait on them)
-- discards @onlyWrites@ or both evals and writes
extendTxLog :: MonadIO m => Bool -> TxLog r m -> TxLog r m -> m ()
extendTxLog onlyWrites txlog1 txlog2 = do
	let updEntry (uid,entry) = do
		entry1 <- unbufferDynTxVar'' True txlog1 onlyWrites entry
		mb <- liftIO $ WeakTable.lookup (txLogBuff txlog2) uid
		case mb of
			Nothing -> liftIO $ WeakTable.insertWithMkWeak (txLogBuff txlog2) (dynTxMkWeak entry1) uid entry1
			Just entry2 -> mergeDynTxVars entry1 entry2 >>= liftIO . WeakTable.insertWithMkWeak (txLogBuff txlog2) (dynTxMkWeak entry1) uid
	WeakTable.mapMGeneric_ updEntry (txLogBuff txlog1)
  where
	-- merges dependents for @Read@ children
	-- dirty dependencies for @New@ children
	mergeDynTxVars :: MonadIO m => DynTxVar r m -> DynTxVar r m -> m (DynTxVar r m)
	mergeDynTxVars entry1@(DynTxM Nothing txdeps1 m (Read b1)) entry2@(DynTxM Nothing txdeps2 _ (Read b2)) = do
		txdeps <- mergeTxDependents txdeps1 txdeps2
		return $ DynTxM Nothing txdeps m $ Read (max b1 b2)
	mergeDynTxVars entry1@(DynTxM Nothing txdeps1 m (Read b1)) entry2 = do
		txdeps2 <- liftM Just $ dynTxVarDependents entry2
		mergeTxDependents txdeps1 txdeps2
		return entry2
		
	mergeDynTxVars entry1@(DynTxU Nothing txdeps1 m (Read b1)) entry2@(DynTxU Nothing txdeps2 _ (Read b2)) = do
		txdeps <- mergeTxDependents txdeps1 txdeps2
		return $ DynTxU Nothing txdeps m $ Read (max b2 b2)
	mergeDynTxVars entry1@(DynTxU Nothing txdeps1 m (Read b1)) entry2 = do
		txdeps2 <- liftM Just $ dynTxVarDependents entry2
		mergeTxDependents txdeps1 txdeps2
		return entry2
	
	mergeDynTxVars x1 x2 = return x1
	
	mergeTxDependents :: MonadIO m => Maybe (TxDependents r m) -> Maybe (TxDependents r m) -> m (Maybe (TxDependents r m))
	mergeTxDependents Nothing txdeps2 = return txdeps2
	mergeTxDependents txdeps1 Nothing = return txdeps1
	mergeTxDependents (Just txdeps1) (Just txdeps2) = unionWithKey dependenciesTxW txdeps2 txdeps1 >> return (Just txdeps2)

dynTxStatus :: DynTxVar r m -> TxStatus
dynTxStatus (DynTxU _ _ _ s) = s
dynTxStatus (DynTxM _ _ _ s) = s

dynTxMeta :: DynTxVar r m -> TxNodeMeta r m
dynTxMeta (DynTxU _ _ u _) = metaTxU u
dynTxMeta (DynTxM _ _ m _) = metaTxM m

dynTxId :: DynTxVar r m -> Unique
dynTxId = idTxNM . dynTxMeta

dynTxMkWeak :: DynTxVar r m -> MkWeak
dynTxMkWeak (DynTxU _ _ u _) = MkWeak $ mkWeakRefKey $ dataTxU u
dynTxMkWeak (DynTxM _ _ m _) = MkWeak $ mkWeakRefKey $ dataTxM m

-- gets a buffered dependents set, generating a new Eval if needed
getTxDependents :: MonadIO m => TxLogs r m -> TxNodeMeta r m -> TxStatus -> m (TxDependents r m)
getTxDependents tbl meta status = bufferTxNM meta status tbl >>= dynTxVarDependents

getTxDependencies :: (MonadIO m,MonadRef r m) => TxLogs r m -> TxNodeMeta r m -> TxStatus -> m (TxDependencies r m)
getTxDependencies tbl meta status = liftM fromJust $ bufferTxNM meta status tbl >>= dynTxVarDependencies

copyRef r = readRef r >>= newRef

proxyTxAdapton = Proxy :: Proxy TxAdapton

instance DeepTypeable TxM where
	typeTree _ = MkTypeTree (mkName "Control.Monad.Transactional.TxAdapton.Types.TxM") [] []

instance (DeepTypeable l,DeepTypeable inc,DeepTypeable r,DeepTypeable m,DeepTypeable a) => DeepTypeable (TxM l inc r m a) where
	typeTree (_ :: Proxy (TxM l inc r m a)) = MkTypeTree (mkName "Control.Monad.Transactional.TxAdapton.Types.TxM") args [MkConTree (mkName "Control.Monad.Transactional.TxAdapton.mod") [typeTree (Proxy::Proxy a)]]
		where args = [typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy r),typeTree (Proxy::Proxy m),typeTree (Proxy::Proxy a)]

instance DeepTypeable TxU where
	typeTree _ = MkTypeTree (mkName "Control.Monad.Transactional.TxAdapton.Types.TxU") [] []

instance (DeepTypeable l,DeepTypeable inc,DeepTypeable r,DeepTypeable m,DeepTypeable a) => DeepTypeable (TxU l inc r m a) where
	typeTree (_ :: Proxy (TxU l inc r m a)) = MkTypeTree (mkName "Control.Monad.Transactional.TxAdapton.Types.TxU") args [MkConTree (mkName "Control.Monad.Transactional.TxAdapton.thunk") [typeTree (Proxy::Proxy a)]]
		where args = [typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy r),typeTree (Proxy::Proxy m),typeTree (Proxy::Proxy a)]

type STxAdaptonM = STxM TxAdapton IORef IO

-- a channel for synchronous stdout debugging messages
TH.declareChan "debugChan" [t| String |]

debugTx :: TxLayer l r m => String -> l TxAdapton r m ()
--debugTx str = return ()
--debugTx str = inL $ liftIO $ putStrLn str
debugTx str = do
	time <- readTxTime
	threadid <- inL $ liftIO $ myThreadId
	inL $ liftIO $ writeChan debugChan $ "{"++show threadid ++"}["++show time++"] " ++ str
--	inL $ liftIO $ putStrLn $ "{"++show threadid ++"}["++show time++"] " ++ str

debugTx' :: MonadIO m => String -> m ()
--debugTx' str = return ()
--debugTx' str = liftIO $ putStrLn str
debugTx' str = do
	threadid <- liftIO $ myThreadId
	liftIO $ writeChan debugChan $ "{"++ show threadid ++ "}" ++ str
--	liftIO $ putStrLn $ "{"++ show threadid ++ "}" ++ str

-- makes sure to empty the buffer before killing the debugger thread
debugger :: IO ()
debugger = flip finally (getChanContents debugChan >>= Control.Monad.mapM_ putStrLn) $ do
    readChan debugChan >>= putStrLn
    debugger

readTxStack :: TxLayer l r m => l TxAdapton r m (TxCallStack r m)
readTxStack = liftM (\(x :!: y :!: z) -> y) $ Reader.ask

readTxLog :: TxLayer l r m => l TxAdapton r m (TxLogs r m)
readTxLog = liftM (\(x :!: y :!: z) -> z) $ Reader.ask

readTxTime :: TxLayer l r m => l TxAdapton r m (UTCTime)
readTxTime = liftM (\(x :!: y :!: z) -> x) Reader.ask >>= inL . readRef



