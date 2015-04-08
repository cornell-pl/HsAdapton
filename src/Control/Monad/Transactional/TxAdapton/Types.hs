{-# LANGUAGE CPP, TypeFamilies, TupleSections, StandaloneDeriving, BangPatterns, EmptyDataDecls, FlexibleContexts, TypeOperators, ConstraintKinds, MagicHash, ViewPatterns, KindSignatures, GADTs, ScopedTypeVariables, DeriveDataTypeable, TemplateHaskell #-}

module Control.Monad.Transactional.TxAdapton.Types where

import Data.Time.Clock
import Control.Monad.Incremental
import Control.Concurrent
import Control.Monad.Transactional
import qualified Control.Concurrent.Map as CMap
import Control.Monad.Incremental.Adapton.Types
import Data.Concurrent.Deque.Class as Queue
import Data.Concurrent.Deque.Reference.DequeInstance
import Data.Time
import Control.Concurrent.Chan
import System.IO.Unsafe
import Control.Monad.Fix
import Data.Global.TH as TH
import qualified Control.Concurrent.STM as STM
import Safe
import System.Mem

import Data.Monoid
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
import Control.Monad.Catch as Catch
import Control.Concurrent.Lock as Lock
import Control.Concurrent.MVar
import qualified System.Mem.Concurrent.WeakMap as CWeakMap
import Control.Monad
import System.Mem.WeakKey
import System.Mem.MemoTable
import Data.Hashable
import System.Mem.WeakTable as WeakTable
import Data.Set as Set
import Data.List as List
import System.Mem.WeakMap as WeakMap
import Debug

type instance IncK TxAdapton a = (Typeable a,Eq a,Show a)

readTxStack :: TxLayer l r m => l TxAdapton r m (TxCallStack r m)
readTxStack = liftM (\(x :!: y :!: z) -> y) $ Reader.ask

readTxLog :: TxLayer l r m => l TxAdapton r m (TxLogs r m)
readTxLog = liftM (\(x :!: y :!: z) -> z) $ Reader.ask

readTxTime :: TxLayer l r m => l TxAdapton r m (UTCTime)
readTxTime = liftM (\(x :!: y :!: z) -> x) Reader.ask >>= inL . readRef

-- a tx wakeup pair (update buffered data,release locks)
--type TxWake m = (m (),IO ())
--type TxWake = IO ()
type Wakes = Map Unique Lock

type TxCreator r m = WTxNodeMeta r m
type WTxNodeMeta r m = Weak (TxNodeMeta r m)

#ifndef CSHF
type TLock = STM.TMVar ()
#endif
#ifdef CSHF
type TLock = MVar ()
#endif

newtype TxStatus = TxStatus (TxStatusVar,Bool) deriving (Eq,Show,Typeable)
txStatusVar (TxStatus (x,y)) = x
tsStatusDependents (TxStatus (x,y)) = y

data TxStatusVar = Read | Eval | Write | New Bool deriving (Eq,Show,Typeable)
-- read does not copy the original data, and just logs that it has been accessed
-- False: no buffered dependents
-- True: buffered dependents
-- eval is a non-conflicting write; we compute data that is consistent with the original state
-- write is a write to a modifiable or dirtying of a thunk
-- new is for new transaction allocations; the boolean denotes whether the new variable depends on a written variable or not

instance Monoid TxStatusVar where
	mempty = Read
	mappend Read y = y
	mappend Eval Read = Eval
	mappend Eval y = y
	mappend Write Read = Write
	mappend Write Eval = Write
	mappend Write Write = Write
	mappend Write (New j) = New True
	mappend (New i) Read = New i
	mappend (New i) Eval = New i
	mappend (New i) Write = New True
	mappend (New i) (New j) = New (max i j)

instance Monoid TxStatus where
	mempty = TxStatus (mempty,False)
	mappend (TxStatus (m1,d1)) (TxStatus (m2,d2)) = TxStatus (m1 `mappend` m2,max d1 d2)

isNew :: TxStatus -> Bool
isNew (TxStatus (New _,_)) = True
isNew _ = False

isWrite :: TxStatus -> Bool
isWrite (TxStatus (Write,_)) = True
isWrite _ = False

isDependentsWrite :: TxStatus -> Bool
isDependentsWrite (TxStatus (_,b)) = b

type TxBuffMemoTable r m k b = MemoTable k (MkWeak,TxU Inside TxAdapton r m b)
-- function that unmemoizes buffered entries from the buffered table of a specific txlog
type TxUnmemo r m = TxLog r m -> IO ()

type TxDependents (r :: * -> *) (m :: * -> *) = WeakMap Unique (TxDependent r m)
type TxDependent r m = TxDependency r m

newtype TxM (l :: * -> (* -> *) -> (* -> *) -> * -> *) inc (r :: * -> *) (m :: * -> *) a = TxM (
		r a -- a constant value
	,	TxNodeMeta r m
	) deriving (Typeable)

newtype TxU (l :: * -> (* -> *) -> (* -> *) -> * -> *) inc (r :: * -> *) (m :: * -> *) a = TxU (
		r (TxUData l inc r m a) -- data (stored in a MVar because it is accessed concurrently by multiple threads)
	,	TxNodeMeta r m		-- metadata
	) deriving (Typeable)

data TxUData (l :: * -> (* -> *) -> (* -> *) -> * -> *) inc (r :: * -> *) (m :: * -> *) a =
	  TxValue !UBool !a !(l inc r m a) !(TxDependencies r m) -- a thunk that has been previously evaluated; invariant: the node is dirty if any of its dependencies is dirty; the dirty flag is just an optimization
	| TxThunk !(l inc r m a) -- a thunk that has never been evaluated
	| TxConst !a -- a constant value
  deriving Typeable

newtype TxNodeMeta (r :: * -> *) (m :: * -> *) = TxNodeMeta (
		Unique
	,	TxDependents r m -- list of dependents; we purge the list at finalization time
	,	(TxLogs r m -> m ()) -- function that dirties the corresponding value (thunks only)
	,	(TxLogs r m -> m ()) -- function that forgets all the old cached thunk data (thunks only)
	,   (TxStatus -> TxLogs r m -> m (DynTxVar r m)) -- function that writes a buffered copy of the original data to a transaction log
	,   Maybe (TxCreator r m) -- the parent thunk under which the reference was created (modifiables only)
	,   WaitQueue -- a sequence of wake-up actions for txs that are waiting on further updates to this node (modifiables only)
	,   TLock -- a lock for writes to this variable
#ifdef CSHF 
	,	MVar (Map ThreadId Bool) -- a set of threads that depend on this variable (True=isWriteOrNewTrue, False = otherwise)
#endif
	)

-- (a new starttime,repairing actions over an invalid environment)
type Repair = (UTCTime,Set Unique)
type TxRepair r m = (UTCTime,TxRepair' r m)
type InvalidTxRepair r m = (UTCTime,Outside TxAdapton r m ())
type TxRepair' r m = TxLog r m -> Outside TxAdapton r m ()

-- a wait queue of tx locks and environments
-- thread-safe, because a retrying thread that only reads a variable places itself in the queue (and reads are not locked)
type WaitQueue = Deque Threadsafe Threadsafe SingleEnd SingleEnd Grow Safe Lock

-- a reference holding buffered data and maybe a pointer to the original dependencies reference (either the original references (keeping them alive) or a weak pointer (not keeping them alive))
-- buffered dependends EXTEND the original dependents
type BuffTxUData l inc r m a = (TxUData l inc r m a , (Either (TxDependencies r m) (Weak (TxDependencies r m))))

newtype BuffTxU (l :: * -> (* -> *) -> (* -> *) -> * -> *) (r :: * -> *) (m :: * -> *) a = BuffTxU { unBuffTxU :: r (BuffTxUData l TxAdapton r m a) } deriving Typeable

newtype BuffTxM (l :: * -> (* -> *) -> (* -> *) -> * -> *) (r :: * -> *) (m :: * -> *) a = BuffTxM { unBuffTxM :: a } deriving Typeable

type TxDependencies r m = r (SList (TxDependency r m,Weak (TxDependency r m)))
-- bool says whether the dependency is original or buffered
newtype TxDependency (r :: * -> *) (m :: * -> *) = TxDependency (
		TxNodeMeta r m -- the metadata of the source node
	,	r Bool -- dirty flag
	,	Inside TxAdapton r m (Bool,TxStatus) -- a checking condition with the previously seen value of the source node
	,	TxNodeMeta r m -- the metadata of the target node
	,   r Bool -- original/buffered flag
	,	MkWeak -- parent dependencies list
	)
deriving instance Typeable TxDependency

-- a buffered thunk and an optional global thunk
-- we use weak pointers to the original data because we don't want to keep original thunks alive unecessarily; if the original data dies then the buffered data dies
-- read False = Nothing Nothing original
-- read True = Nothing (Just $ buffered dependents) original
-- eval = Just (Just $ buffered dependents)original
-- write = Just (Just $ buffered dependents) original
-- new = Nothing Nothing new
-- buffered dependents are ALWAYS seen as an overlay over the original dependents
data DynTxVar r m where
	DynTxU :: (IncK TxAdapton a,TxLayer l r m) => Maybe (BuffTxU l r m a) -> Maybe (TxDependents r m) -> TxU l TxAdapton r m a -> TxStatus -> DynTxVar r m
	DynTxM :: (IncK TxAdapton a,TxLayer l r m) => Maybe (BuffTxM l r m a) -> Maybe (TxDependents r m) -> TxM l TxAdapton r m a -> TxStatus -> DynTxVar r m
--	DynTxL :: Maybe (BuffTxL l inc r m a) -> TxL l inc r m a -> TxStatus -> DynTxVar r m

-- we use a lock-free map since multiple threads may be accessing it to register/unregister new txlogs
-- a persistent memo table and a family of buffered (transaction-local) memo tables indexed by a buffered tx log
-- note that added entries to the memotable are always New (hence we have TxU and not BuffTxU in buffered tables)
type TxMemoTable r m k b = (MemoTable k (TxU Inside TxAdapton r m b) :!: CMap.Map (TxLog r m) (TxBuffMemoTable r m k b))

data DynTxMemoTable r m where
	DynTxMemoTable :: (Eq k,Hashable k) => TxMemoTable r m k b -> DynTxMemoTable r m


-- an IORef is just fine, because the stack is transaction-local
-- the dependencies references in the stack are always buffered copies
type TxCallStack (r :: * -> *) (m :: * -> *) = IORef (TxCallStack' r m)
type TxCallStack' (r :: * -> *) (m :: * -> *) = SList (TxStackElement r m)

type TxStackElement (r :: * -> *) (m :: * -> *) = (TxNodeMeta r m :!: SMaybe (TxDependencies r m) :!: r TxStatus)

-- | A table mapping variable identifiers to buffered content, together with a list of memo tables (one per memoized function) that contain transient entries for this log
-- has a unique identifier so that it can be a key in a table of buffered memotables
-- this list is local to the tx (so no concurrent handling is needed) and used to know which tables to merge with the persistent memo tables at commit time
newtype TxLog r m = TxLog (Unique :!: WeakTable Unique (DynTxVar r m) :!: IORef [DynTxMemoTable r m]) deriving Typeable
type TxLogs r m = SList (TxLog r m)

instance WeakKey (TxLog r m) where
	mkWeakKey (TxLog (_ :!: tbl :!: memos)) = mkWeakKey memos

-- a tx environment contains (a tx start time,a callstack,a list of nested logs -- to support nested transactions)
type TxEnv r m = (r UTCTime :!: TxCallStack r m :!: TxLogs r m)


data TxAdapton deriving Typeable

type TxLayer l r m = (Typeable l,Typeable m,Typeable r,MonadMask (l TxAdapton r m),MonadMask m,MonadCatch m,Layer l TxAdapton r m,MonadReader (TxEnv r m) (l TxAdapton r m),MonadIO m,MonadRef r (l TxAdapton r m))

-- a channel for synchronous stdout debugging messages
TH.declareChan "debugChan" [t| String |]

-- makes sure to empty the buffer before killing the debugger thread
debugger :: IO ()
debugger = flip finally (getChanContents debugChan >>= Control.Monad.mapM_ putStrLn) $ do
    readChan debugChan >>= putStrLn
    debugger

debugTx,debugTx2 :: TxLayer l r m => String -> l TxAdapton r m ()
--debugTx str = return ()
--debugTx str = inL $ liftIO $ putStrLn str
debugTx str = do
	time <- readTxTime
	threadid <- inL $ liftIO $ myThreadId
	inL $ liftIO $ writeChan debugChan $ "{"++show threadid ++"}["++show time++"] " ++ str
--	inL $ liftIO $ putStrLn $ "{"++show threadid ++"}["++show time++"] " ++ str
--debugTx2 = debugTx
debugTx2 msg = return ()

debugTx',debugTx2' :: MonadIO m => String -> m ()
--debugTx' str = return ()
--debugTx' str = liftIO $ putStrLn str
debugTx' str = do
	threadid <- liftIO $ myThreadId
	liftIO $ writeChan debugChan $ "{"++ show threadid ++ "}" ++ str
--	liftIO $ putStrLn $ "{"++ show threadid ++ "}" ++ str
--debugTx2' = debugTx'
debugTx2' msg = return ()

$( derive makeDeepTypeableAbstract ''TxAdapton )

--isReadTrueOrEval (Read True) = True
--isReadTrueOrEval Eval = True
--isReadTrueOrEval _ = False

isEval (TxStatus (Eval,_)) = True
isEval _ = False

isEvalOrWrite (TxStatus (Eval,i)) = Just i
isEvalOrWrite (TxStatus (Write,i)) = Just i
isEvalOrWrite _ = Nothing

isReadOrEvalOrWrite (TxStatus (Read,i)) = Just i
isReadOrEvalOrWrite (TxStatus (Eval,i)) = Just i
isReadOrEvalOrWrite (TxStatus (Write,i)) = Just i
isReadOrEvalOrWrite _ = Nothing

instance Eq (TxLog r m) where
	(TxLog (id1 :!: _ :!: _)) == (TxLog (id2 :!: _ :!: _)) = id1 == id2
instance Hashable (TxLog r m) where
	hashWithSalt i (TxLog (id1 :!: _ :!: _)) = hashWithSalt i id1

finalizeTxLog :: MonadIO m => TxLog r m -> m ()
finalizeTxLog txlog = do
	liftIO $ WeakTable.finalize (txLogBuff txlog)

txLogId (TxLog (x :!: y :!: z)) = x
txLogBuff (TxLog (x :!: y :!: z)) = y
txLogMemo (TxLog (x :!: y :!: z)) = z

emptyTxLog :: IO (TxLog r m)
emptyTxLog = do
	uid <- newUnique
	memos <- newIORef []
	buff <- WeakTable.new
	return $ TxLog (uid :!: buff :!: memos)

#ifndef CSHF
newTLockIO = STM.newTMVarIO ()
tryWaitTLock :: TLock -> STM.STM Bool
tryWaitTLock mv = do
	mb <- STM.tryTakeTMVar mv
	case mb of
		Nothing -> return False
		Just v -> STM.putTMVar mv v >> return True
tryWaitTLocks :: Foldable t => t TLock -> STM.STM Bool
tryWaitTLocks = Foldable.foldlM (\b lck -> liftM (b&&) $ tryWaitTLock lck) True
waitOrRetryTLock :: TLock -> STM.STM ()
waitOrRetryTLock mv = STM.tryTakeTMVar mv >>= maybe STM.retry (STM.putTMVar mv)
acquireOrRetryTLock :: TLock -> STM.STM ()
acquireOrRetryTLock = STM.takeTMVar
tryAcquireTLock :: TLock -> STM.STM Bool
tryAcquireTLock = liftM isJust . STM.tryTakeTMVar
releaseTLock mv = do
	b <- STM.tryPutTMVar mv ()
	when (not b) $ error "Control.Concurrent.Lock.release: Can't release unlocked Lock!"
tryAcquireTLocks :: Foldable t => t TLock -> STM.STM Bool
tryAcquireTLocks lcks = do
	(b,acquired) <- Foldable.foldlM (\(b,lcks) lck -> liftM (\x -> if x then (b,lck:lcks) else (False,lcks)) $ tryAcquireTLock lck) (True,[]) lcks
	unless b $ Foldable.mapM_ releaseTLock acquired
	return b
#endif
#ifdef CSHF
newTLockIO = newMVar ()
tryWaitTLock :: MonadIO m => TLock -> m Bool
tryWaitTLock mv = liftIO $ do
	mb <- tryTakeMVar mv
	case mb of
		Nothing -> return False
		Just v -> putMVar mv v >> return True
tryWaitTLocks :: (MonadIO m,Foldable t) => t TLock -> m Bool
tryWaitTLocks = Foldable.foldlM (\b lck -> liftM (b&&) $ tryWaitTLock lck) True
tryAcquireTLock :: MonadIO m => TLock -> m Bool
tryAcquireTLock = liftIO . liftM isJust . tryTakeMVar
releaseTLock :: MonadIO m => TLock -> m ()
releaseTLock mv = liftIO $ do
	b <- tryPutMVar mv ()
	when (not b) $ error "Control.Concurrent.Lock.release: Can't release unlocked Lock!"
tryAcquireTLocks :: MonadIO m => [(Unique,TLock)] -> m Bool
tryAcquireTLocks xs = do
	(b,acquired) <- tryAcquireTLocks' xs
	unless b $ releaseTLocks acquired
	return b
tryAcquireTLocks' :: MonadIO m => [(Unique,TLock)] -> m (Bool,[(Unique,TLock)])
tryAcquireTLocks' [] = return (True,[])
tryAcquireTLocks' ((uid,lck):xs) = do
	b <- tryAcquireTLock lck
	if b
		then do
--			debugTx' $ "acquired " ++ show uid
			(b,lcks) <- tryAcquireTLocks' xs
			return (b,(uid,lck):lcks)
		else return (False,[])
releaseTLocks :: MonadIO m => [(Unique,TLock)] -> m ()
releaseTLocks = Prelude.mapM_ (\(uid,lck) -> releaseTLock lck {->> debugTx' ("released " ++ show uid)-})
#endif

-- eval locks: to force concurrent writes to wait until the eval is written to the variable
writeLock :: TxStatus -> Int
writeLock (TxStatus (Read,False)) = 1 -- no lock
writeLock (TxStatus (New False,_)) = 1 -- no lock
writeLock (TxStatus (Read,True)) = 2 -- optional lock
writeLock (TxStatus (Eval,_)) = 2 -- optional lock
writeLock (TxStatus (Write,_)) = 3 -- mandatory lock
writeLock (TxStatus (New True,_)) = 3 -- mandatory lock

-- returns a set of locks for variables to which a txlog intends to write (@Eval@ and Write@) and to read from (@Read@)
txLocks :: TxLogs r m -> IO (Map Unique (TLock,TxStatus))
txLocks = Foldable.foldrM getLocks Map.empty where
	getLocks :: TxLog r m -> Map Unique (TLock,TxStatus) -> IO (Map Unique (TLock,TxStatus))
	getLocks txlog lcks = WeakTable.foldM add lcks (txLogBuff txlog) where
		add xs (uid,tvar) = return $ Map.insertWith merge uid (dynTxVarLock tvar,dynTxStatus tvar) xs
	merge (lck,b1) (_,b2) = (lck,mappend b1 b2)

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

emptyTxUnmemo txlog = return ()
joinTxUnmemos f g = \txlog -> f txlog >> g txlog

-- commits dependencies
commitDependenciesTx :: (MonadIO m,WeakRef r,MonadRef r m) => TxDependencies r m -> (Either (TxDependencies r m) (Weak (TxDependencies r m))) -> m ()
commitDependenciesTx dependencies (Left oridependencies) = do
	-- the old dependencies reference lives as long as the new dependencies reference; because some dependencies may still point to the old dependencies reference
	liftIO $ mkWeakRefKey dependencies oridependencies Nothing >> return ()
commitDependenciesTx dependencies (Right w) = do -- we finalize all the old dependencies, to prevent old original dependencies to remain buffered
	mb <- liftIO $ Weak.deRefWeak w
	case mb of
		Nothing -> return ()
		Just oridependencies -> clearDependenciesTx True oridependencies			

markOriginalDependenciesTx :: MonadRef r m => TxDependencies r m -> m ()
markOriginalDependenciesTx dependencies = readRef dependencies >>= Foldable.mapM_ markDependencyTx where
	markDependencyTx (d,w) = writeRef (flagTxW d) True

srcMetaTxW (TxDependency (srcMeta,dirty,check,tgtMeta,flag,dependencies)) = srcMeta
dirtyTxW (TxDependency (srcMeta,dirty,check,tgtMeta,flag,dependencies)) = dirty
checkTxW (TxDependency (srcMeta,dirty,check,tgtMeta,flag,dependencies)) = check
tgtMetaTxW (TxDependency (srcMeta,dirty,check,tgtMeta,flag,dependencies)) = tgtMeta
flagTxW (TxDependency (srcMeta,dirty,check,tgtMeta,flag,dependencies)) = flag
dependenciesTxW (TxDependency (srcMeta,dirty,check,tgtMeta,flag,dependencies)) = dependencies

dataTxM (TxM (v,meta)) = v
metaTxM (TxM (v,meta)) = meta

instance Eq (TxM l inc r m a) where
	t1 == t2 = idTxNM (metaTxM t1) == idTxNM (metaTxM t2)

dataTxU (TxU (dta,meta)) = dta
metaTxU (TxU (dta,meta)) = meta

instance Eq (TxU l inc r m a) where
	t1 == t2 = idTxNM (metaTxU t1) == idTxNM (metaTxU t2)

#ifndef CSHF
idTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck)) = uid
dependentsTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck)) = deps
dirtyTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck)) = dirty
forgetTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck)) = forget
bufferTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck)) = buffer
creatorTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck)) = creator
waitTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck)) = wait
lockTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck)) = lck
#endif

#ifdef CSHF
idTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck,n)) = uid
dependentsTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck,n)) = deps
dirtyTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck,n)) = dirty
forgetTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck,n)) = forget
bufferTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck,n)) = buffer
creatorTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck,n)) = creator
waitTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck,n)) = wait
lockTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck,n)) = lck
notifiesTxNM (TxNodeMeta (uid,deps,dirty,forget,buffer,creator,wait,lck,n)) = n
#endif

-- | registers a new wait on modifications of a node when a lock is provided
enqueueWait :: MonadIO m => Lock -> TxNodeMeta r m -> m ()
enqueueWait lck meta = liftIO $ pushL (waitTxNM meta) lck

-- the same tx may depend on various modifiables, so we need to account for multiple wakeups
tryRelease :: Lock -> IO ()
tryRelease lck = do
	isLocked <- locked lck
	if isLocked then release lck else return ()

dynTxVarLock :: DynTxVar r m -> TLock
dynTxVarLock (DynTxM _ _ m _) = lockTxNM $ metaTxM m
dynTxVarLock (DynTxU _ _ u _) = lockTxNM $ metaTxU u

dynVarTxCreator :: DynTxVar r m -> Maybe (TxCreator r m)
dynVarTxCreator (DynTxM _ _ m _) = creatorTxNM $ metaTxM m
dynVarTxCreator (DynTxU _ _ u _) = creatorTxNM $ metaTxU u

-- merge the original dependents on top of the buffered ones (without overriding buffered content)
dynTxVarDependents :: MonadIO m => DynTxVar r m -> m (TxDependents r m,Map Unique (TxDependent r m))
dynTxVarDependents (DynTxM Nothing Nothing m (TxStatus (New _,_))) = do
	deps <- WeakMap.toMap' $ dependentsTxNM $ metaTxM m
	return (dependentsTxNM $ metaTxM m,deps)
dynTxVarDependents (DynTxU Nothing Nothing u (TxStatus (New _,_))) = do
	deps <- WeakMap.toMap' $ dependentsTxNM $ metaTxU u
	return (dependentsTxNM $ metaTxU u,deps)
dynTxVarDependents (DynTxM _ mbtxdeps m _) = do
	case mbtxdeps of
		Just txdeps -> do
			xs <- WeakMap.toMap' txdeps
			ys <- WeakMap.toMap' (dependentsTxNM $ metaTxM m)
			return (txdeps,xs `Map.union` ys)
		Nothing -> do
			deps <- WeakMap.toMap' $ dependentsTxNM $ metaTxM m
			return (error "no buffered dependents",deps)
dynTxVarDependents (DynTxU _ mbtxdeps u _) = do
	case mbtxdeps of
		Just txdeps -> do
			xs <- WeakMap.toMap' txdeps
			ys <- WeakMap.toMap' (dependentsTxNM $ metaTxU u)
			return (txdeps,xs `Map.union` ys)
		Nothing -> do
			deps <- WeakMap.toMap' $ dependentsTxNM $ metaTxU u
			return (error "no buffered dependents",deps)

-- we don't overlap the buffered dependencies
--extendTxDependents :: (MonadIO m,MonadRef r m) => TxDependents r m -> TxDependents r m -> m ()
--extendTxDependents = WeakMap.extendWithKey dependenciesTxW --WeakMap.mergeWithKey (\oldd newd -> readRef (flagTxW oldd)) dependenciesTxW

-- returns only the buffered dependents of a buffered variable
dynTxVarBufferedDependents :: MonadIO m => DynTxVar r m -> (Maybe (TxDependents r m))
dynTxVarBufferedDependents (DynTxM Nothing Nothing m (TxStatus (New _,_))) = Just $ dependentsTxNM $ metaTxM m
dynTxVarBufferedDependents (DynTxU Nothing Nothing u (TxStatus (New _,_))) = Just $ dependentsTxNM $ metaTxU u
dynTxVarBufferedDependents (DynTxM _ mbtxdeps m _) = mbtxdeps
dynTxVarBufferedDependents (DynTxU _ mbtxdeps u _) = mbtxdeps

dynTxVarDependencies :: (MonadIO m,MonadRef r m) => DynTxVar r m -> m (Maybe (TxDependencies r m))
dynTxVarDependencies (DynTxU (Just (BuffTxU buff_dta)) _ _ _) = do
	(dta,_) <- readRef buff_dta
	case dta of
		TxValue dirty value force dependencies -> return $ Just dependencies
		otherwise -> return Nothing
dynTxVarDependencies (DynTxU _ _ u _) = do
	(readRef $ dataTxU u) >>= \dta -> case dta of
		TxValue dirty value force dependencies -> return $ Just dependencies
		otherwise -> return Nothing
dynTxVarDependencies _ = return Nothing

bufferTxDependents :: MonadIO m => Maybe (TxDependents r m) -> Bool -> m (Maybe (TxDependents r m))
bufferTxDependents Nothing True = liftM Just $ liftIO $ WeakMap.new'
bufferTxDependents deps _ = return deps

#ifdef CSHF
addTxNotify :: MonadIO m => Maybe TxStatus -> TxStatus -> TxLogs r m -> TxNodeMeta r m -> m ()
addTxNotify old new txlogs meta = liftIO $ do
	case diffTxStatus old new of
		Nothing -> return ()
		Just isWriteOrNewTrue -> do
			tid <- myThreadId
			modifyMVarMasked_ (notifiesTxNM meta) (return . Map.insert tid isWriteOrNewTrue)
  where
	diffTxStatus Nothing st2 = Just (isWriteOrNewTrue st2)
	diffTxStatus (Just st1) st2 = if (isWriteOrNewTrue st1 && isWriteOrNewTrue st2) then Nothing else Just (isWriteOrNewTrue st2)
#endif

-- stores a modifiable in a transactional buffer with a minimal status and returns a buffered copy
-- Read = an entry is enough
-- Eval = a transaction-consistent entry is enough
-- Write = creates a new or inconsistent entry
bufferTxM :: (IncK TxAdapton a,TxLayer l r m,MonadRef r m,MonadIO m) => TxM l TxAdapton r m a -> TxStatus -> TxLogs r m -> m (DynTxVar r m)
bufferTxM m st@(TxStatus (Read,i)) txlogs = doBlock $ do
	let !idm = idTxNM $ metaTxM m
	mb <- liftIO $ findTxLogEntry txlogs idm

	case mb of
		Just (DynTxM Nothing Nothing u status@(TxStatus (New _,j))) -> do
			let !tvar = DynTxM Nothing Nothing m $ status `mappend` st
			unless (i == j) $ liftIO $ addTxLogEntry txlogs idm tvar
#ifdef CSHF
			addTxNotify (Just status) st txlogs (metaTxM m)
#endif
			return tvar
		Just (DynTxM buff_dta buff_deps m status@(isReadOrEvalOrWrite -> Just j)) -> do
			buff_deps' <- bufferTxDependents buff_deps i
			let !tvar = DynTxM buff_dta buff_deps' m $ status `mappend` st
			unless (isDependentsWrite status == i) $ liftIO $ addTxLogEntry txlogs idm tvar
#ifdef CSHF
			addTxNotify (Just status) st txlogs (metaTxM m)
#endif
			return tvar
		Nothing -> do
			buff_deps <- bufferTxDependents Nothing i
			let !tvar = DynTxM Nothing buff_deps m st
			liftIO $ addTxLogEntry txlogs idm tvar
#ifdef CSHF
			addTxNotify Nothing st txlogs (metaTxM m)
#endif
			return tvar
bufferTxM m st@(TxStatus (Eval,i)) txlogs = doBlock $ do
	let !idm = idTxNM $ metaTxM m
	mb <- liftIO $ findTxLogEntry txlogs idm
	
	let new_entry mbtxdeps i status (mb::Maybe TxStatus) = do
		!buff_value <- (readRef $ dataTxM m)
		mbtxdeps' <- bufferTxDependents mbtxdeps i
		let !tvar = DynTxM (Just $ BuffTxM buff_value) mbtxdeps m status
		liftIO $ addTxLogEntry txlogs idm tvar
#ifdef CSHF
		addTxNotify mb st txlogs (metaTxM m)
#endif
		return tvar

	case mb of
		Just (DynTxM Nothing Nothing m status@(TxStatus (New _,j))) -> do
			let !tvar = DynTxM Nothing Nothing m $ status `mappend` st
			unless (i == j) $ liftIO $ addTxLogEntry txlogs idm tvar
#ifdef CSHF
			addTxNotify (Just status) st txlogs (metaTxM m)
#endif
			return tvar
		Just (DynTxM Nothing mbtxdeps _ status@(TxStatus (Read,_))) -> new_entry mbtxdeps i (st `mappend` status) (Just status)
		Just (DynTxM buff_dta mbtxdeps m status@(isEvalOrWrite -> Just j)) -> do
			mbtxdeps' <- bufferTxDependents mbtxdeps i
			let tvar = DynTxM buff_dta mbtxdeps' m $ st `mappend` status
			unless (isDependentsWrite status == i) $
				liftIO $ addTxLogEntry txlogs idm tvar
#ifdef CSHF
			addTxNotify (Just status) st txlogs (metaTxM m)
#endif
			return tvar
		Nothing -> new_entry Nothing i st Nothing
bufferTxM m status txlogs = changeTxM m Nothing status txlogs

-- changes a modifiable, or just buffers it
changeTxM :: (IncK TxAdapton a,TxLayer l r m,MonadRef r m,MonadIO m) => TxM l TxAdapton r m a -> Maybe a -> TxStatus ->  TxLogs r m -> m (DynTxVar r m)
changeTxM m mbv' st@(isEvalOrWrite -> Just i) txlogs = doBlock $ do
	let !idm = idTxNM $ metaTxM m
	mb <- liftIO $ findTxLogEntry txlogs idm
	
	let new_entry mbtxdeps i status (mb::Maybe TxStatus) = do
		mbtxdeps' <- bufferTxDependents mbtxdeps i
		!old_value <- (readRef $ dataTxM m)
		let !v' = maybe old_value id mbv'
		let !tvar = DynTxM (Just (BuffTxM v')) mbtxdeps' m status
		liftIO $ addTxLogEntry txlogs idm tvar
#ifdef CSHF
		addTxNotify mb st txlogs (metaTxM m)
#endif
		return tvar
		
	case mb of
		Just (DynTxM Nothing mbtxdeps _ status@(TxStatus (Read,i))) -> new_entry mbtxdeps i (status `mappend` st) (Just status)
		Just (DynTxM (Just (BuffTxM buff_value)) buff_deps _ status@(isEvalOrWrite -> Just j)) -> do
			let !v' = maybe (coerce buff_value) id mbv'
			mbtxdeps' <- bufferTxDependents buff_deps i
			let !tvar = DynTxM (Just $ BuffTxM v') mbtxdeps' m $ status `mappend` st
			unless (status == st) $ liftIO $ addTxLogEntry txlogs idm tvar
#ifdef CSHF
			addTxNotify (Just status) st txlogs (metaTxM m)
#endif
			return tvar
		Just tvar@(DynTxM Nothing Nothing m status@(TxStatus (New _,j))) -> case mbv' of
			Nothing -> return tvar
			Just v' -> do
				writeRef (dataTxM m) (coerce v')
				let newstatus = status `mappend` st
				let !tvar = DynTxM Nothing Nothing m newstatus
				when (status == newstatus) $ liftIO $ addTxLogEntry txlogs idm tvar
#ifdef CSHF
				addTxNotify (Just status) st txlogs (metaTxM m)
#endif
				return tvar
		Nothing -> new_entry Nothing False (TxStatus (Write,False)) Nothing
changeTxM m _ status _ = error $ "changeTxM " ++ show status

bufferTxU :: (IncK TxAdapton a,TxLayer l r m,MonadRef r m,MonadIO m) => TxU l TxAdapton r m a -> TxStatus -> TxLogs r m -> m (DynTxVar r m)
bufferTxU u st@(TxStatus (Read,i)) txlogs = doBlock $ do 
	let !idu = idTxNM $ metaTxU u
	mb <- liftIO $ findTxLogEntry txlogs idu
	
	case mb of
		Just (DynTxU Nothing Nothing u status@(TxStatus (New _,j))) -> do
			let !tvar = DynTxU Nothing Nothing u $ status `mappend` st
			unless (i == j) $ liftIO $ addTxLogEntry txlogs idu tvar
#ifdef CSHF
			addTxNotify (Just status) st txlogs (metaTxU u)
#endif
			return tvar
		Just (DynTxU buff_dta buff_deps u status@(isReadOrEvalOrWrite -> Just _)) -> do
			buff_deps' <- bufferTxDependents buff_deps i
			let !tvar = DynTxU buff_dta buff_deps' u $ status `mappend` st
			unless (isDependentsWrite status == i) $ liftIO $ addTxLogEntry txlogs idu tvar
#ifdef CSHF
			addTxNotify (Just status) st txlogs (metaTxU u)
#endif
			return tvar
		Nothing -> do
			buff_deps <- bufferTxDependents Nothing i
			let !tvar = DynTxU Nothing buff_deps u $ TxStatus (Read,i)
			liftIO $ addTxLogEntry txlogs idu tvar
#ifdef CSHF
			addTxNotify Nothing st txlogs (metaTxU u)
#endif
			return tvar
bufferTxU u st@(TxStatus (Eval,i)) txlogs = doBlock $ do
	let !idu = idTxNM $ metaTxU u
	mb <- liftIO $ findTxLogEntry txlogs idu
	
	let new_entry mbtxdeps i status (mb::Maybe TxStatus) = do
		-- the copies point to the original dependencies reference
		buff_deps <- bufferTxDependents mbtxdeps i
		!buff_dta <- (readRef $ dataTxU u) >>= copyTxUData
		let !tvar = DynTxU (Just $ BuffTxU buff_dta) buff_deps u status
		liftIO $ addTxLogEntry txlogs idu tvar
#ifdef CSHF
		addTxNotify mb st txlogs (metaTxU u)
#endif
		return tvar
	
	case mb of
		Just (DynTxU Nothing Nothing u status@(TxStatus (New _,j))) -> do
			let !tvar = DynTxU Nothing Nothing u $ status `mappend` st
			unless (i == j) $ liftIO $ addTxLogEntry txlogs idu tvar
#ifdef CSHF
			addTxNotify (Just status) st txlogs (metaTxU u)
#endif
			return tvar
		Just (DynTxU Nothing mbtxdeps _ status@(TxStatus (Read,_))) -> new_entry mbtxdeps i (status `mappend` st) (Just status)
		Just (DynTxU buff_dta buff_deps u status@(isEvalOrWrite -> Just _)) -> do
			buff_deps' <- bufferTxDependents buff_deps i
			let !tvar = DynTxU buff_dta buff_deps' u $ status `mappend` st
			unless (i == isDependentsWrite status) $ liftIO $ addTxLogEntry txlogs idu tvar
#ifdef CSHF
			addTxNotify (Just status) st txlogs (metaTxU u)
#endif
			return tvar
		Nothing -> new_entry Nothing i st Nothing
bufferTxU u st txlogs = {-debugM "bufferTxU4" $ -}changeTxU u Nothing st txlogs

-- changes a thunk, or just buffers it
changeTxU :: (IncK TxAdapton a,TxLayer l r m,MonadRef r m,MonadIO m) => TxU l TxAdapton r m a -> Maybe (BuffTxUData l TxAdapton r m a -> m (BuffTxUData l TxAdapton r m a)) -> TxStatus -> TxLogs r m -> m (DynTxVar r m)
changeTxU u mbChgDta status@(isEvalOrWrite -> Just i) txlogs = doBlock $ do
	let !idu = idTxNM $ metaTxU u
	mb <- liftIO $ findTxLogEntry txlogs idu
	
	let new_entry mbtxdeps i newstatus (mb::Maybe TxStatus) = do
		mbtxdeps' <- bufferTxDependents mbtxdeps i
		!buff_dta <- (readRef $ dataTxU u) >>= copyTxUData
		mapRefM_ (maybe return id mbChgDta) buff_dta
		let !tvar = DynTxU (Just $ BuffTxU buff_dta) mbtxdeps' u newstatus
		liftIO $ addTxLogEntry txlogs idu tvar
#ifdef CSHF
		addTxNotify mb status txlogs (metaTxU u)
#endif
		return tvar
		
	case mb of
		Just (DynTxU Nothing mbtxdeps _ buff_status@(TxStatus (Read,_))) -> new_entry mbtxdeps i (buff_status `mappend` status) (Just buff_status)
		Just (DynTxU (Just (BuffTxU buff_dta)) buff_deps _ buff_status@(isEvalOrWrite -> Just _)) -> do
			let chg = maybe return id mbChgDta
			mapRefM_ (chg) (coerce buff_dta)
			buff_deps' <- bufferTxDependents buff_deps i
			let !tvar = DynTxU (Just $ BuffTxU $ coerce buff_dta) buff_deps' u $ mappend buff_status status
			-- all changes are logged on the nested transaction's log
			unless (buff_status == status) $ liftIO $ addTxLogEntry txlogs idu tvar
#ifdef CSHF
			addTxNotify (Just buff_status) status txlogs (metaTxU u)
#endif
			return tvar
		Just (DynTxU Nothing Nothing u buff_status@(TxStatus (New _,j))) -> do
			case mbChgDta of
				Nothing -> return ()
				Just chgDta -> liftIO deadWeak >>= \w -> flip mapRefM_ (dataTxU u) (liftM (coerce . Prelude.fst) . chgDta . (,Right w) . coerce) 
			let newstatus = mappend buff_status status
			let !tvar = DynTxU Nothing Nothing u newstatus
			-- all changes are logged on the nested transaction's log
			unless (buff_status == newstatus) $ liftIO $ addTxLogEntry txlogs idu tvar
#ifdef CSHF
			addTxNotify (Just buff_status) status txlogs (metaTxU u)
#endif
			return tvar
		Nothing -> new_entry Nothing i status Nothing
changeTxU u _ status _ = error $ "changeTxU " ++ show status

changeStatus st = TxStatus (if isWriteOrNewTrue st then Write else Eval,False)

-- remembers the original dependencies reference
copyTxUData :: (MonadIO m,MonadRef r m) => TxUData l TxAdapton r m a -> m (r (BuffTxUData l TxAdapton r m a))
copyTxUData dta@(TxValue dirty value force dependencies) = do
	buff_dependencies <- readRef dependencies >>= newRef
	buff_dta <- newRef (TxValue dirty value force buff_dependencies,Left dependencies)
	return buff_dta
copyTxUData dta = liftIO deadWeak >>= \w -> newRef (dta,Right w)

{-# INLINE coerce #-}
coerce :: (Typeable a,Typeable b) => a -> b
coerce = Unsafe.unsafeCoerce
--coerce x = case cast x of
--	Nothing -> error "failed coerce"
--	Just y -> y

-- finds a buffered entry in any depth of a nested tx log
findTxLogEntry :: TxLogs r m -> Unique -> IO (Maybe (DynTxVar r m))
findTxLogEntry txlogs uid = Foldable.foldlM (\mb txlog -> maybe (WeakTable.lookup (txLogBuff txlog) uid) (return . Just) mb) Nothing txlogs

addTxLogEntry :: TxLogs r m -> Unique -> DynTxVar r m -> IO ()
addTxLogEntry txlogs@(SCons txlog _) uid tvar = WeakTable.insertWithMkWeak (txLogBuff txlog) (dynTxMkWeak tvar) uid tvar

-- merges a nested txlog with its parent txlog, by overriding parent entries
mergeTxLog :: MonadIO m => TxLog r m -> TxLog r m -> m ()
mergeTxLog txlog1 txlog2 = liftIO $ do
	WeakTable.mapM_ (\(uid,entry) -> let !mkWeak = dynTxMkWeak entry in WeakTable.insertWithMkWeak (txLogBuff txlog2) mkWeak uid entry) (txLogBuff txlog1)

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

dirtyBufferedDynTxVar :: (WeakRef r,MonadIO m,MonadRef r m) => TxLogs r m -> DynTxVar r m -> m ()
dirtyBufferedDynTxVar txlogs tvar = do
--	liftIO $ performGC -- try to remove dependents that can be killed and don't need to be dirtied
	dirtyBufferedTxData tvar
	dirtyCreatorBufferedTx txlogs (dynVarTxCreator tvar)
	dirtyRecursivelyBufferedDynTxVar txlogs tvar

dirtyCreatorBufferedTx :: (MonadIO m,MonadRef r m,WeakRef r) => TxLogs r m -> Maybe (TxCreator r m) -> m ()
dirtyCreatorBufferedTx txlogs Nothing = return ()
dirtyCreatorBufferedTx txlogs (Just wcreator) = do
	mb <- liftIO $ deRefWeak wcreator
	case mb of
		Just creatorMeta -> do
			mb <- liftIO $ findTxLogEntry txlogs (idTxNM creatorMeta)
			case mb of
				Nothing -> return ()
				Just creator_var -> do
					forgetBufferedTxData creator_var
					dirtyRecursivelyBufferedDynTxVar txlogs creator_var
		Nothing -> return ()

-- dirties only buffered dependents in a txlog, without changing the status of their variables
dirtyRecursivelyBufferedDynTxVar :: (MonadIO m,MonadRef r m) => TxLogs r m -> DynTxVar r m -> m ()
dirtyRecursivelyBufferedDynTxVar txlogs tvar = do
	debugTx2' $ "dirtyRecursivelyBufferedDynTxVar " ++ show (dynTxId tvar)
	let mb = dynTxVarBufferedDependents tvar
	case mb of
		Nothing -> return () -- stop when there are no buffered dependents
		Just buff_dependents -> do
			dependents <- WeakMap.toMap buff_dependents
			Foldable.mapM_ dirtyTx' dependents
  where
	dirtyTx' = \w -> do
		mb <- liftIO $ Weak.deRefWeak w
		case mb of
			Nothing -> return ()
			Just d -> do -- buffered dependents are never original
				isDirty <- readRef (dirtyTxW d)
				isOriginal <- readRef (flagTxW d)
				when isOriginal $ error "original dependent found"
				unless isDirty $ do -- stop when the dependency is original or already dirty
					writeRef (dirtyTxW d) True -- dirty the buffered dependency
					mb <- liftIO $ findTxLogEntry txlogs (idTxNM $ tgtMetaTxW d)
					case mb of
						Nothing -> return () -- stop when the dependent variable is not buffered
						Just tgt_tvar -> do
							dirtyBufferedTxData tgt_tvar
							dirtyRecursivelyBufferedDynTxVar txlogs tgt_tvar

-- dirties a buffered variable in-place without changing its status
dirtyBufferedTxData :: (MonadIO m,MonadRef r m) => DynTxVar r m -> m ()
dirtyBufferedTxData (DynTxU _ _ u (TxStatus (New _,_))) = flip mapRefM_ (dataTxU u) chgDirty  where
	chgDirty (TxValue _ value force dependencies) = return $ TxValue 1# value force dependencies
	chgDirty dta = return dta
dirtyBufferedTxData (DynTxU (Just (BuffTxU buff_dta)) _ _ _) = mapRef chgDirty buff_dta where
	chgDirty (TxValue _ value force dependencies,ori) = (TxValue 1# value force dependencies,ori)
	chgDirty dta = dta
dirtyBufferedTxData _ = return ()

forgetBufferedTxData :: (MonadIO m,MonadRef r m) => DynTxVar r m -> m ()
forgetBufferedTxData (DynTxU _ _ u (TxStatus (New _,_))) = flip mapRefM_ (dataTxU u) forget where
	forget (TxValue _ value force dependencies) = do
		clearDependenciesTx False
		return $ TxThunk force
	forget dta = return dta
forgetBufferedTxData (DynTxU (Just (BuffTxU buff_dta)) _ u _) = mapRefM_ forget buff_dta where
	forget (TxValue _ value force dependencies,ori) = do
		ori' <- case ori of
			Left deps -> liftIO $ liftM Right $ mkWeakRefKey deps deps Nothing
			Right wdeps -> return $ Right wdeps
		clearDependenciesTx False
		return (TxThunk force,ori)
	forget dta = return dta
forgetBufferedTxData _ = return ()

isUnevaluatedOriginalTxU :: TxLayer l r m => TxU l TxAdapton r m a -> m Bool
isUnevaluatedOriginalTxU t = do
	dta <- readRef (dataTxU t)
	case dta of
		TxThunk force -> return True --unevaluated thunk
		otherwise -> return False

-- ** unbuffering

-- don't dirty unbuffered entries (since we are unbuffering all written variables, dependents of unbuffered entries are unbuffered as well)
unbufferTopTxLog :: MonadIO m => TxLogs r m -> Bool -> m ()
unbufferTopTxLog txlogs@(SCons txlog _) onlyWrites = WeakTable.mapMGeneric_ (unbufferDynTxVar' onlyWrites txlogs) $ txLogBuff txlog

unbufferDynTxVar :: MonadIO m => Bool -> TxLogs r m -> DynTxVar r m -> m ()
unbufferDynTxVar onlyWrites txlogs entry = unbufferDynTxVar' onlyWrites txlogs (dynTxId entry,entry)

unbufferTxVar :: MonadIO m => Bool -> TxLogs r m -> Unique -> m ()
unbufferTxVar onlyWrites txlogs uid = do
	mb <- liftIO $ findTxLogEntry txlogs uid
	case mb of
		Just tvar -> do
--			debugTx' $ "unbufferingTx " ++ show onlyWrites ++ "  " ++ show uid ++ " " ++ show (dynTxStatus tvar)
			unbufferDynTxVar' onlyWrites txlogs (uid,tvar)
		Nothing -> do
--			debugTx' $ "nonbufferingTx " ++ show onlyWrites ++ "  " ++ show uid
			return ()

unbufferDynTxVar' :: MonadIO m => Bool -> TxLogs r m -> (Unique,DynTxVar r m) -> m ()
unbufferDynTxVar' onlyWrites txlogs (uid,entry) = do
	when (if onlyWrites then isWriteOrNewTrue (dynTxStatus entry) else True) $ do
		mb <- unbufferDynTxVar'' txlogs onlyWrites entry
		case mb of
			Nothing -> return ()
			Just tvar' -> liftIO $ addTxLogEntry txlogs (dynTxId entry) tvar'

isWriteOrNewTrue :: TxStatus -> Bool
isWriteOrNewTrue (TxStatus (Write,_)) = True
isWriteOrNewTrue (TxStatus (New True,_)) = True
isWriteOrNewTrue _ = False

-- we need to clear the dependencies of unbuffered variables
-- buffered dependents are always preserved, but dirtied in case we discard the buffered data
unbufferDynTxVar'' :: MonadIO m => TxLogs r m -> Bool -> DynTxVar r m -> m (Maybe (DynTxVar r m))
unbufferDynTxVar'' txlogs onlyWrites tvar@(DynTxU Nothing Nothing u (TxStatus (New i,b))) = do
	if (if onlyWrites then i else True)
		then do -- we don't unbuffer the value of New variables; New variables cannot have original dependents
			dirtyBufferedDynTxVar txlogs tvar
			if i
				then return $ Just $ DynTxU Nothing Nothing u $ TxStatus (New False,b)
				else return Nothing
		else return Nothing
unbufferDynTxVar'' txlogs onlyWrites tvar@(DynTxM Nothing Nothing m (TxStatus (New i,b))) = do
	if (if onlyWrites then i else True)
		then do
			dirtyBufferedDynTxVar txlogs tvar
			if i
				then return $ Just $ DynTxM Nothing Nothing m $ TxStatus (New False,b)
				else return Nothing
		else return Nothing
unbufferDynTxVar'' txlogs onlyWrites tvar@(DynTxU _ _ u stat) = if (if onlyWrites then isWriteOrNewTrue else Prelude.const True) stat
	then do
		forgetBufferedTxData tvar
		dirtyBufferedDynTxVar txlogs tvar
		-- unbuffer the thunk, so that the fresher original data is used instead
		let mbtxdeps = dynTxVarBufferedDependents tvar
		let newstat = TxStaatus (Read,isJust mbtxdeps)
		if stat == newstat
			then return Nothing
			else return $ Just $ DynTxU Nothing mbtxdeps u newstat
	else return Nothing
unbufferDynTxVar'' txlogs onlyWrites tvar@(DynTxM _ _ m stat) = if (if onlyWrites then isWriteOrNewTrue else Prelude.const True) stat
	then do
		let mbtxdeps = dynTxVarBufferedDependents tvar
		dirtyBufferedDynTxVar txlogs tvar
		let newstat = TxStatus (Read,isJust mbtxdeps)
		if stat == newstat
			then return Nothing
			else return $ Just $ DynTxM Nothing mbtxdeps m newstat
	else return Nothing

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
extendTxLog :: MonadIO m => TxLog r m -> TxLogs r m -> m ()
extendTxLog txlog1 txlogs2 = do
	let updEntry (uid,entry) = do
		mb_entry1 <- unbufferDynTxVar'' (SCons txlog1 txlogs2) True entry
		case mb_entry1 of
			Nothing -> liftIO $ addTxLogEntry txlogs2 uid entry
			Just entry1 -> liftIO $ addTxLogEntry txlogs2 uid entry1
	WeakTable.mapMGeneric_ updEntry (txLogBuff txlog1)

dynTxStatus :: DynTxVar r m -> TxStatus
dynTxStatus (DynTxU _ _ _ s) = s
dynTxStatus (DynTxM _ _ _ s) = s

dynTxMeta :: DynTxVar r m -> TxNodeMeta r m
dynTxMeta (DynTxU _ _ u _) = metaTxU u
dynTxMeta (DynTxM _ _ m _) = metaTxM m

dynTxId :: DynTxVar r m -> Unique
dynTxId = idTxNM . dynTxMeta

dynTxMkWeak :: DynTxVar r m -> MkWeak
dynTxMkWeak (DynTxU _ _ u _) = MkWeak $ mkWeakRefKey $! dataTxU u
dynTxMkWeak (DynTxM _ _ m _) = MkWeak $ mkWeakRefKey $! dataTxM m

-- gets a buffered dependents set
getBufferedTxDependents :: MonadIO m => TxLogs r m -> TxNodeMeta r m -> TxStatus -> m (TxDependents r m)
getBufferedTxDependents tbl meta status = liftM dynTxVarBufferedDependents (bufferTxNM meta status tbl) >>= \mb -> case mb of
	Just deps -> return deps
	Nothing -> error "getBufferedTxDependents"

-- gets the union of the original and buffered dependents set
getTxDependents :: MonadIO m => TxLogs r m -> TxNodeMeta r m -> TxStatus -> m (TxDependents r m,Map Unique (TxDependent r m))
getTxDependents tbl meta status = do
	var <- bufferTxNM meta status tbl
	txdeps <- dynTxVarDependents var
	return txdeps

getTxDependencies :: (MonadIO m,MonadRef r m) => TxLogs r m -> TxNodeMeta r m -> TxStatus -> m (TxDependencies r m)
getTxDependencies tbl meta status = liftM (fromJustNote "no buffered dependencies") $ bufferTxNM meta status tbl >>= dynTxVarDependencies

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

joinTxRepairMb' :: Monad (Outside TxAdapton r m) => Maybe (TxRepair' r m) -> Maybe (TxRepair' r m) -> Maybe (TxRepair' r m)
joinTxRepairMb' Nothing mb2 = mb2
joinTxRepairMb' mb1 Nothing = mb1
joinTxRepairMb' (Just f) (Just g) = Just $ \txlog -> f txlog >> g txlog

joinTxRepair' :: Monad (Outside TxAdapton r m) => (TxRepair' r m) -> (TxRepair' r m) -> (TxRepair' r m)
joinTxRepair' f g = \txlog -> f txlog >> g txlog

{-# INLINE modifyMVarMasked_' #-}
modifyMVarMasked_' :: (MonadIO m,MonadMask m) => MVar a -> (a -> m a) -> m ()
modifyMVarMasked_' m io =
  Catch.mask_ $ do
    a  <- liftIO $ takeMVar m
    a' <- io a `Catch.onException` liftIO (putMVar m a)
    liftIO $ putMVar m a'


#ifndef CSHF
doBlock :: Monad m => m a -> m a
doBlock = id
#endif
#ifdef CSHF
doBlock :: MonadMask m => m a -> m a
doBlock = Catch.uninterruptibleMask_
#endif


