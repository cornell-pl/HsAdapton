{-# LANGUAGE CPP, MultiParamTypeClasses, UndecidableInstances, FlexibleInstances, DataKinds, Rank2Types, TypeFamilies, TupleSections, StandaloneDeriving, BangPatterns, EmptyDataDecls, FlexibleContexts, TypeOperators, ConstraintKinds, MagicHash, ViewPatterns, KindSignatures, GADTs, ScopedTypeVariables, DeriveDataTypeable, TemplateHaskell #-}

module Control.Concurrent.Transactional.Internal.TxAdapton.Types where

import Data.Time.Clock
import Control.Monad.Incremental
import Control.Concurrent.Future
import Control.Concurrent hiding (readMVar)
import Control.Concurrent.Transactional
import qualified Control.Concurrent.Map.Exts as CMap
import Control.Monad.Incremental.Internal.Adapton.Types
import Data.Concurrent.Deque.Class as Queue
import Data.Concurrent.Deque.Reference.DequeInstance
import Data.Time
import Control.Concurrent.Chan
import qualified Data.HashTable.Weak.ST.Basic as WeakSTBasic
import Data.HashTable.Weak.IO as WeakHash
import Data.HashTable.Weak.Class as WeakHash

import System.IO.Unsafe
import Control.Monad.Fix
import Data.Global.TH as TH
import qualified Control.Concurrent.STM as STM
import Safe
import System.Mem
import System.Mem.StableName.Exts
import GHC.IORef
import GHC.STRef

import Data.Monoid
import Data.Maybe
import Data.Unique
import Data.Typeable
import Data.DeepTypeable
import Data.WithClass.Derive.DeepTypeable
import Data.DeriveTH
import Language.Haskell.TH.Syntax hiding (lift,Infix,Fixity)

import Data.IORef.Exts
import Data.IORef
import Control.Monad.IO.Class
import System.Mem.Weak.Exts as Weak
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map
import Data.HashMap.Strict (HashMap(..))
import qualified Data.HashMap.Strict as HashMap
import Data.Foldable as Foldable
import qualified Data.Strict.Maybe as Strict
import qualified Data.Strict.List as Strict
import qualified Data.Strict.NeList as NeStrict
import Data.Strict.NeList (NeList(..))
import Data.Strict.Tuple as Strict
import qualified Unsafe.Coerce as Unsafe
import Control.Monad.Reader (Reader(..),ReaderT(..),MonadReader(..))
import qualified Control.Monad.Reader as Reader
import Control.Monad.Catch as Catch
import Control.Concurrent.Lock.Exts as Lock
import Control.Concurrent.MVar.Exts
import Control.Monad

import System.Mem.MemoTable (MemoTable(..))
import qualified System.Mem.MemoTable as MemoTable
import Data.Hashable
import Data.Set as Set
import Data.List as List
import System.Mem.WeakMap as WeakMap
import Debug
import Control.Monad.Incremental.Draw

-- ** Transactional exceptions

data TxException = forall e . Exception e => TxException e
    deriving Typeable

instance Show TxException where
    show (TxException e) = show e

instance Exception TxException

txExceptionToException :: Exception e => e -> SomeException
txExceptionToException = toException . TxException

txExceptionFromException :: Exception e => SomeException -> Maybe e
txExceptionFromException x = do
    TxException a <- fromException x
    cast a

-- for parent threads that kill futures
data ThreadKilledTx = ThreadKilledTx deriving (Typeable,Show)

instance Exception ThreadKilledTx where
	toException = txExceptionToException
	fromException = txExceptionFromException

-- if an inner tx validation fails, then we throw an @InvalidTx@ exception to retry the whole atomic block
data AsyncInvalidTx c = AsyncInvalidTx (InvalidTxRepair c) deriving (Typeable)
data InvalidTx c = InvalidTx (InvalidTxRepair c) deriving (Typeable)
instance Show (TxId c) => Show (AsyncInvalidTx c) where
	show (AsyncInvalidTx (from :!: to :!: repair)) = "AsyncInvalidTx " ++ show from ++" "++ show to
instance Show (TxId c) => Show (InvalidTx c) where
	show (InvalidTx (from :!: to :!: repair)) = "InvalidTx" ++ show from ++" "++ show to

instance (Show (AsyncInvalidTx c),Typeable c) => Exception (AsyncInvalidTx c) where
	toException = txExceptionToException
	fromException = txExceptionFromException

instance (Show (InvalidTx c),Typeable c) => Exception (InvalidTx c) where
	toException = txExceptionToException
	fromException = txExceptionFromException
	
data BlockedOnTxRetry = BlockedOnTxRetry deriving (Show,Typeable)
instance Exception BlockedOnTxRetry where
	toException = txExceptionToException
	fromException = txExceptionFromException

-- ** Transactional masked

class Monad m => MonadBlock (c :: TxConflict) m where
	doBlock :: Proxy c -> m a -> m a

type instance IncK (TxAdapton c) a = (Typeable a,Eq a,Show a
	, Draw (TxAdapton c) a
	, Draw (TxAdapton c) ((TxM Versioned c) Inside (TxAdapton c) a)
	, Draw (TxAdapton c) ((TxM Forgetful c) Inside (TxAdapton c) a)
	, Draw (TxAdapton c) ((TxM Cumulative c) Inside (TxAdapton c) a)
	, Draw (TxAdapton c) ((TxM Versioned c) Outside (TxAdapton c) a)
	, Draw (TxAdapton c) ((TxM Forgetful c) Outside (TxAdapton c) a)
	, Draw (TxAdapton c) ((TxM Cumulative c) Outside (TxAdapton c) a)
	, Draw (TxAdapton c) ((TxU c) Inside (TxAdapton c) a)
	, Draw (TxAdapton c) ((TxU c) Outside (TxAdapton c) a)
	)

readRootTx :: TxLayer l c => l (TxAdapton c) (Bool :!: ThreadId)
readRootTx = do
	(s :!: parent :!: fs :!: x :!: y :!: z) <- (txLayer Proxy Reader.ask)
	case parent of
		Just tid -> return (True :!: tid)
		Nothing -> unsafeIOToInc $ liftM (False :!: ) $ myThreadId

readFuturesTx :: TxLayer l c => l (TxAdapton c) (IORef (Map ThreadId (DynTxFuture c)))
readFuturesTx = do
	(s :!: parent :!: fs :!: x :!: y :!: z) <- (txLayer Proxy Reader.ask)
	return fs

{-# INLINE readTxStack #-}
readTxStack :: TxLayer l c => l (TxAdapton c) (TxCallStack c)
readTxStack = liftM (\(!(s :!: parent :!: fs :!: x :!: y :!: z)) -> y) $ (txLayer Proxy Reader.ask)

{-# INLINE readTxParams #-}
readTxParams :: TxLayer l c => l (TxAdapton c) (IncParams (TxAdapton c))
readTxParams = liftM (\(!(s :!: parent :!: fs :!: x :!: y :!: z)) -> s) $ (txLayer Proxy Reader.ask)

{-# INLINE readTxLogs #-}
readTxLogs :: TxLayer l c => l (TxAdapton c) (TxLogs c)
readTxLogs = liftM (\(!(s :!: parent :!: fs :!: x :!: y :!: z)) -> z) $ (txLayer Proxy Reader.ask)

{-# INLINE readTxLogs' #-}
readTxLogs' :: ReaderT (TxEnv c) IO (TxLogs c)
readTxLogs' = liftM (\(!(s :!: parent :!: fs :!: x :!: y :!: z)) -> z) $ Reader.ask

{-# INLINE readTxId #-}
readTxId :: TxLayer l c => l (TxAdapton c) (TxId c)
readTxId = liftM (\(!(s :!: parent :!: fs :!: x :!: y :!: z)) -> x) (txLayer Proxy Reader.ask) >>= unsafeIOToInc . readIORef

{-# INLINE readTxIdRef #-}
readTxIdRef :: TxLayer l c => l (TxAdapton c) (IORef (TxId c))
readTxIdRef = liftM (\(!(s :!: parent :!: fs :!: x :!: y :!: z)) -> x) $ txLayer Proxy $ Reader.ask

type Wakes = Map Unique Lock

type TxCreator c = WTxNodeMeta c
type WTxNodeMeta c = Weak (TxNodeMeta c)

type TxLogId = Int -- the length of the log stack
type TxLogBlockId = TxLogId :!: Int -- logid :!: length of the block stack

newtype TxStatus = TxStatus (TxStatusVar :!: Bool) deriving (Eq,Show,Typeable)
data TxStatusVar = Read !(Strict.Maybe TxLogBlockId) | Eval !(Strict.Maybe TxLogBlockId) | Write !(Strict.Maybe TxLogBlockId) | New !Bool deriving (Eq,Show,Typeable)
-- Read | Eval | Write can be nested (boolean = True); this means that they work over a transaction-local variable
-- read does not copy the original data, and just logs that it has been accessed
-- False: no buffered dependents
-- True: buffered dependents
-- eval is a non-conflicting write; we compute data that is consistent with the original state
-- write is a write to a modifiable or dirtying of a thunk
-- new is for new transaction allocations; the boolean denotes whether the new variable depends on a written variable or not

appendTxLogBlocks Strict.Nothing mb2 = mb2
appendTxLogBlocks mb1 Strict.Nothing = mb1
appendTxLogBlocks mb1@(Strict.Just id1) mb2@(Strict.Just id2) = if id1 == id2 then mb1 else error $ "not appending two uniques " ++ show mb1 ++ show mb2

instance Monoid TxStatusVar where
	mempty = Read Strict.Nothing
	mappend (Read n1) (Read n2) = Read (appendTxLogBlocks n1 n2)
	mappend (Read n1) (Eval n2) = Eval (appendTxLogBlocks n1 n2)
	mappend (Read n1) (Write n2) = Write (appendTxLogBlocks n1 n2)
	mappend (Read n1) (New b) = New b
	mappend (Eval n1) (Read n2) = Eval (appendTxLogBlocks n1 n2)
	mappend (Eval n1) (Eval n2) = Eval (appendTxLogBlocks n1 n2)
	mappend (Eval n1) (Write n2) = Write (appendTxLogBlocks n1 n2)
	mappend (Eval n1) (New b) = New b
	mappend (Write n1) (Read n2) = Write (appendTxLogBlocks n1 n2)
	mappend (Write n1) (Eval n2) = Write (appendTxLogBlocks n1 n2)
	mappend (Write n1) (Write n2) = Write (appendTxLogBlocks n1 n2)
	mappend (Write b1) (New j) = New True
	mappend (New i) (Read _) = New i
	mappend (New i) (Eval _) = New i
	mappend (New i) (Write _) = New True
	mappend (New i) (New j) = New (max i j)

instance Monoid TxStatus where
	mempty = TxStatus (mempty :!: False)
	mappend (TxStatus (m1 :!: d1)) (TxStatus (m2 :!: d2)) = TxStatus (m1 `mappend` m2 :!: max d1 d2)

type TxBuffMemoTable c k b = MemoTable k (MkWeak :!: TxU c Inside (TxAdapton c) b)
-- function that unmemoizes buffered entries from the buffered table of a specific txlog
type TxUnmemo = TxRepair

type TxDependents c = WeakMap Unique (TxDependent c)
type TxDependent c = TxDependency c

newtype TxM (i :: Isolation) (c :: TxConflict) (l :: * -> * -> *) inc a = TxM (
		IORef a -- a constant value
	:!:	TxNodeMeta c
	:!: TxResolve i inc a
	) deriving (Typeable)

type family TxResolve i inc a :: * where
	TxResolve Versioned inc a = ()
	TxResolve Forgetful inc a = ()
	TxResolve Cumulative inc a = Resolve inc a

defaultTxResolve :: (Typeable (TxResolve i inc a),Typeable inc,Typeable a,IsolationKind i,Layer Inside inc) => Proxy i -> Proxy inc -> Proxy a -> TxResolve i inc a
defaultTxResolve i (inc :: Proxy inc) (a :: Proxy a) = case toIsolation i of
	Versioned -> coerce ()
	Forgetful -> coerce ()
	Cumulative -> coerce ((\original parent child -> return child) :: TxResolve Cumulative inc a)

newtype TxU (c :: TxConflict) (l :: * -> * -> *) inc a = TxU (
		IORef (TxUData c l inc a) -- data (stored in a MVar because it is accessed concurrently by multiple threads)
	:!:	TxNodeMeta c		-- metadata
	) deriving (Typeable)

data TxUData (c :: TxConflict) (l :: * -> * -> *) inc a =
	  TxValue !UBool !a !(l inc a) !(TxDependencies c) -- a thunk that has been previously evaluated; invariant: the node is dirty if any of its dependencies is dirty; the dirty flag is just an optimization
	| TxThunk !(l inc a) -- a thunk that has never been evaluated
	| TxConst !a -- a constant value
  deriving Typeable

newtype TxNodeMeta c = TxNodeMeta
		(	Unique
		:!:	TxDependents c -- list of dependents; we purge the list at finalization time
		:!:	(Bool -> ThreadId -> ThreadId -> TxLogs c -> IO ()) -- function that dirties the corresponding value (thunks only)
		:!:	(Bool -> ThreadId -> ThreadId -> TxLogs c -> IO ()) -- function that forgets all the old cached thunk data (thunks only)
		:!:   (Bool -> ThreadId -> ThreadId -> TxStatus -> TxLogs c -> IO (DynTxVar c)) -- function that writes a buffered copy of the original data to a transaction log
		:!:   Maybe (TxCreator c) -- the parent thunk under which the reference was created (modifiables only)
		:!:   WaitQueue -- a sequence of wake-up actions for txs that are waiting on further updates to this node (modifiables only)
		:!:   Lock -- a lock for writes to this variable
		:!:	TxNotifies c
		:!: TxContent c -- local content for faster variable lookup
		:!: MVar (IO () :!: Map ThreadId (IO ())) -- original unmemo and threads that have buffered memo entries for this variable (use the root thread for transactions)
		:!: Outside (TxAdapton c) DrawDot
		)

-- a map from threads to (the top txlog identifier, list of entries)
type TxContent c = CMap.Map ThreadId (IORef (TxLogBlockId :!: Strict.List (Weak (DynTxVar c))))

class TxConflictClass (c :: TxConflict) where
	
	addTxNotifies :: ThreadId -> Maybe TxStatus -> TxStatus -> TxLogs c -> Unique -> TxNotifies c -> IO ()
	newTxNotifies :: Proxy c -> IO (TxNotifies c)
	-- removes the current thread from the notify lists
	unTxNotifies :: Proxy c -> ThreadId -> Unique -> TxNotifies c -> IO ()
	checkTxNotifies :: Proxy c -> Bool -> TxNodeMeta c -> TxNotifies c -> IO (RepairDynTxVar c)
	
	startTx :: Proxy c -> IO (TxId c)
	finishTx :: TxId c -> (TxUnmemo c,RepairDynTxVar c) -> IO ()
	exitTx :: TxLayer l c => l (TxAdapton c) ()
	-- restarts a transaction with a configurable reset parameter
	restartTxWithRepair :: TxLayer l c => Maybe (InvalidTxRepair c) -> String -> TxId c -> l (TxAdapton c) a -> l (TxAdapton c) a
	
	validateTxs :: Bool -> TxId c -> TxLogs c -> IO (Maybe (InvalidTxRepair c))

	appendInvalidTxRepair :: TxRepair c -> InvalidTxRepair c -> InvalidTxRepair c
	extendInvalidTxRepair :: InvalidTxRepair c -> TxRepair c -> InvalidTxRepair c
	
	exceptionalFutureTx :: Exception e => ThreadId -> ThreadId -> TxFuture c a -> e -> ReaderT (TxEnv c) IO ()
	
	withTxLocks :: (TxLayer Outside c,TxLayer l c) => (Bool -> l (TxAdapton c) a) -> l (TxAdapton c) a

newtype TxFuture (c :: TxConflict) a = TxFuture { unTxFuture :: MVar (Either SomeException a :!: ThreadId :!: TxEnv c) }

type family TxNotifies (c :: TxConflict) where
	TxNotifies EarlyConflict = MVar (Map ThreadId Bool) -- a set of threads that depend on this variable (True=isWriteOrNewTrue, False = otherwise) (always use root thread for transactions)
	TxNotifies CommitConflict = ()

-- the information produced by commit to inform concurrent threads of possible conflicts
-- updating a modifiable may propagate to multiple thunks; they are all logged
-- a map from written modifiables/thunks to a boolean indicating whether it is a node write (True) or a dependents write (False)
data family RepairDynTxVar (c :: TxConflict)

instance Eq (TxNodeMeta c) where
	m1 == m2 = idTxNM m1 == idTxNM m2
	{-# INLINE (==) #-}
instance Ord (TxNodeMeta c) where
	compare m1 m2 = idTxNM m1 `compare` idTxNM m2
	{-# INLINE compare #-}

newtype instance RepairDynTxVar EarlyConflict = RepairDynTxVarE { unRepairDynTxVarE :: Map ThreadId (TxRepair EarlyConflict) }
newtype instance RepairDynTxVar CommitConflict = RepairDynTxVarC { unRepairDynTxVarC :: Map (TxNodeMeta CommitConflict) Bool }

instance Monoid (RepairDynTxVar EarlyConflict) where
	mempty = RepairDynTxVarE Map.empty
	mappend (RepairDynTxVarE m1) (RepairDynTxVarE m2) = RepairDynTxVarE $ Map.unionWith mappend m1 m2
instance Monoid (RepairDynTxVar CommitConflict) where
	mempty = RepairDynTxVarC Map.empty
	mappend (RepairDynTxVarC m1) (RepairDynTxVarC m2) = RepairDynTxVarC $ Map.unionWith max m1 m2

-- (a new starttime,receiver thread,repairing actions over an invalid environment)
type InvalidTxRepair c = (TxId c :!: ThreadId :!: TxRepair c)

newtype TxRepair c = TxRepair (forall l . TxLayer l c => l (TxAdapton c) ())

-- a wait queue of tx locks and environments
-- thread-safe, because a retrying thread that only reads a variable places itself in the queue (and reads are not locked)
type WaitQueue = Deque Threadsafe Threadsafe SingleEnd SingleEnd Grow Safe Lock

-- buffered data and a list of dependency referencies on which the current dependencies may depend
type BuffTxUData c l inc a = (TxUData c l inc a :!: [TxDependencies c])

type BuffTxU c l a = IORef (Maybe (BuffTxUData c l (TxAdapton c) a))

blankBuffTxU :: IO (BuffTxU c l a)
blankBuffTxU = newIORef Nothing
blankBuffTxM :: IO (BuffTxM a)
blankBuffTxM = newIORef Nothing

type BuffTxM a = IORef (Maybe a)


type TxDependencies c = IORef [(TxDependency c,Weak (TxDependency c))]
-- bool says whether the dependency is original or buffered
newtype TxDependency c = TxDependency (
		TxNodeMeta c -- the metadata of the source node
	:!:	IORef Bool -- dirty flag
	:!:	Inside (TxAdapton c) (Bool,TxStatus) -- a checking condition with the previously seen value of the source node
	:!:	TxNodeMeta c -- the metadata of the target node
	:!:   IORef Bool -- original/buffered flag
	:!:	MkWeak -- parent dependencies list
	)
deriving instance Typeable TxDependency

data WeakTxVar c where
	WeakTxU :: (IncK (TxAdapton c) a,TxLayer l c) => !(Weak (TxU c l (TxAdapton c) a)) -> WeakTxVar c
	WeakTxM :: (IncK (TxAdapton c) a,TxLayer l c) => !(Weak (TxM i c l (TxAdapton c) a)) -> WeakTxVar c

-- a buffered thunk and an optional global thunk
-- we use weak pointers to the original data because we don't want to keep original thunks alive unecessarily; if the original data dies then the buffered data dies
-- read False = Nothing Nothing original
-- read True = Nothing (Just $ buffered dependents) original
-- eval = Just (Just $ buffered dependents)original
-- write = Just (Just $ buffered dependents) original
-- new = Nothing Nothing new
-- buffered dependents are ALWAYS seen as an overlay over the original dependents
data DynTxVar c where
	DynTxU :: (IncK (TxAdapton c) a,TxLayer l c) => BuffTxU c l a -> (TxDependents c) -> !(TxU c l (TxAdapton c) a) -> !(IORef TxStatus) -> DynTxVar c
	DynTxM :: (Typeable (TxResolve i (TxAdapton c) a),IncK (TxAdapton c) a,TxLayer l c,IsolationKind i) => BuffTxM a -> (TxDependents c) -> !(TxM i c l (TxAdapton c) a) -> !(IORef TxStatus) -> DynTxVar c
--	DynTxL :: Maybe (BuffTxL l inc a) -> TxL l inc a -> TxStatus -> DynTxVar

data DynTxFuture c where
	DynTxFuture :: FutureK (Outside (TxAdapton c)) a => TxFuture c a -> DynTxFuture c

-- we use a lock-free map since multiple threads may be accessing it to register/unregister new txlogs
-- a persistent memo table and a family of buffered (transaction-local) memo tables indexed by a buffered tx log
-- note that added entries to the memotable are always New (hence we have TxU and not in buffered tables)
type TxMemoTable c k b = (MemoTable k (TxU c Inside (TxAdapton c) b) :!: CMap.Map ThreadId (TxBuffMemoTables c k b))

-- mapping from txlogs to buffered memo tables
type TxBuffMemoTables c k b = IORef (Map TxLogId [TxBuffMemoTable c k b])

data DynTxMemoTable c where
	DynTxMemoTable :: (Eq k,Hashable k) => TxMemoTable c k b -> DynTxMemoTable c

-- an IORef is just fine, because the stack is transaction-local
-- the dependencies references in the stack are always buffered copies
type TxCallStack c = IORef (TxCallStack' c)
type TxCallStack' c = Strict.List (TxStackElement c)

type TxStackElement c = (TxNodeMeta c :!: Strict.Maybe (TxDependencies c) :!: IORef TxStatus)

-- | A table mapping variable identifiers to buffered content, together with a list of memo tables (one per memoized function) that contain transient entries for this log
-- has a unique identifier so that it can be a key in a table of buffered memotables
-- this list is local to the tx (so no concurrent handling is needed) and used to know which tables to merge with the persistent memo tables at commit time
-- a txlog is partitioned into a list of memotables, to handle nested futures
newtype TxLog c = TxLog (TxLogId :!: IORef (NeList (TxLogBlock c)) :!: IORef [DynTxMemoTable c]) deriving Typeable
type TxLogs c = NeList (TxLog c)
type TxLogBlock c = MemoTable.CuckooHashTable Unique (DynTxVar c)

instance Eq (TxLog c) where
	(TxLog (uid1 :!: _ :!: _)) == (TxLog (uid2 :!: _ :!: _)) = uid1 == uid2
	{-# INLINE (==) #-}
instance Ord (TxLog c) where
	compare (TxLog (uid1 :!: _ :!: _)) (TxLog (uid2 :!: _ :!: _)) = compare uid1 uid2
	{-# INLINE compare #-}

instance WeakKey (TxLog c) where
	mkWeakKey (TxLog (_ :!: tbl :!: memos)) = mkWeakKey memos
	{-# INLINE mkWeakKey #-}

-- a tx environment contains (incremental parameters, a parent thread (for nested futures) , a list of children futures, a tx start time,a callstack,a list of nested logs -- to support nested transactions)
type TxEnv c = (IncParams (TxAdapton c) :!: Maybe ThreadId :!: IORef (Map ThreadId (DynTxFuture c)) :!: IORef (TxId c) :!: TxCallStack c :!: TxLogs c)

type TxEnvE = (IncParams (TxAdapton EarlyConflict) :!: Maybe ThreadId :!: IORef (Map ThreadId (DynTxFuture EarlyConflict)) :!: IORef ThreadId :!: TxCallStack EarlyConflict :!: TxLogs EarlyConflict)
type TxEnvC = (IncParams (TxAdapton CommitConflict) :!: Maybe ThreadId :!: IORef (Map ThreadId (DynTxFuture CommitConflict)) :!: IORef UTCTime :!: TxCallStack CommitConflict :!: TxLogs CommitConflict)

txEnvParams (params :!: _ :!: _ :!: _ :!: _ :!: _) = params
txEnvLogs (_ :!: _ :!: _ :!: _ :!: _ :!: txlogs) = txlogs

data family TxAdapton (c :: TxConflict) :: *
deriving instance Typeable TxAdapton

type family TxId (c :: TxConflict) where
	TxId EarlyConflict = ThreadId -- the thread of the tx
	TxId CommitConflict = UTCTime -- the starting time of the tx

data TxConflict = EarlyConflict | CommitConflict deriving Typeable
deriving instance Typeable EarlyConflict
deriving instance Typeable CommitConflict

class TxLayerImpl l where
	unTxLayer :: Proxy l -> l (TxAdapton c) a -> ReaderT (TxEnv c) IO a
	txLayer :: Proxy l -> ReaderT (TxEnv c) IO a -> l (TxAdapton c) a

type TxLayer l c = (Monoid (RepairDynTxVar c),TxLayerImpl l,Show (InvalidTx c),Show (TxId c),MonadBlock c (ReaderT (TxEnv c) IO),MonadBlock c IO,Typeable c,TxConflictClass c,Typeable l,Layer l (TxAdapton c))

debugTx,debugTx2 :: TxLayer l c => String -> l (TxAdapton c) ()
--debugTx str = return ()
debugTx str = do
	time <- readTxId
	threadid <- unsafeIOToInc $ myThreadId
	unsafeIOToInc $ debugM ("["++show time++"] " ++ str) $ return ()
--debugTx2 = debugTx
debugTx2 msg = return ()

debugTx',debugTx2' :: MonadIO m => String -> m ()
--debugTx' str = return ()
debugTx' str = debugM str $ return ()
--debugTx2' = debugTx'
debugTx2' msg = return ()

instance DeepTypeable EarlyConflict where
	typeTree _ = MkTypeTree (mkName "Control.Concurrent.Transactional.TxAdapton.Types.EarlyConflict") [] []

instance DeepTypeable CommitConflict where
	typeTree _ = MkTypeTree (mkName "Control.Concurrent.Transactional.TxAdapton.Types.CommitConflict") [] []

instance DeepTypeable TxAdapton where
	typeTree _ = MkTypeTree (mkName "Control.Concurrent.Transactional.TxAdapton.Types.TxAdapton") [] []

instance (DeepTypeable c) => DeepTypeable (TxAdapton c) where
	typeTree (_ :: Proxy (TxAdapton c)) = MkTypeTree (mkName "Control.Concurrent.Transactional.TxAdapton.Types.TxAdapton") args []
		where args = [typeTree (Proxy::Proxy c)]

isNested :: TxStatus -> Strict.Maybe TxLogBlockId
isNested (TxStatus (Read n :!: _)) = n
isNested (TxStatus (Eval n :!: _)) = n
isNested (TxStatus (Write n :!: _)) = n
isNested _ = Strict.Nothing
{-# INLINE isNested #-}

dirtyStatus status1 bid status2 = status1 `mappend` unNew bid status2
{-# INLINE dirtyStatus #-}

-- returns a status that is a write or an eval based on the original status
bitStatus st = let !v = if isWriteOrNewTrue st then Write else Eval in TxStatus $! (v Strict.Nothing :!: False)
{-# INLINE bitStatus #-}

unNew :: TxLogBlockId -> TxStatus -> TxStatus
unNew bid (TxStatus (New False :!: b)) = TxStatus (Read (Strict.Just bid) :!: b)
unNew bid (TxStatus (New True :!: b)) = TxStatus (Write (Strict.Just bid) :!: b)
{-# INLINE unNew #-}

mkNew :: TxStatus -> TxStatus
mkNew (TxStatus (Read _ :!: b)) = TxStatus (New False :!: b)
mkNew (TxStatus (Eval _ :!: b)) = TxStatus (New False :!: b)
mkNew (TxStatus (Write _ :!: b)) = TxStatus (New True :!: b)

isNew :: TxStatus -> Bool
isNew (TxStatus (New _ :!: _)) = True
isNew _ = False
{-# INLINE isNew #-}

{-# INLINE isWriteOrNewTrue #-}
isWriteOrNewTrue :: TxStatus -> Bool
isWriteOrNewTrue (TxStatus (Write _ :!: _)) = True
isWriteOrNewTrue (TxStatus (New True :!: _)) = True
isWriteOrNewTrue _ = False

isWrite :: TxStatus -> Bool
isWrite (TxStatus (Write _ :!: _)) = True
isWrite _ = False
{-# INLINE isWrite #-}

isDependentsWrite :: TxStatus -> Bool
isDependentsWrite (TxStatus (_ :!: b)) = b
{-# INLINE isDependentsWrite #-}

isRead (TxStatus (Read _ :!: _)) = True
isRead _ = False
{-# INLINE isRead #-}

isEval (TxStatus (Eval _ :!: _)) = True
isEval _ = False
{-# INLINE isEval #-}

isEvalOrWrite (TxStatus (Eval n :!: i)) = Just (n,i)
isEvalOrWrite (TxStatus (Write n :!: i)) = Just (n,i)
isEvalOrWrite _ = Nothing
{-# INLINE isEvalOrWrite #-}

isReadOrEvalOrWrite (TxStatus (Read n :!: i)) = Just (n,i)
isReadOrEvalOrWrite (TxStatus (Eval n :!: i)) = Just (n,i)
isReadOrEvalOrWrite (TxStatus (Write n :!: i)) = Just (n,i)
isReadOrEvalOrWrite _ = Nothing
{-# INLINE isReadOrEvalOrWrite #-}

instance Hashable (TxLog c) where
	hashWithSalt i (TxLog (id1 :!: _ :!: _)) = hashWithSalt i id1

{-# INLINE finalizeTxLog #-}
finalizeTxLog :: TxConflictClass c => Maybe ThreadId -> TxLog c -> IO ()
finalizeTxLog unnotify (txlog::TxLog c) = {-# SCC finalizeTxLog #-} do
	let c = Proxy :: Proxy c
	blocks@(NeStrict.head -> block) <- readIORef (txLogBuff txlog)
	let purge block = do
		case unnotify of
			Nothing -> return ()
			Just thread -> MemoTable.mapM_ (\(uid,tvar) -> unTxNotifies c thread uid $ dynTxNotifies tvar) block 
		MemoTable.finalize block
	Foldable.mapM_ purge blocks
	writeIORef' (txLogBuff txlog) $ NeWrap block

txLogId (TxLog (x :!: y :!: z)) = x
txLogBuff (TxLog (x :!: y :!: z)) = y
txLogMemo (TxLog (x :!: y :!: z)) = z
{-# INLINE txLogId #-}
{-# INLINE txLogBuff #-}
{-# INLINE txLogMemo #-}

addBlockTxLogs :: Int -> TxLogs c -> IO ()
addBlockTxLogs memosize (NeStrict.head -> txlog) = do
	let mkWeak = MkWeak $ mkWeakKey (txLogMemo txlog)
	x <- MemoTable.newSizedWithMkWeak memosize mkWeak
	modifyIORef' (txLogBuff txlog) (NeCons x)

snapshotTxLogs :: Int -> ThreadId -> TxLogs c -> IO (ThreadId -> IO (TxLogs c))
snapshotTxLogs memosize parent_thread (NeWrap parent_txlog) = do
	(child_txlog,add) <- snapshotTxLog memosize parent_thread parent_txlog
	return $ \child_thread -> add child_thread >> return (NeWrap child_txlog)
snapshotTxLogs memosize parent_thread (NeCons parent_txlog parent_txlogs) = do
	(child_txlog,add) <- snapshotTxLog memosize parent_thread parent_txlog
	return $ \child_thread -> add child_thread >> return (NeCons child_txlog parent_txlogs)

snapshotTxLog :: Int -> ThreadId -> TxLog c -> IO (TxLog c,ThreadId -> IO ())
snapshotTxLog memosize parent_thread parent_txlog@(TxLog (uid :!: buff :!: memos)) = do
	-- snapshot parent
	buff' <- copyIORef' buff
	-- add block to parent
	modifyIORefM_' buff $ \xs -> do
		let mkWeak = MkWeak $ mkWeakKey memos
		x <- MemoTable.newSizedWithMkWeak memosize mkWeak
		return (NeCons x xs)
	
	-- copy the memos
	memos' <- copyIORef' memos
	-- add snapshoted memo entries for the new thread
	let snapshotMemo io (DynTxMemoTable (ori_tbl :!: buff_tbls)) = do
		mb <- CMap.lookup parent_thread buff_tbls
		case mb of
			Nothing -> return io
			Just buffs -> do
				-- snapshot parent
				buffs' <- copyIORef' buffs
				-- add block to parent
				modifyIORefM_' buffs $ \xs -> do
					let !memoMkWeak = MkWeak $ mkWeakKey ori_tbl
					x <- MemoTable.newSizedWithMkWeak memosize memoMkWeak
					return $ Map.insertWith (++) uid [x] xs -- insert to the head
				-- add block to child
				modifyIORefM_' buffs' $ \xs -> do
					let !memoMkWeak = MkWeak $ mkWeakKey ori_tbl
					x <- MemoTable.newSizedWithMkWeak memosize memoMkWeak
					return $ Map.insertWith (++) uid [x] xs -- insert to the head
				-- delay commit to concurrent map until we have the child's threadid
				return $ \child_thread -> io child_thread >> CMap.insert child_thread buffs' buff_tbls
	
	io <- readIORef memos >>= Foldable.foldlM snapshotMemo mempty
	return (TxLog (uid :!: buff' :!: memos'),io)

{-# INLINE emptyTxLog #-}
emptyTxLog :: TxLogId -> IORef (TxId c) -> Int -> IO (TxLog c)
emptyTxLog logid start memosize = do
	memos <- newIORef []
	let mkWeak = MkWeak $ mkWeakKey start
	buff <- MemoTable.newSizedWithMkWeak memosize mkWeak >>= newIORef' . NeWrap
	return $ TxLog (logid :!: buff :!: memos)			

{-# INLINE markOriginalTxDependencies #-}
markOriginalTxDependencies :: TxDependencies c -> IO ()
markOriginalTxDependencies dependencies = readIORef dependencies >>= Foldable.mapM_ markDependencyTx where
	markDependencyTx (d,w) = do
		writeIORef' (originalTxW d) True

srcMetaTxW (TxDependency (srcMeta :!: dirty :!: check :!: tgtMeta :!: flag :!: dependencies)) = srcMeta
dirtyTxW (TxDependency (srcMeta :!: dirty :!: check :!: tgtMeta :!: flag :!: dependencies)) = dirty
checkTxW (TxDependency (srcMeta :!: dirty :!: check :!: tgtMeta :!: flag :!: dependencies)) = check
tgtMetaTxW (TxDependency (srcMeta :!: dirty :!: check :!: tgtMeta :!: flag :!: dependencies)) = tgtMeta
originalTxW (TxDependency (srcMeta :!: dirty :!: check :!: tgtMeta :!: flag :!: dependencies)) = flag
dependenciesTxW (TxDependency (srcMeta :!: dirty :!: check :!: tgtMeta :!: flag :!: dependencies)) = dependencies
{-# INLINE srcMetaTxW #-}
{-# INLINE dirtyTxW #-}
{-# INLINE checkTxW #-}
{-# INLINE tgtMetaTxW #-}
{-# INLINE originalTxW #-}
{-# INLINE dependenciesTxW #-}

dataTxM (TxM (v :!: meta :!: resolve)) = v
metaTxM (TxM (v :!: meta :!: resolve)) = meta
resolveTxM (TxM (v :!: meta :!: resolve)) = resolve
{-# INLINE dataTxM #-}
{-# INLINE metaTxM #-}
{-# INLINE resolveTxM #-}

instance Eq (TxM i c l inc a) where
	t1 == t2 = idTxNM (metaTxM t1) == idTxNM (metaTxM t2)
	{-# INLINE (==) #-}

dataTxU (TxU (dta :!: meta)) = dta
metaTxU (TxU (dta :!: meta)) = meta
{-# INLINE dataTxU #-}
{-# INLINE metaTxU #-}

instance Eq (TxU c l inc a) where
	t1 == t2 = idTxNM (metaTxU t1) == idTxNM (metaTxU t2)
	{-# INLINE (==) #-}

isDynTxU (DynTxU _ _ _ _) = True
isDynTxU _ = False

idTxNM 			(TxNodeMeta (uid :!: deps :!: dirty :!: forget :!: buffer :!: creator :!: wait :!: lck :!: n :!: c :!: unmemo :!: draw)) = uid
dependentsTxNM 	(TxNodeMeta (uid :!: deps :!: dirty :!: forget :!: buffer :!: creator :!: wait :!: lck :!: n :!: c :!: unmemo :!: draw)) = deps
dirtyTxNM 		(TxNodeMeta (uid :!: deps :!: dirty :!: forget :!: buffer :!: creator :!: wait :!: lck :!: n :!: c :!: unmemo :!: draw)) = dirty
forgetTxNM 		(TxNodeMeta (uid :!: deps :!: dirty :!: forget :!: buffer :!: creator :!: wait :!: lck :!: n :!: c :!: unmemo :!: draw)) = forget
bufferTxNM 		(TxNodeMeta (uid :!: deps :!: dirty :!: forget :!: buffer :!: creator :!: wait :!: lck :!: n :!: c :!: unmemo :!: draw)) = buffer
creatorTxNM 	(TxNodeMeta (uid :!: deps :!: dirty :!: forget :!: buffer :!: creator :!: wait :!: lck :!: n :!: c :!: unmemo :!: draw)) = creator
waitTxNM 		(TxNodeMeta (uid :!: deps :!: dirty :!: forget :!: buffer :!: creator :!: wait :!: lck :!: n :!: c :!: unmemo :!: draw)) = wait
lockTxNM 		(TxNodeMeta (uid :!: deps :!: dirty :!: forget :!: buffer :!: creator :!: wait :!: lck :!: n :!: c :!: unmemo :!: draw)) = lck
notifiesTxNM 	(TxNodeMeta (uid :!: deps :!: dirty :!: forget :!: buffer :!: creator :!: wait :!: lck :!: n :!: c :!: unmemo :!: draw)) = n
contentTxNM 	(TxNodeMeta (uid :!: deps :!: dirty :!: forget :!: buffer :!: creator :!: wait :!: lck :!: n :!: c :!: unmemo :!: draw)) = c
unmemoTxNM 		(TxNodeMeta (uid :!: deps :!: dirty :!: forget :!: buffer :!: creator :!: wait :!: lck :!: n :!: c :!: unmemo :!: draw)) = unmemo
drawTxNM 		(TxNodeMeta (uid :!: deps :!: dirty :!: forget :!: buffer :!: creator :!: wait :!: lck :!: n :!: c :!: unmemo :!: draw)) = draw
{-# INLINE idTxNM #-}
{-# INLINE dependentsTxNM #-}
{-# INLINE dirtyTxNM #-}
{-# INLINE forgetTxNM #-}
{-# INLINE bufferTxNM #-}
{-# INLINE creatorTxNM #-}
{-# INLINE waitTxNM #-}
{-# INLINE lockTxNM #-}
{-# INLINE notifiesTxNM #-}
{-# INLINE contentTxNM #-}
{-# INLINE unmemoTxNM #-}

weakTxU :: (IncK (TxAdapton c) a,TxLayer l c) => TxU c l (TxAdapton c) a -> IO (WeakTxVar c)
weakTxU u = do
	w <- mkWeakKey (dataTxU u) u Nothing
	return $ WeakTxU w

weakTxM :: (IncK (TxAdapton c) a,TxLayer l c) => TxM i c l (TxAdapton c) a -> IO (WeakTxVar c)
weakTxM m = do
	w <- mkWeakKey (dataTxM m) m Nothing
	return $ WeakTxM w

-- | registers a new wait on modifications of a node when a lock is provided
enqueueWait :: Lock -> TxNodeMeta c -> IO ()
enqueueWait lck meta = pushL (waitTxNM meta) lck
{-# INLINE enqueueWait #-}

-- the same tx may depend on various modifiables, so we need to account for multiple wakeups
tryRelease :: Lock -> IO ()
tryRelease lck = do
	isLocked <- locked lck
	if isLocked then release lck else return ()
{-# INLINE tryRelease #-}

dynTxMValue :: Typeable a => DynTxVar c -> IO (a :!: TxStatus)
dynTxMValue (DynTxM (buff_value) txdeps m txstat) = do
	stat <- readIORef' txstat
	case stat of
		(isEvalOrWrite -> Just _) -> do
			v <- liftM (fromJustNote $ "dynTxMValue " ++ show stat) $ readIORef buff_value
			let c = coerce v
			return (c :!: stat)
		otherwise -> do
			v <- readIORef (dataTxM m)
			let c = coerce v
			return (c :!: stat)
dynTxMValue _ = error "not a TxM"

dynTxVarLock :: DynTxVar c -> Lock
dynTxVarLock (DynTxM _ _ m _) = lockTxNM $ metaTxM m
dynTxVarLock (DynTxU _ _ u _) = lockTxNM $ metaTxU u
{-# INLINE dynTxVarLock #-}

dynVarTxCreator :: DynTxVar c -> Maybe (TxCreator c)
dynVarTxCreator (DynTxM _ _ m _) = creatorTxNM $ metaTxM m
dynVarTxCreator (DynTxU _ _ u _) = creatorTxNM $ metaTxU u
{-# INLINE dynVarTxCreator #-}

-- merge the original dependents on top of the buffered ones (without overriding buffered content)
-- buffered dependents take priority
dynTxVarDependents :: DynTxVar c -> IO (TxDependents c,HashMap Unique (TxDependent c))
dynTxVarDependents (DynTxU _ txdeps u txstat) = do
	!stat <- readIORef txstat
	case stat of
		TxStatus (New _ :!: _) -> do
			deps <- WeakMap.toMap $ dependentsTxNM $ metaTxU u
			return (dependentsTxNM $ metaTxU u,deps)
		TxStatus (_ :!: hasDep) -> if hasDep
			then do
				xs <- WeakMap.toMap txdeps
				ys <- WeakMap.toMap (dependentsTxNM $ metaTxU u)
				return (txdeps,xs `HashMap.union` ys)
			else do
				deps <- WeakMap.toMap $ dependentsTxNM $ metaTxU u
				return (error "no buffered dependents",deps)
dynTxVarDependents (DynTxM _ txdeps m txstat) = do
	!stat <- readIORef txstat
	case stat of
		TxStatus (New _ :!: _) -> do
			deps <- WeakMap.toMap $ dependentsTxNM $ metaTxM m
			return (dependentsTxNM $ metaTxM m,deps)
		TxStatus (_ :!: hasDep) -> if hasDep
			then do
				xs <- WeakMap.toMap txdeps
				ys <- WeakMap.toMap (dependentsTxNM $ metaTxM m)
				return (txdeps,xs `HashMap.union` ys)
			else do
				deps <- WeakMap.toMap $ dependentsTxNM $ metaTxM m
				return (error "no buffered dependents",deps)

-- returns only the buffered dependents of a buffered variable
dynTxVarBufferedDependents :: DynTxVar c -> IO (TxDependents c)
dynTxVarBufferedDependents (DynTxM _ txdeps m txstat) = do
	!stat <- readIORef txstat
	case stat of
		(TxStatus (New _ :!: _)) -> return $ dependentsTxNM $ metaTxM m
		otherwise -> return txdeps
dynTxVarBufferedDependents (DynTxU _ txdeps u txstat) = do
	!stat <- readIORef txstat
	case stat of
		(TxStatus (New _ :!: _)) -> return $ dependentsTxNM $ metaTxU u
		otherwise -> return txdeps
{-# INLINE dynTxVarBufferedDependents #-}	

dynTxVarDependencies :: DynTxVar c -> IO (Maybe (TxDependencies c))
dynTxVarDependencies (DynTxU (buff_dta) _ u txstat) = do
	!stat <- readIORef txstat
	case stat of
		(isEvalOrWrite -> Just _) -> do
			mb <- readIORef' buff_dta
			case mb of
				Just !(dta :!: oris) -> do
					case dta of
						TxValue dirty value force dependencies -> return $ Just dependencies
						otherwise -> return Nothing
				Nothing -> error $ "dynTxVarDependencies " ++ show stat
		otherwise -> do
			(readIORef $ dataTxU u) >>= \dta -> case dta of
				TxValue dirty value force dependencies -> return $ Just dependencies
				otherwise -> return Nothing
dynTxVarDependencies _ = return Nothing

-- stores a modifiable in a transactional buffer with a minimal status and returns a buffered copy
-- Read = an entry is enough
-- Eval = a transaction-consistent entry is enough
-- Write = creates a new or inconsistent entry
bufferTxM :: (Typeable (TxResolve i (TxAdapton c) a),IsolationKind i,IncK (TxAdapton c) a,TxLayer l c,TxLayer Outside c) => Bool -> ThreadId -> ThreadId -> TxM i c l (TxAdapton c) a -> TxStatus -> TxLogs c -> IO (DynTxVar c)
bufferTxM isFuture rootThread thread m st@(TxStatus (Read _ :!: i)) (txlogs :: TxLogs c) = doBlock (Proxy :: Proxy c) $ do
	let !idm = idTxNM $ metaTxM m
	mb <- findTxContentEntry isFuture thread txlogs (metaTxM m)
	case mb of
		Strict.Just (tvar@(DynTxM (buff_dta) txdeps u txstat) :!: isTop :!: blockid) -> do
			!status <- readIORef txstat
			case status of
				TxStatus (New _ :!: j) -> if isTop
					then do
						let !newst = status `mappend` st
						writeIORef' txstat newst
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						return tvar
					else do
						let !newst = dirtyStatus st blockid status
						buff_dta' <- blankBuffTxM
						txdeps' <- WeakMap.new'
						txstat' <- newIORef' newst
						let tvar' = DynTxM buff_dta' txdeps' m txstat'
						addTxLogEntryUp thread txlogs idm tvar'
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
--						debugTx' $ "newReadM " ++ show idm ++ " " ++ show newst
						return tvar'
				otherwise -> if isTop
					then do
						let !newst = status `mappend` st
						writeIORef' txstat newst
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						return tvar
					else do
						let !newst = status `mappend` st
						buff_dta' <- if isRead status then blankBuffTxM else (liftM coerce $ copyIORef' buff_dta)
						txdeps' <- WeakMap.copy' txdeps
						txstat' <- newIORef' newst
						let tvar' = DynTxM buff_dta' txdeps' m txstat'
						addTxLogEntryUp thread txlogs idm tvar'
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						return tvar'
		Strict.Nothing -> do
			buff_dta' <- blankBuffTxM
			txdeps' <- WeakMap.new'
			txstat' <- newIORef' st
			let !tvar' = DynTxM buff_dta' txdeps' m txstat'
			addTxLogEntryUp thread txlogs idm tvar'
			addTxNotifies rootThread Nothing st txlogs idm (notifiesTxNM $! metaTxM m)
			return tvar'		
bufferTxM isFuture rootThread thread m st@(TxStatus (Eval _ :!: i)) (txlogs :: TxLogs c) = doBlock (Proxy :: Proxy c) $ do
	let !idm = idTxNM $ metaTxM m
	mb <- findTxContentEntry isFuture thread txlogs (metaTxM m)
	case mb of
		Strict.Just (tvar@(DynTxM (buff_dta) txdeps m txstat) :!: isTop :!: blockid) -> do
			!status <- readIORef txstat
			case status of
				TxStatus (New _ :!: j) -> if isTop
					then do
						let !newst = status `mappend` st
						writeIORef' txstat newst
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						return tvar
					else do
						let !newst = dirtyStatus st blockid status
						buff_dta' <- readIORef' (dataTxM m) >>= newIORef' . Just
						txstat' <- newIORef' newst
						txdeps' <- WeakMap.new'
						let tvar' = DynTxM (buff_dta') txdeps' m txstat'
						addTxLogEntryUp thread txlogs idm tvar'
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						--debugTx' $ "newEvalM " ++ show idm ++ " " ++ show newst
						return tvar'
				TxStatus (Read _ :!: _) -> if isTop
					then do
						let !newst = st `mappend` status
						buff_value' <- readIORef' $ dataTxM m
						writeIORef buff_dta $! Just $! buff_value'
						writeIORef' txstat newst
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						return tvar
					else do
						let !newst = st `mappend` status
						buff_value' <- readIORef' $ dataTxM m
						buff_dta' <- newIORef' $! Just $! buff_value'
						txdeps' <- WeakMap.copy' txdeps
						txstat' <- newIORef' newst
						let tvar' = DynTxM (buff_dta') txdeps' m txstat'
						addTxLogEntryUp thread txlogs idm tvar'
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						return tvar'
				otherwise -> if isTop
					then do
						let !newst = st `mappend` status
						writeIORef' txstat newst
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						return tvar
					else do
						let !newst = st `mappend` status
						!buff_dta' <- copyIORef' buff_dta
						txdeps' <- WeakMap.copy' txdeps
						txstat' <- newIORef' newst
 						let tvar' = DynTxM (buff_dta') txdeps' m txstat'
						addTxLogEntryUp thread txlogs idm tvar'
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						return tvar'
		Strict.Nothing -> do
			buff_value' <- readIORef' (dataTxM m)
			buff_dta' <- newIORef' $! Just $! buff_value'
			txdeps' <- WeakMap.new'
			txstat' <- newIORef' st
			let tvar' = DynTxM (buff_dta') txdeps' m txstat'
			addTxLogEntryUp thread txlogs idm tvar'
			addTxNotifies rootThread Nothing st txlogs idm (notifiesTxNM $! metaTxM m)
			return tvar'
bufferTxM isFuture rootThread thread m status txlogs = changeTxM isFuture rootThread thread m Nothing status txlogs

-- changes a modifiable, or just buffers it
changeTxM :: (Typeable (TxResolve i (TxAdapton c) a),IsolationKind i,IncK (TxAdapton c) a,TxLayer l c,TxLayer Outside c) => Bool -> ThreadId -> ThreadId -> TxM i c l (TxAdapton c) a -> Maybe a -> TxStatus ->  TxLogs c -> IO (DynTxVar c)
changeTxM isFuture rootThread thread m mbv' st@(isEvalOrWrite -> Just (n,i)) (txlogs :: TxLogs c) = doBlock (Proxy :: Proxy c) $ do
	let !idm = idTxNM $ metaTxM m
	mb <- findTxContentEntry isFuture thread txlogs (metaTxM m)
	case mb of
		Strict.Just (tvar@(DynTxM (buff_dta) txdeps _ txstat) :!: isTop :!: blockid) -> do
			!status <- readIORef txstat
			case status of
				TxStatus (Read _ :!: i) -> if isTop
					then do
						let !newst = status `mappend` st
						!old_value <- readIORef $ dataTxM m
						let !v' = maybe old_value id mbv'
						writeIORef (coerce buff_dta) $! Just $! v'
						writeIORef' txstat newst
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						return tvar
					else do
						let !newst = status `mappend` st
						!old_value <- readIORef $ dataTxM m
						let !v' = maybe old_value id mbv'
						buff_dta' <- newIORef $! Just $! v'
						txdeps' <- WeakMap.copy' txdeps
						txstat' <- newIORef' newst
						let tvar' = DynTxM (buff_dta') txdeps' m txstat'
						addTxLogEntryUp thread txlogs idm tvar'
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						return tvar'
				TxStatus (New _ :!: j) -> if isTop
					then do
						case mbv' of
							Nothing -> return tvar
							Just v' -> do
								let !newst = status `mappend` st
								writeIORef (dataTxM m) $! v'
								writeIORef' txstat newst
								addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
								return tvar
					else do
						let !newst = dirtyStatus st blockid status
						buff_dta' <- case mbv' of
							Nothing -> blankBuffTxM
							Just v' -> newIORef $! Just $! v'
						txdeps' <- WeakMap.new'
						txstat' <- newIORef' newst
						let tvar' = DynTxM buff_dta' txdeps' m txstat'
						addTxLogEntryUp thread txlogs idm tvar'
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						--debugTx' $ "newWriteM " ++ show idm ++ " " ++ show newst
						return tvar'
				otherwise -> if isTop
					then do
						let newst = status `mappend` st
						!buff_value <- liftM (fromJustNote $ "changeTxM " ++ show status) $ readIORef $ coerce $ buff_dta
						let !v' = maybe buff_value id mbv'
						writeIORef (coerce buff_dta) $! Just $! v'
						writeIORef' txstat newst
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						return tvar
					else do
						let newst = status `mappend` st
						!buff_value <- liftM (fromJustNote $ "changeTxM " ++ show status) $ readIORef $ coerce buff_dta
						let !v' = maybe buff_value id mbv'
						buff_dta' <- newIORef $! Just $! v'
						txdeps' <- WeakMap.copy' txdeps
						txstat' <- newIORef' newst
						let tvar' = DynTxM (buff_dta') txdeps' m txstat'
						addTxLogEntryUp thread txlogs idm tvar'
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						return tvar'
		Strict.Nothing -> do
			!old_value <- readIORef $ dataTxM m
			let !v' = maybe old_value id mbv'
			buff_dta <- newIORef' $ Just $! v'
			txdeps <- WeakMap.new'
			txstat <- newIORef' st
			let tvar = DynTxM (buff_dta) txdeps m txstat
			addTxLogEntryUp thread txlogs idm tvar
			addTxNotifies rootThread Nothing st txlogs idm (notifiesTxNM $! metaTxM m)
			return tvar
changeTxM isFuture rootThread thread m _ status _ = error $ "changeTxM " ++ show status

bufferTxU :: (IncK (TxAdapton c) a,TxLayer l c,TxLayer Outside c) => Bool -> ThreadId -> ThreadId -> TxU c l (TxAdapton c) a -> TxStatus -> TxLogs c -> IO (DynTxVar c)
bufferTxU isFuture rootThread thread u st@(TxStatus (Read _ :!: i)) (txlogs :: TxLogs c) = doBlock (Proxy :: Proxy c) $ do 
	let !idu = idTxNM $ metaTxU u
	mb <- findTxContentEntry isFuture thread txlogs (metaTxU u)
	case mb of
		Strict.Just (tvar@(DynTxU (buff_dta) txdeps _ txstat) :!: isTop :!: blockid) -> do
			!status <- readIORef txstat
			case status of
				TxStatus (New _ :!: j) -> if isTop
					then do
						let newst = status `mappend` st
						writeIORef' txstat newst
						addTxNotifies rootThread (Just status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar
					else do
						let !newst = dirtyStatus st blockid status
						buff_dta' <- blankBuffTxU
						txdeps' <- WeakMap.new'
						txstat' <- newIORef' newst
						let tvar' = DynTxU buff_dta' txdeps' u txstat'
						addTxLogEntryUp thread txlogs idu tvar'
						addTxNotifies rootThread (Just status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						--debugTx' $ "newReadU " ++ show idu ++ " " ++ show newst
						return tvar'
				otherwise -> if isTop
					then do
						let !newst = status `mappend` st
						writeIORef' txstat newst
						addTxNotifies rootThread (Just status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar
					else do
						let !newst = status `mappend` st
						buff_dta' <- liftM coerce $ copyBuffTxU buff_dta
						txdeps' <- WeakMap.copy' txdeps
						txstat' <- newIORef' newst
						let tvar' = DynTxU buff_dta' txdeps' u txstat'
						addTxLogEntryUp thread txlogs idu tvar'
						addTxNotifies rootThread (Just status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar'
		Strict.Nothing -> do
			buff_dta <- blankBuffTxU
			txdeps <- WeakMap.new'
			txstat <- newIORef' st
			let tvar = DynTxU buff_dta txdeps u txstat
			addTxLogEntryUp thread txlogs idu tvar
			addTxNotifies rootThread Nothing st txlogs idu (notifiesTxNM $! metaTxU u)
			return tvar
bufferTxU isFuture rootThread thread u st@(TxStatus (Eval _ :!: i)) (txlogs :: TxLogs c) = doBlock (Proxy :: Proxy c) $ do
	let !idu = idTxNM $ metaTxU u
	mb <- findTxContentEntry isFuture thread txlogs (metaTxU u)
	case mb of
		Strict.Just (tvar@(DynTxU buff_dta txdeps _ txstat) :!: isTop :!: blockid) -> do
			!status <- readIORef txstat
			case status of
				TxStatus (New _ :!: j) -> if isTop
					then do
						let !newst = status `mappend` st
						writeIORef' txstat newst
						addTxNotifies rootThread (Just status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar
					else do -- we need to define a layer over the new local variable
						let !newst = dirtyStatus st blockid status
						buff_dta' <- readIORef (coerce $ dataTxU u) >>= copyTxUData >>= newIORef' . Just
						txdeps' <- WeakMap.new'
						txstat' <- newIORef' newst
						let tvar' = DynTxU buff_dta' txdeps' u txstat'
						addTxLogEntryUp thread txlogs idu tvar'
						addTxNotifies rootThread (Just status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						--debugTx' $ "newEvalU " ++ show idu ++ " " ++ show newst
						return tvar'
				TxStatus (Read _ :!: _) -> if isTop
					then do
						let !newst = status `mappend` st
						readIORef (coerce $ dataTxU u) >>= copyTxUData >>= writeIORef' buff_dta . Just
						writeIORef' txstat newst
						addTxNotifies rootThread (Just status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar
					else do
						let !newst = status `mappend` st
						buff_dta' <- readIORef (dataTxU u) >>= copyTxUData >>= newIORef' . Just
						txdeps' <- WeakMap.copy' txdeps
						txstat' <- newIORef' newst
						let tvar' = DynTxU (buff_dta') txdeps' u txstat'
						addTxLogEntryUp thread txlogs idu tvar'
						addTxNotifies rootThread (Just status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar'
				otherwise -> if isTop
					then do
						let !newst = status `mappend` st
						writeIORef' txstat newst
						addTxNotifies rootThread (Just status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar
					else do
						let !newst = status `mappend` st
						buff_dta' <- copyBuffTxU buff_dta
						txdeps' <- WeakMap.copy' txdeps
						txstat' <- newIORef' newst
						let tvar' = DynTxU (coerce buff_dta') txdeps' u txstat'
						addTxLogEntryUp thread txlogs idu tvar'
						addTxNotifies rootThread (Just status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar'
		Strict.Nothing -> do
			!buff_dta <- (readIORef $ dataTxU u) >>= copyTxUData >>= newIORef' . Just
			txdeps <- WeakMap.new'
			txstat <- newIORef' st
			let tvar = DynTxU (buff_dta) txdeps u txstat
			addTxLogEntryUp thread txlogs idu tvar
			addTxNotifies rootThread Nothing st txlogs idu (notifiesTxNM $! metaTxU u)
			return tvar
bufferTxU isFuture rootThread thread u st txlogs = changeTxU isFuture rootThread thread u Nothing st txlogs

-- changes a thunk, or just buffers it
changeTxU :: (IncK (TxAdapton c) a,TxLayer l c,TxLayer Outside c) => Bool -> ThreadId -> ThreadId -> TxU c l (TxAdapton c) a -> Maybe (BuffTxUData c l (TxAdapton c) a -> IO (BuffTxUData c l (TxAdapton c) a)) -> TxStatus -> TxLogs c -> IO (DynTxVar c)
changeTxU isFuture rootThread thread u mbChgDta status@(isEvalOrWrite -> Just (n,i)) (txlogs :: TxLogs c) = doBlock (Proxy :: Proxy c) $ do
	let !idu = idTxNM $ metaTxU u
	mb <- findTxContentEntry isFuture thread txlogs (metaTxU u)
	case mb of
		Strict.Just (tvar@(DynTxU buff_dta txdeps u txstat) :!: isTop :!: blockid) -> do
			!buff_status <- readIORef txstat
			case buff_status of
				TxStatus (Read _ :!: _) -> if isTop
					then do
						let !newst = buff_status `mappend` status
						readIORef (dataTxU u) >>= copyTxUData >>= writeIORef' buff_dta . Just
						modifyIORefM_' (coerce buff_dta) (liftM Just . maybe return id mbChgDta . fromJust)
						writeIORef' txstat newst
						addTxNotifies rootThread (Just buff_status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar
					else do
						let !newst = buff_status `mappend` status
						txdeps' <- WeakMap.copy' txdeps
						!buff_dta' <- readIORef (dataTxU u) >>= copyTxUData >>= newIORef' . Just
						modifyIORefM_' (coerce buff_dta') (liftM Just . maybe return id mbChgDta . fromJust)
						txstat' <- newIORef' newst
						let !tvar' = DynTxU (buff_dta') txdeps' u txstat'
						addTxLogEntryUp thread txlogs idu tvar'
						addTxNotifies rootThread (Just buff_status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar'
						
				TxStatus (New _ :!: j) -> if isTop
					then do
						let !newst = mappend buff_status status
						case mbChgDta of
							Nothing -> return ()
							Just chgDta -> modifyIORefM_' (coerce $ dataTxU u) $ \dta -> do
								(dta' :!: oris') <- chgDta (dta :!: [])
								case dta' of
									TxValue _ _ _ dependencies -> linkTxDependencies dependencies oris'
									otherwise -> return ()
								return dta'
						writeIORef' txstat newst
						addTxNotifies rootThread (Just buff_status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar
					else do -- we can't modify new variables created at another transactional layer
						let !newst = dirtyStatus status blockid buff_status
						!buff_dta' <- readIORef (dataTxU u) >>= copyTxUData >>= newIORef' . Just
						modifyIORefM_' (coerce buff_dta') (liftM Just . maybe return id mbChgDta . fromJust) 
						txdeps' <- WeakMap.new'
						txstat' <- newIORef' newst
						let tvar' = DynTxU (buff_dta') txdeps' u txstat'
						addTxLogEntryUp thread txlogs idu tvar'
						addTxNotifies rootThread (Just buff_status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						--debugTx' $ "newWriteU " ++ show idu ++ " " ++ show newst ++ " " ++ show (txLogId $ Strict.head txlogs)
						return tvar'
						
				otherwise -> if isTop
					then do
						let !newst = mappend buff_status status
						let chg = maybe return id mbChgDta
						modifyIORefM_' (coerce buff_dta) (liftM Just . chg . fromJustNote ("changeTxU " ++ show buff_status))
						writeIORef' txstat newst
						addTxNotifies rootThread (Just buff_status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar
					else do
						let !newst = mappend buff_status status
						!buff_dta' <- copyBuffTxU buff_dta
						let chg = maybe return id mbChgDta
						modifyIORefM_' (coerce buff_dta) (liftM Just . chg . fromJustNote ("changeTxU " ++ show buff_status))
						txstat' <- newIORef' newst
						txdeps' <- WeakMap.copy' txdeps
						let !tvar' = DynTxU (buff_dta') txdeps' u txstat'
						-- all changes are logged on the nested transaction's log
						addTxLogEntryUp thread txlogs idu tvar'
						addTxNotifies rootThread (Just buff_status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar'
						
		Strict.Nothing -> do
			txdeps <- WeakMap.new'
			!buff_dta <- (readIORef $ dataTxU u) >>= copyTxUData >>= newIORef' . Just
			modifyIORefM_' buff_dta $ \(Just dta) -> liftM Just $! maybe return id mbChgDta dta
			txstat <- newIORef' status
			let !tvar = DynTxU (buff_dta) txdeps u txstat
			addTxLogEntryUp thread txlogs idu tvar
			addTxNotifies rootThread Nothing status txlogs idu (notifiesTxNM $! metaTxU u)
			return tvar
changeTxU isFuture rootThread thread u _ status _ = error $ "changeTxU " ++ show status

copyBuffTxU :: BuffTxU c l a -> IO (BuffTxU c l a)
copyBuffTxU buff_dta = do
 	mb <- readIORef' buff_dta
	case mb of
		Just (dta :!: oris) -> do
			case dta of
				TxValue dirty value force dependencies -> do
					dependencies' <- copyIORef' dependencies -- create a new dependencies reference
					newIORef' $ Just $! (TxValue dirty value force dependencies' :!: (dependencies : oris))
				otherwise -> newIORef' $ Just $! (dta :!: oris)
		Nothing -> newIORef' Nothing

-- remembers the original dependencies reference
{-# INLINE copyTxUData #-}
copyTxUData :: TxUData c l (TxAdapton c) a -> IO (BuffTxUData c l (TxAdapton c) a)
copyTxUData dta@(TxValue dirty value force dependencies) = do
	buff_dependencies <- copyIORef' dependencies -- copy the dependencies reference
	return $! (TxValue dirty value force buff_dependencies :!: [dependencies]) -- we use the original dependencies list
copyTxUData dta = return $! (dta :!: [])

coerce :: (Typeable a,Typeable b) => a -> b
#ifndef DEBUG
coerce = Unsafe.unsafeCoerce
#endif
#ifdef DEBUG
coerce = coerce' Proxy where
	coerce' :: (Typeable a,Typeable b) => Proxy b -> a -> b
	coerce' b (x::a) = case cast x of
		Nothing -> error $ "failed coerce: cast " ++ show (typeRep (Proxy :: Proxy a)) ++ " into " ++ show (typeRep b)
		Just y -> y
#endif
{-# INLINE coerce #-}

{-# INLINE findTxContentEntry #-}
findTxContentEntry :: TxLayer Outside c => Bool -> ThreadId -> TxLogs c -> TxNodeMeta c -> IO (Strict.Maybe (DynTxVar c :!: Bool :!: TxLogBlockId))
findTxContentEntry !isFuture !thread !txlogs@(NeStrict.head -> txlog) !meta = {-# SCC findTxContentEntry #-} do
	blocks <- readIORef' $ txLogBuff txlog
	let blockid = txLogId txlog :!: NeStrict.length blocks
	mb_stack <- CMap.lookup thread (contentTxNM meta)
	!r <- case mb_stack of
		Nothing -> if isFuture --variable-local cache for nested threads is built on demand, to avoid explicit snapshotting when the future is created
			then do
				let !uid = idTxNM meta
				mb <- findTxLogEntry txlogs uid -- lookup in the (slower) txlogs
				case mb of
					Strict.Nothing -> return Strict.Nothing
					Strict.Just r@(tvar :!: _ :!: _) -> do -- add result to the variable-local cache for the future's thread
						addTxLogEntryUp thread txlogs uid tvar 
						return mb
			else return Strict.Nothing
		Just stack -> modifyIORefM' stack $ \(topid :!: xs) -> findTxContent' blockid topid xs
	return r
  where
	{-# INLINE findTxContent' #-}
	findTxContent' blockid topid Strict.Nil = return (topid :!: Strict.Nil,Strict.Nothing)
	findTxContent' blockid topid xs@(Strict.Cons w t) = do
		mb <- Weak.deRefWeak w
		case mb of
			Nothing -> findTxContent' blockid topid t
			Just entry -> do
				let isTop = blockid == topid
				return (topid :!: xs,Strict.Just (entry :!: isTop :!: topid))

-- finds a buffered entry in any depth of a nested tx log
{-# INLINE findTxLogEntry #-}
findTxLogEntry :: TxLogs c -> Unique -> IO (Strict.Maybe (DynTxVar c :!: Bool :!: TxLogBlockId))
findTxLogEntry !txlogs !uid = {-# SCC findTxLogEntry #-} findTxLogEntry' txlogs uid True where
	findTxLogEntry' (NeWrap txlog) uid isTop = do
		blocks <- readIORef' $ txLogBuff txlog
		mb_tvar <- findTxBlockEntry' (txLogId txlog) blocks uid isTop
		case mb_tvar of
			Strict.Just tvar -> return mb_tvar
			Strict.Nothing -> return Strict.Nothing
	findTxLogEntry' (NeCons txlog txlogs) uid isTop = do
		blocks <- readIORef' $ txLogBuff txlog
		mb_tvar <- findTxBlockEntry' (txLogId txlog) blocks uid isTop
		case mb_tvar of
			Strict.Just tvar -> return mb_tvar
			Strict.Nothing -> findTxLogEntry' txlogs uid False
	-- only the first block is top; we don't want to modify older blocks
	findTxBlockEntry' logid xs@(NeWrap block) uid isTop = do
		mb <- MemoTable.lookup block uid
		case mb of
			Just tvar -> do
				let blockid = logid :!: NeStrict.length xs
				return $ Strict.Just (tvar :!: isTop :!: blockid)
			Nothing -> return Strict.Nothing
	findTxBlockEntry' logid xs@(NeCons block blocks) uid isTop = do
		mb <- MemoTable.lookup block uid
		case mb of
			Just tvar -> do
				let blockid = logid :!: NeStrict.length xs
				return $! Strict.Just (tvar :!: isTop :!: blockid)
			Nothing -> findTxBlockEntry' logid blocks uid False

{-# INLINE addTxLogEntryUp #-}
addTxLogEntryUp :: TxLayer Outside c => ThreadId -> TxLogs c -> Unique -> DynTxVar c -> IO ()
addTxLogEntryUp !thread !txlogs@(NeStrict.head -> txlog) !uid !(tvar :: DynTxVar c) = {-# SCC addTxLogEntryUp #-} doBlock (Proxy :: Proxy c) $ do
	let !logid = txLogId txlog
	let (MkWeak mkWeak) = dynTxMkWeak tvar
	wtvar <- mkWeak tvar Nothing
	blocks@(NeStrict.head -> block) <- readIORef' $ txLogBuff txlog
	MemoTable.insertWeak block uid wtvar
	stack <- CMap.lookupOrInsert thread (return . Just) (newIORef ((logid :!: 0) :!: Strict.Nil) >>= \x -> return $! (x,x)) (\_ -> return ()) (contentTxNM $ dynTxMeta tvar)
	atomicModifyIORef' stack $ \(_ :!: xs) -> (((logid :!: NeStrict.length blocks) :!: Strict.Cons wtvar xs),())

{-# INLINE addTxLogEntryDown #-}
addTxLogEntryDown :: TxLayer Outside c => ThreadId -> TxLogs c -> Unique -> Weak (DynTxVar c) -> DynTxVar c -> IO ()
addTxLogEntryDown !thread !txlogs@(NeStrict.head -> txlog) !uid !(wtvar :: Weak (DynTxVar c)) !tvar = {-# SCC addTxLogEntryDown #-} doBlock (Proxy :: Proxy c) $ do
	blocks@(NeStrict.head -> block) <- readIORef' $ txLogBuff txlog
	let blockid = txLogId txlog :!: NeStrict.length blocks
	addTxBlockEntryDown thread block blockid uid wtvar tvar
	
{-# INLINE addTxBlockEntryDown #-}
addTxBlockEntryDown :: TxLayer Outside c => ThreadId -> TxLogBlock c -> TxLogBlockId -> Unique -> Weak (DynTxVar c) -> DynTxVar c -> IO ()
addTxBlockEntryDown !thread !block blockid@(logid :!: _) !uid !wtvar !tvar = {-# SCC addTxLogEntryDown #-} do
	commitNestedDynTxVar tvar blockid
	
	MemoTable.insertWeak block uid wtvar
	stack <- CMap.lookupOrInsert thread (return . Just) (newIORef ((logid :!: 0) :!: Strict.Nil) >>= \x -> return $! (x,x)) (\_ -> return ()) (contentTxNM $ dynTxMeta tvar)
	-- just change the top-level blockid
	modifyIORef' stack $ \(_ :!: xs) -> (blockid :!: xs)

-- keep all the older dependency references alive as long as the current dependency reference is alive
linkTxDependencies :: TxDependencies c -> [TxDependencies c] -> IO ()
linkTxDependencies new_dependencies oris = do
	Control.Monad.mapM_ (\ds -> mkWeakRefKey new_dependencies ds Nothing) oris

-- updates a transaction-local new entry with a nested entry
commitNestedDynTxVar :: (TxLayer Outside c) => DynTxVar c -> TxLogBlockId -> IO ()
commitNestedDynTxVar (DynTxU (buff_dta) txdeps u txstat :: DynTxVar c) parent_id = do
	let !proxy = Proxy :: Proxy c
	stat <- readIORef' txstat	
	case stat of
		TxStatus (Read (Strict.Just ((==parent_id) -> True)) :!: b) -> do
			WeakMap.unionWithKey' (dependentsTxNM $ metaTxU u) txdeps
			WeakMap.clean' txdeps
			writeIORef' txstat $ mkNew stat
		TxStatus (Eval (Strict.Just ((==parent_id) -> True)) :!: b) -> do
			WeakMap.unionWithKey' (dependentsTxNM $ metaTxU u) txdeps
			WeakMap.clean' txdeps
			mb <- readIORef' buff_dta
			case mb of
				Just (dta :!: oris) -> do
					case dta of
						TxValue _ _ _ dependencies -> linkTxDependencies dependencies oris
						otherwise -> return ()
					writeIORef' (dataTxU u) dta
				Nothing -> error $ "commitNestedDynTxVar " ++ show stat
			writeIORef buff_dta Nothing
			writeIORef' txstat $ mkNew stat
		TxStatus (Write (Strict.Just ((==parent_id) -> True)) :!: b) -> do
			WeakMap.unionWithKey' (dependentsTxNM $ metaTxU u) txdeps
			WeakMap.clean' txdeps
			mb <- readIORef' buff_dta
			case mb of
				Just (dta :!: oris) -> do
					case dta of
						TxValue _ _ _ dependencies -> linkTxDependencies dependencies oris
						otherwise -> return ()
					writeIORef' (dataTxU u) dta
				Nothing -> error $ "commitNestedDynTxVar " ++ show stat
			writeIORef buff_dta Nothing
			writeIORef' txstat $ mkNew stat
		otherwise -> do
			case isNested stat of
				Strict.Just pid -> debugTx' $ "commitNestedDynTxVar WARNING " ++ show pid ++ " " ++ show parent_id
				Strict.Nothing -> return ()
			return ()
commitNestedDynTxVar (DynTxM (buff_dta) txdeps m txstat :: DynTxVar c) parent_id = do
	let !proxy = Proxy :: Proxy c
	stat <- readIORef' txstat	
	case stat of
		TxStatus (Read (Strict.Just ((==parent_id) -> True)) :!: b) -> do
			WeakMap.unionWithKey' (dependentsTxNM $ metaTxM m) txdeps
			WeakMap.clean' txdeps
			writeIORef' txstat $ mkNew stat
		TxStatus (Eval (Strict.Just ((==parent_id) -> True)) :!: b) -> do
			WeakMap.unionWithKey' (dependentsTxNM $ metaTxM m) txdeps
			WeakMap.clean' txdeps
			mb_buff_value <- readIORef' buff_dta
			case mb_buff_value of
				Nothing -> error $ "commitNestedDynTxVar " ++ show stat
				Just buff_value -> writeIORef' (dataTxM m) $ buff_value
			writeIORef buff_dta Nothing
			writeIORef' txstat $ mkNew stat
		TxStatus (Write (Strict.Just ((==parent_id) -> True)) :!: b) -> do
			WeakMap.unionWithKey' (dependentsTxNM $ metaTxM m) txdeps
			WeakMap.clean' txdeps
			mb_buff_value <- readIORef' buff_dta
			case mb_buff_value of
				Nothing -> error $ "commitNestedDynTxVar " ++ show stat
				Just buff_value -> writeIORef' (dataTxM m) buff_value
			writeIORef buff_dta Nothing
			writeIORef' txstat $ mkNew stat
		otherwise -> do
			case isNested stat of
				Strict.Just pid -> debugTx' $ "commitNestedDynTxVar WARNING " ++ show pid ++ " " ++ show parent_id
				Strict.Nothing -> return ()
			return ()

findTxMContentValue :: (Typeable a,TxLayer Outside c) => Bool -> ThreadId -> TxLogs c -> TxM i c l (TxAdapton c) a -> IO a
findTxMContentValue isFuture thread txlogs m = do
	mb <- findTxContentEntry isFuture thread txlogs (metaTxM m)
	case mb of
		Strict.Just (tvar :!: _ :!: _) -> liftM Strict.fst $ dynTxMValue tvar
		Strict.Nothing -> readIORef' (dataTxM m)

findTxMLogValue :: Typeable a => TxLogs c -> TxM i c l (TxAdapton c) a -> IO a
findTxMLogValue  txlogs m = do
	mb <- findTxLogEntry txlogs (idTxNM $ metaTxM m)
	case mb of
		Strict.Just (tvar :!: _ :!: _) -> liftM Strict.fst $ dynTxMValue tvar
		Strict.Nothing -> readIORef' (dataTxM m)

-- conflates all the blocks of a txlog
flattenTxLogBlocks :: TxLayer Outside c => ThreadId -> TxLog c -> IO (TxLogBlock c)
flattenTxLogBlocks thread (txlog :: TxLog c) = doBlock (Proxy :: Proxy c) $ do
	blocks <- readIORef $ txLogBuff txlog
	block <- flattenTxLogBlocks' thread (txLogId txlog) blocks
	writeIORef' (txLogBuff txlog) (NeWrap block)
	return block

flattenTxLogBlocks' :: TxLayer Outside c => ThreadId -> TxLogId -> NeList (TxLogBlock c) -> IO (TxLogBlock c)
flattenTxLogBlocks' thread logid (NeWrap b) = return b
flattenTxLogBlocks' thread logid (NeCons b1 bs@(NeStrict.head -> b2)) = do
	let add (uid,wentry) = do
		mb <- Weak.deRefWeak wentry
		case mb of
			Nothing -> return ()
			Just entry -> addTxBlockEntryDown thread b2 (logid :!: succ (NeStrict.length bs)) uid wentry entry
	MemoTable.mapWeakM_ add b1
	mkWeakKey b2 b2 $! Just $! MemoTable.finalize b1
	flattenTxLogBlocks' thread logid bs

-- merges a nested txlog with its parent txlog, by overriding parent entries
-- unbuffer writes if @doWrites@ is set to @False@
{-# INLINE mergeTxLog #-}
mergeTxLog :: TxLayer Outside c => Bool -> Bool -> ThreadId -> TxLog c -> TxLogs c -> IO ()
mergeTxLog doWrites !isFuture !thread txlog1 txlogs2@(NeStrict.head -> txlog2) = flattenTxLogBlocks thread txlog1 >>= MemoTable.mapWeakM_ updEntry where
	updEntry (uid,wentry) = do
		mb <- Weak.deRefWeak wentry
		case mb of
			Nothing -> return ()
			Just entry -> do
				unless doWrites $ unbufferDynTxVar'' isFuture thread (NeCons txlog1 txlogs2) True entry
				addTxLogEntryDown thread txlogs2 uid wentry entry

-- merge the second variable over the first one
mergeDynTxVar :: Bool -> DynTxVar c -> DynTxVar c -> IO ()
mergeDynTxVar overwriteData (DynTxM (buff_dta1) buff_deps1 m1 stat1) (DynTxM (buff_dta2) buff_deps2 m2 stat2) = do
	when overwriteData $ readIORef' buff_dta2 >>= writeIORef' buff_dta1 . coerce
	WeakMap.unionWithKey' buff_deps1 buff_deps2
	modifyIORefM_' stat1 $ \s1 -> readIORef' stat2 >>= \s2 -> return (mappend s1 s2)
mergeDynTxVar overwriteData (DynTxU (buff_dta1) buff_deps1 m1 stat1) (DynTxU (buff_dta2) buff_deps2 m2 stat2) = do	
	when overwriteData $ readIORef' buff_dta2 >>= writeIORef' buff_dta1 . coerce
	WeakMap.unionWithKey' buff_deps1 buff_deps2
	modifyIORefM_' stat1 $ \s1 -> readIORef' stat2 >>= \s2 -> return (mappend s1 s2)

diffTxLog :: TxLog c -> TxLog c -> IO (TxLog c)
diffTxLog (TxLog (uid :!: buff1 :!: memos)) (TxLog (_ :!: buff2 :!: _)) = do
	blocks1 <- readIORef' buff1
	blocks2 <- readIORef' buff2
	blocks' <- newIORef' $ NeStrict.take (NeStrict.length blocks1 - NeStrict.length blocks2) blocks1
	return $ TxLog (uid :!: blocks' :!: memos)

-- ** dirtying

{-# INLINE forgetBufferedDynTxVar #-}
forgetBufferedDynTxVar :: TxLayer Outside c => Bool -> ThreadId -> TxLogs c -> DynTxVar c -> IO ()
forgetBufferedDynTxVar = dirtyForgetBufferedDynTxVar True

{-# INLINE dirtyBufferedDynTxVar #-}
dirtyBufferedDynTxVar :: TxLayer Outside c => Bool -> ThreadId -> TxLogs c -> DynTxVar c -> IO ()
dirtyBufferedDynTxVar = dirtyForgetBufferedDynTxVar False

{-# INLINE dirtyForgetBufferedDynTxVar #-}
dirtyForgetBufferedDynTxVar :: TxLayer Outside c => Bool -> Bool -> ThreadId -> TxLogs c -> DynTxVar c -> IO ()
dirtyForgetBufferedDynTxVar forget !isFuture !thread txlogs tvar = do
	if forget then forgetBufferedTxData thread tvar else dirtyBufferedTxData tvar
	dirtyCreatorBufferedTx isFuture thread txlogs (dynVarTxCreator tvar)
	dirtyRecursivelyBufferedDynTxVar isFuture thread txlogs tvar

dirtyCreatorBufferedTx :: TxLayer Outside c => Bool -> ThreadId -> TxLogs c -> Maybe (TxCreator c) -> IO ()
dirtyCreatorBufferedTx !isFuture !thread txlogs Nothing = return ()
dirtyCreatorBufferedTx !isFuture !thread txlogs (Just wcreator) = do
	mb <- deRefWeak wcreator
	case mb of
		Just creatorMeta -> do
			mb <- findTxContentEntry isFuture thread txlogs (creatorMeta)
			case mb of
				Strict.Nothing -> return ()
				Strict.Just (creator_var :!: isTop :!: blockid) -> do
					forgetBufferedTxData thread creator_var
					dirtyRecursivelyBufferedDynTxVar isFuture thread txlogs creator_var
		Nothing -> return ()

-- dirties only buffered dependents in a txlog, without changing the status of their variables
dirtyRecursivelyBufferedDynTxVar :: TxLayer Outside c => Bool -> ThreadId -> TxLogs c -> DynTxVar c -> IO ()
dirtyRecursivelyBufferedDynTxVar isFuture !thread txlogs tvar = do
	debugTx2' $ "dirtyRecursivelyBufferedDynTxVar " ++ show (dynTxId tvar)
	buff_dependents <- dynTxVarBufferedDependents tvar
	WeakMap.mapWeakM_ dirtyTx' buff_dependents
  where
	dirtyTx' = \(_,w) -> do
		mb <- Weak.deRefWeak w
		case mb of
			Nothing -> return ()
			Just d -> do -- buffered dependents are never original
				!isDirty <- readIORef (dirtyTxW d)
-- #ifdef DEBUG
				!isOriginal <- readIORef (originalTxW d)
				when isOriginal $ error "original dependent found"
-- #endif
				unless isDirty $ do -- stop when the dependency is original or already dirty
					writeIORef (dirtyTxW d) $! True -- dirty the buffered dependency
					mb <- findTxContentEntry isFuture thread txlogs (tgtMetaTxW d)
					case mb of
						Strict.Nothing -> return () -- stop when the dependent variable is not buffered
						Strict.Just (tgt_tvar :!: isTop :!: blockid) -> do
							dirtyBufferedTxData tgt_tvar
							dirtyRecursivelyBufferedDynTxVar isFuture thread txlogs tgt_tvar

-- dirties a buffered variable in-place without changing its status
dirtyBufferedTxData :: DynTxVar c -> IO ()
dirtyBufferedTxData (DynTxU (buff_dta) _ u txstat) = do
	!stat <- readIORef txstat
	case stat of
		TxStatus (New _ :!: _) -> modifyIORefM_' (dataTxU u) chgDirty
		TxStatus (Read _ :!: _) -> return ()
		otherwise -> modifyIORef' buff_dta chgDirtyBuff
  where
	chgDirty (TxValue _ value force dependencies) = return $ TxValue 1# value force dependencies
	chgDirty dta = return dta
	chgDirtyBuff (Just (TxValue _ value force dependencies :!: oris)) = Just $! (TxValue 1# value force dependencies :!: oris)
	chgDirtyBuff dta = dta
dirtyBufferedTxData _ = return ()

forgetBufferedTxData :: ThreadId -> DynTxVar c -> IO ()
forgetBufferedTxData thread (DynTxU (buff_dta) _ u txstat) = do
	!stat <- readIORef txstat
	-- unmemoize this thunk from the buffered memotables for this thread
	let unmemo = do
		(_ :!: buff_unmemos) <- readMVar (unmemoTxNM $ metaTxU u)
		case Map.lookup thread buff_unmemos of
			Nothing -> return ()
			Just unmemo -> unmemo
	case stat of
		TxStatus (New _ :!: _) -> unmemo >> modifyIORefM_' (dataTxU u) forget
		TxStatus (Read _ :!: _) -> return ()
		otherwise -> unmemo >> modifyIORefM_' buff_dta forgetBuff
  where
	forget (TxValue _ value force dependencies) = do
		clearTxDependencies False dependencies
		return $ TxThunk force
	forget dta = return dta	
	forgetBuff (Just (TxValue _ value force dependencies :!: oris)) = do
		clearTxDependencies False dependencies
		return $ Just $! (TxThunk force :!: [])
	forgetBuff dta = return dta
forgetBufferedTxData thread _ = return ()

isUnevaluatedOriginalTxU :: TxLayer l c => TxU c l (TxAdapton c) a -> IO Bool
isUnevaluatedOriginalTxU t = do
	!dta <- readIORef (dataTxU t)
	case dta of
		TxThunk force -> return True --unevaluated thunk
		otherwise -> return False

-- ** unbuffering

-- don't dirty unbuffered entries (since we are unbuffering all written variables, dependents of unbuffered entries are unbuffered as well)
{-# INLINE unbufferTopTxLog #-}
unbufferTopTxLog :: (TxLayer Outside c,TxLayer l c) => Bool -> ThreadId -> TxLogs c -> Bool -> l (TxAdapton c) ()
unbufferTopTxLog !isFuture !thread txlogs !onlyWrites = do
	b <- isInside
	let txlog = NeStrict.head txlogs
	-- we don't need to unbuffer a computation at the inner layer, because it has no writes
	unless (onlyWrites && b) $ unsafeIOToInc $ flattenTxLogBlocks thread txlog >>= MemoTable.mapM_ (unbufferDynTxVar' isFuture thread onlyWrites txlogs)

{-# INLINE unbufferDynTxVar #-}
unbufferDynTxVar :: TxLayer Outside c => Bool -> ThreadId -> Bool -> TxLogs c -> DynTxVar c -> IO ()
unbufferDynTxVar !isFuture !thread !onlyWrites txlogs !entry = unbufferDynTxVar' isFuture thread onlyWrites txlogs (dynTxId entry,entry)
			
unbufferTxVar :: TxLayer Outside c => Bool -> ThreadId -> Bool -> TxLogs c -> TxNodeMeta c -> IO ()
unbufferTxVar isFuture thread onlyWrites txlogs meta = do
	mb <- findTxContentEntry isFuture thread txlogs meta
	case mb of
		Strict.Just (tvar :!: isTop :!: blockid) -> do
--			debugTx' $ "unbufferingTx " ++ show onlyWrites ++ "  " ++ show uid ++ " " ++ show (dynTxStatus tvar)
			unbufferDynTxVar' isFuture thread onlyWrites txlogs (idTxNM meta,tvar)
		Strict.Nothing -> do
--			debugTx' $ "nonbufferingTx " ++ show onlyWrites ++ "  " ++ show uid
			return ()

unbufferDynTxVar' :: TxLayer Outside c => Bool -> ThreadId -> Bool -> TxLogs c -> (Unique,DynTxVar c) -> IO ()
unbufferDynTxVar' isFuture thread onlyWrites txlogs (uid,entry) = do
	!stat <- readIORef $ dynTxStatus entry
	when (if onlyWrites then isWriteOrNewTrue stat else True) $ do
		unbufferDynTxVar'' isFuture thread txlogs onlyWrites entry

-- we need to clear the dependencies of unbuffered variables
-- buffered dependents are ALWAYS preserved, but dirtied in case we discard the buffered data
unbufferDynTxVar'' :: TxLayer Outside c => Bool -> ThreadId -> TxLogs c -> Bool -> DynTxVar c -> IO ()
unbufferDynTxVar'' isFuture thread txlogs onlyWrites tvar@(DynTxU buff_dta txdeps u txstat) = do
	!stat <- readIORef txstat
	case stat of
		-- we don't unbuffer the value of New variables; New variables cannot have original dependents
		TxStatus (New i :!: b) -> when (if onlyWrites then i else True) $ do 
--			debugTx' $ "unbuffered " ++ show stat ++ " " ++ show (idTxNM $ metaTxU u)
			dirtyBufferedDynTxVar isFuture thread txlogs tvar
			writeIORef' txstat $ TxStatus (New False :!: b)
		otherwise -> when ((if onlyWrites then isWriteOrNewTrue else Prelude.const True) stat) $ do
--			debugTx' $ "unbuffered " ++ show stat ++ " " ++ show (idTxNM $ metaTxU u)
			case isNested stat of
				nested@(Strict.Just _) -> do -- we can't remove the buffered data
					dirtyBufferedDynTxVar isFuture thread txlogs tvar
				nested@Strict.Nothing -> do
					forgetBufferedDynTxVar isFuture thread txlogs tvar
					-- unbuffer the thunk, so that the fresher original data is used instead
					hasDeps <- WeakMap.null' txdeps
					writeIORef' txstat $ TxStatus (Read nested :!: hasDeps)
					writeIORef buff_dta Nothing
unbufferDynTxVar'' isFuture thread txlogs onlyWrites tvar@(DynTxM buff_dta txdeps m txstat) = do
	!stat <- readIORef txstat
	case stat of
		TxStatus (New i :!: b) -> when (if onlyWrites then i else True) $ do
--			debugTx' $ "unbuffered " ++ show stat ++ " " ++ show (idTxNM $ metaTxM m)
			dirtyBufferedDynTxVar isFuture thread txlogs tvar
			writeIORef' txstat $ TxStatus (New False :!: b)
		otherwise -> when ((if onlyWrites then isWriteOrNewTrue else Prelude.const True) stat) $ do
--			debugTx' $ "unbuffered " ++ show stat ++ " " ++ show (idTxNM $ metaTxM m)
			case isNested stat of
				nested@(Strict.Just _) -> do -- we can't remove the buffered data
					dirtyBufferedDynTxVar isFuture thread txlogs tvar
				nested@Strict.Nothing -> do
					forgetBufferedDynTxVar isFuture thread txlogs tvar
					hasDeps <- WeakMap.null' txdeps
					writeIORef' txstat $ TxStatus (Read nested :!: hasDeps)
					writeIORef buff_dta Nothing
	
{-# INLINE clearTxDependencies #-}
-- clear only buffered dependencies
clearTxDependencies :: Bool -> TxDependencies c -> IO ()
clearTxDependencies doAll = \r -> readIORef r >>= Foldable.mapM_ clearDependency where
	clearDependency !(d,w) = if doAll
		then Weak.finalize w
		else do
			!isOriginal <- readIORef (originalTxW d)
			unless isOriginal $ Weak.finalize w

dynTxStatus :: DynTxVar c -> IORef TxStatus
dynTxStatus (DynTxU _ _ _ s) = s
dynTxStatus (DynTxM _ _ _ s) = s
{-# INLINE dynTxStatus #-}

dynTxNotifies :: DynTxVar c -> TxNotifies c
dynTxNotifies = notifiesTxNM . dynTxMeta
{-# INLINE dynTxNotifies #-}

dynTxMeta :: DynTxVar c -> TxNodeMeta c
dynTxMeta (DynTxU _ _ u _) = metaTxU u
dynTxMeta (DynTxM _ _ m _) = metaTxM m
{-# INLINE dynTxMeta #-}

dynTxId :: DynTxVar c -> Unique
dynTxId = idTxNM . dynTxMeta
{-# INLINE dynTxId #-}

dynTxMkWeak :: DynTxVar c -> MkWeak
dynTxMkWeak (DynTxU _ _ u _) = MkWeak $ mkWeakRefKey $! dataTxU u
dynTxMkWeak (DynTxM _ _ m _) = MkWeak $ mkWeakRefKey $! dataTxM m
{-# INLINE dynTxMkWeak #-}

-- gets a buffered dependents set
{-# INLINE getBufferedTxDependents #-}
getBufferedTxDependents :: Bool -> ThreadId -> ThreadId -> TxLogs c -> TxNodeMeta c -> TxStatus -> IO (TxDependents c)
getBufferedTxDependents isFuture rootThread thread tbl meta status = bufferTxNM meta isFuture rootThread thread status tbl >>= dynTxVarBufferedDependents

-- gets the union of the original and buffered dependents set
{-# INLINE getTxDependents #-}
getTxDependents :: Bool -> ThreadId -> ThreadId -> TxLogs c -> TxNodeMeta c -> TxStatus -> IO (TxDependents c,HashMap Unique (TxDependent c))
getTxDependents isFuture rootThread thread tbl meta status = do
	var <- bufferTxNM meta isFuture rootThread thread status tbl
	dynTxVarDependents var

{-# INLINE getTxDependencies #-}
getTxDependencies :: Bool -> ThreadId -> ThreadId -> TxLogs c -> TxNodeMeta c -> TxStatus -> IO (Maybe (TxDependencies c))
getTxDependencies isFuture rootThread thread tbl meta status = bufferTxNM meta isFuture rootThread thread status tbl >>= dynTxVarDependencies

proxyTxAdapton :: Proxy c -> Proxy (TxAdapton c)
proxyTxAdapton c = Proxy 
{-# INLINE proxyTxAdapton #-}

instance DeepTypeable TxM where
	typeTree _ = MkTypeTree (mkName "Control.Concurrent.Transactional.TxAdapton.Types.TxM") [] []

instance (DeepTypeable i,DeepTypeable c) => DeepTypeable (TxM i c) where
	typeTree _ = MkTypeTree (mkName "Control.Concurrent.Transactional.TxAdapton.Types.TxM") [typeTree (Proxy::Proxy i),typeTree (Proxy::Proxy c)] []

instance (DeepTypeable i,DeepTypeable c,DeepTypeable l,DeepTypeable inc,DeepTypeable a) => DeepTypeable (TxM i c l inc a) where
	typeTree (_ :: Proxy (TxM i c l inc a)) = MkTypeTree (mkName "Control.Concurrent.Transactional.TxAdapton.Types.TxM") args [MkConTree (mkName "Control.Concurrent.Transactional.TxAdapton.mod") [typeTree (Proxy::Proxy a)]]
		where args = [typeTree (Proxy::Proxy i),typeTree (Proxy::Proxy c),typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy a)]

instance DeepTypeable TxU where
	typeTree _ = MkTypeTree (mkName "Control.Concurrent.Transactional.TxAdapton.Types.TxU") [] []

instance DeepTypeable c => DeepTypeable (TxU c) where
	typeTree _ = MkTypeTree (mkName "Control.Concurrent.Transactional.TxAdapton.Types.TxU") [typeTree (Proxy::Proxy c)] []

instance (DeepTypeable c,DeepTypeable l,DeepTypeable inc,DeepTypeable a) => DeepTypeable (TxU c l inc a) where
	typeTree (_ :: Proxy (TxU c l inc a)) = MkTypeTree (mkName "Control.Concurrent.Transactional.TxAdapton.Types.TxU") args [MkConTree (mkName "Control.Concurrent.Transactional.TxAdapton.thunk") [typeTree (Proxy::Proxy a)]]
		where args = [typeTree (Proxy::Proxy c),typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy a)]

type STxAdaptonM c = STxM (TxAdapton c)

instance Monoid (TxRepair c) where
	mempty = TxRepair $ return $! ()
	{-# INLINE mempty #-}
	mappend (TxRepair f) (TxRepair g) = TxRepair $ f >> g
	{-# INLINE mappend #-}

{-# SPECIALIZE doBlock :: Proxy EarlyConflict -> IO a -> IO a #-}
{-# SPECIALIZE doBlock :: Proxy CommitConflict -> IO a -> IO a #-}
{-# SPECIALIZE doBlock :: Proxy EarlyConflict -> Inside (TxAdapton EarlyConflict) a -> Inside (TxAdapton EarlyConflict) a #-}
{-# SPECIALIZE doBlock :: Proxy EarlyConflict -> Outside (TxAdapton EarlyConflict) a -> Outside (TxAdapton EarlyConflict) a #-}
{-# SPECIALIZE doBlock :: Proxy CommitConflict -> Inside (TxAdapton CommitConflict) a -> Inside (TxAdapton CommitConflict) a #-}
{-# SPECIALIZE doBlock :: Proxy CommitConflict -> Outside (TxAdapton CommitConflict) a -> Outside (TxAdapton CommitConflict) a #-}

instance (MonadMask m,MonadIO m) => MonadBlock EarlyConflict m where
	doBlock _ = Catch.uninterruptibleMask_
	{-# INLINE doBlock #-}
instance Monad m => MonadBlock CommitConflict m where
	doBlock _ = id
	{-# INLINE doBlock #-}

{-# INLINE doBlockTx #-}
doBlockTx :: TxLayer l c => l (TxAdapton c) a -> l (TxAdapton c) a
doBlockTx !(m :: l (TxAdapton c) a) = do
	let !l = Proxy :: Proxy l
	let !c = Proxy :: Proxy c
	txLayer l $! doBlock c $! unTxLayer l m

instance Monoid a => Monoid (IO a) where
	mempty = return $! mempty
	mappend io1 io2 = io1 >> io2

-- eval locks: to force concurrent writes to wait until the eval is written to the variable
-- 1 = read, just wait, no lock
-- 2 = eval, optional commit, optional lock
-- 3 = write, mandatory lock
txStatusType :: TxStatus -> Int
txStatusType (TxStatus (Read Strict.Nothing :!: False)) = 1 
txStatusType (TxStatus (New False :!: _)) = 2
txStatusType (TxStatus (Read Strict.Nothing :!: True)) = 2 
txStatusType (TxStatus (Eval Strict.Nothing :!: _)) = 2 
txStatusType (TxStatus (Write Strict.Nothing :!: _)) = 3
txStatusType (TxStatus (New True :!: _)) = 3 
txStatusType st = error $ "txStatusType: " ++ show st
{-# INLINE txStatusType #-}

-- we acquire locks in sorted order, and release reads right after acquiring all locks
-- if we encounter locked resources, then we release all acquired locks, leave the masked state to be able to receive asynchronous exceptions, and try again from the start.
withTxLocksE :: (TxLayer Outside c,TxLayer l c) => (Bool -> l (TxAdapton c) a) -> l (TxAdapton c) a
withTxLocksE f = do
	readOnly <- isInside
	thread <- unsafeIOToInc myThreadId
	
	NeWrap txlog <- readTxLogs
	let addLock lcks (uid,tvar) = do
		!lockn <- liftM (txStatusType) $ readIORef $ dynTxStatus tvar
		let !lck = dynTxVarLock tvar
		return $ Map.insert uid (lck :!: lockn) lcks
	lcks <- liftM Map.toAscList $ unsafeIOToInc $ flattenTxLogBlocks thread txlog >>= MemoTable.foldM addLock Map.empty
	
	-- read-only transactions have optional evals
	-- read-writes transactions need all evaluated dependents to be commited
	let !next = if readOnly then False else True
	let go isFst = withTxLocks' lcks thread isFst f >>= maybe (unsafeIOToInc (yield >> threadDelay 1000) >> go False) return
	go True
  where
	{-# INLINE lockType #-}
	lockType (1,_) = False
	lockType (2,True) = True
	lockType (2,False) = False
	lockType (3,_) = True

	waitOrAcquireLocks :: Bool -> [(Unique,Lock :!: Int)] -> ([Lock],[Lock]) -> IO (Maybe ([Lock],[Lock]))
	waitOrAcquireLocks doEvals [] res = return $! Just res
	waitOrAcquireLocks doEvals ((_,lck :!: lockn):xs) (waiting,acquired) = do
		b <- Lock.tryAcquire lck
		if b
			then do
				let lockt = lockType (lockn,doEvals)
				if lockt then waitOrAcquireLocks doEvals xs (waiting,lck:acquired) else waitOrAcquireLocks doEvals xs (lck:waiting,acquired)
			else Lock.releases waiting >> Lock.releases acquired >> return Nothing

	{-# INLINE withTxLocks' #-}
	withTxLocks' :: (TxLayer Outside c,TxLayer l c) => [(Unique,Lock :!: Int)] -> ThreadId -> Bool -> (Bool -> l (TxAdapton c) a) -> l (TxAdapton c) (Maybe a)
	withTxLocks' lcks thread doEvals m = doBlockTx $ do
		ok <- unsafeIOToInc $ waitOrAcquireLocks doEvals lcks ([],[])
		case ok of
			Just (waiting,acquired) -> unsafeIOToInc (Lock.releases waiting) >> m doEvals >>= \x -> unsafeIOToInc (Lock.releases acquired) >> (return $! Just x)
			Nothing -> return Nothing
		
-- we acquire locks in sorted order, and release reads right after acquiring all locks
-- blocking version: we do not release acquired locks and wait on locks to be free
withTxLocksC :: (TxLayer Outside c,TxLayer l c) => (Bool -> l (TxAdapton c) a) -> l (TxAdapton c) a
withTxLocksC f = do
	readOnly <- isInside
	thread <- unsafeIOToInc myThreadId
	
	NeWrap txlog <- readTxLogs
	let addLock lcks (uid,tvar) = do
		!lockn <- liftM (txStatusType) $ readIORef $ dynTxStatus tvar
		let !lck = dynTxVarLock tvar
		return $ Map.insert uid (lck :!: lockn) lcks
	lcks <- liftM Map.toAscList $ unsafeIOToInc $ flattenTxLogBlocks thread txlog >>= MemoTable.foldM addLock Map.empty
	
	withTxLocks' lcks thread True f 
  where
	{-# INLINE lockType #-}
	lockType = (/= 1)

	waitOrAcquireLocks :: [(Unique,Lock :!: Int)] -> ([Lock],[Lock]) -> IO ([Lock],[Lock])
	waitOrAcquireLocks [] res = return res
	waitOrAcquireLocks ((_,lck :!: lockn):xs) (waiting,acquired) = do
		Lock.acquire lck
		let lockt = lockType lockn
		if lockt then waitOrAcquireLocks xs (waiting,lck:acquired) else waitOrAcquireLocks xs (lck:waiting,acquired)

	{-# INLINE withTxLocks' #-}
	withTxLocks' :: (TxLayer Outside c,TxLayer l c) => [(Unique,Lock :!: Int)] -> ThreadId -> Bool -> (Bool -> l (TxAdapton c) a) -> l (TxAdapton c) a
	withTxLocks' lcks thread doEvals m = doBlockTx $ do
		(waiting,acquired) <- unsafeIOToInc $ waitOrAcquireLocks lcks ([],[])
		unsafeIOToInc (Lock.releases waiting) >> m doEvals >>= \x -> unsafeIOToInc (Lock.releases acquired) >> (return x)



