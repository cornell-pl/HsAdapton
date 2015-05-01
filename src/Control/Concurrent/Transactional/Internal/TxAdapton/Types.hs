{-# LANGUAGE CPP, MultiParamTypeClasses, UndecidableInstances, FlexibleInstances, DataKinds, Rank2Types, TypeFamilies, TupleSections, StandaloneDeriving, BangPatterns, EmptyDataDecls, FlexibleContexts, TypeOperators, ConstraintKinds, MagicHash, ViewPatterns, KindSignatures, GADTs, ScopedTypeVariables, DeriveDataTypeable, TemplateHaskell #-}

module Control.Concurrent.Transactional.Internal.TxAdapton.Types where

import Data.Time.Clock
import Control.Monad.Incremental
import Control.Concurrent.Future
import Control.Concurrent hiding (readMVar)
import Control.Concurrent.Transactional
import qualified Control.Concurrent.Map as CMap
import Control.Monad.Incremental.Internal.Adapton.Types
import Data.Concurrent.Deque.Class as Queue
import Data.Concurrent.Deque.Reference.DequeInstance
import Data.Time
import Control.Concurrent.Chan
import qualified Control.Concurrent.Map as CMap
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
import Data.Strict.Maybe as Strict
import Data.Strict.List as Strict
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

-- if an inner tx validation fails, then we throw an @InvalidTx@ exception to retry the whole atomic block
data InvalidTx c = InvalidTx (InvalidTxRepair c) deriving (Typeable)
instance Show (InvalidTx EarlyConflict) where
	show (InvalidTx (time :!: repair)) = "InvalidTxEarly " ++ show time
instance Show (InvalidTx CommitConflict) where
	show (InvalidTx (time :!: repair)) = "InvalidTxCommit" ++ show time

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

readRootTx :: TxLayer l c => l (TxAdapton c) (Bool,ThreadId)
readRootTx = do
	(s :!: parent :!: fs :!: x :!: y :!: z) <- Reader.ask
	case parent of
		Just tid -> return (True,tid)
		Nothing -> unsafeIOToInc $ liftM (False,) $ myThreadId

readFuturesTx :: TxLayer l c => l (TxAdapton c) (IORef (Map ThreadId (DynTxFuture c)))
readFuturesTx = do
	(s :!: parent :!: fs :!: x :!: y :!: z) <- Reader.ask
	return fs

{-# INLINE readTxStack #-}
readTxStack :: TxLayer l c => l (TxAdapton c) (TxCallStack c)
readTxStack = liftM (\(!(s :!: parent :!: fs :!: x :!: y :!: z)) -> y) $ Reader.ask

{-# INLINE readTxParams #-}
readTxParams :: TxLayer l c => l (TxAdapton c) (IncParams (TxAdapton c))
readTxParams = liftM (\(!(s :!: parent :!: fs :!: x :!: y :!: z)) -> s) $ Reader.ask

{-# INLINE readTxLogs #-}
readTxLogs :: TxLayer l c => l (TxAdapton c) (TxLogs c)
readTxLogs = liftM (\(!(s :!: parent :!: fs :!: x :!: y :!: z)) -> z) $ Reader.ask

{-# INLINE readTxId #-}
readTxId :: TxLayer l c => l (TxAdapton c) (TxId c)
readTxId = liftM (\(!(s :!: parent :!: fs :!: x :!: y :!: z)) -> x) Reader.ask >>= unsafeIOToInc . readIORef

{-# INLINE readTxIdRef #-}
readTxIdRef :: TxLayer l c => l (TxAdapton c) (IORef (TxId c))
readTxIdRef = liftM (\(!(s :!: parent :!: fs :!: x :!: y :!: z)) -> x) Reader.ask

type Wakes = Map Unique Lock

type TxCreator c = WTxNodeMeta c
type WTxNodeMeta c = Weak (TxNodeMeta c)

newtype TxStatus = TxStatus (TxStatusVar :!: Bool) deriving (Eq,Show,Typeable)
data TxStatusVar = Read !(Maybe Unique) | Eval !(Maybe Unique) | Write !(Maybe Unique) | New !Bool deriving (Eq,Show,Typeable)
-- Read | Eval | Write can be nested (boolean = True); this means that they work over a transaction-local variable
-- read does not copy the original data, and just logs that it has been accessed
-- False: no buffered dependents
-- True: buffered dependents
-- eval is a non-conflicting write; we compute data that is consistent with the original state
-- write is a write to a modifiable or dirtying of a thunk
-- new is for new transaction allocations; the boolean denotes whether the new variable depends on a written variable or not

instance Monoid TxStatusVar where
	mempty = Read Nothing
	mappend (Read n1) (Read n2) = Read (max n1 n2)
	mappend (Read n1) (Eval n2) = Eval (max n1 n2)
	mappend (Read n1) (Write n2) = Write (max n1 n2)
	mappend (Read n1) (New b) = New b
	mappend (Eval n1) (Read n2) = Eval (max n1 n2)
	mappend (Eval n1) (Eval n2) = Eval (max n1 n2)
	mappend (Eval n1) (Write n2) = Write (max n1 n2)
	mappend (Eval n1) (New b) = New b
	mappend (Write n1) (Read n2) = Write (max n1 n2)
	mappend (Write n1) (Eval n2) = Write (max n1 n2)
	mappend (Write n1) (Write n2) = Write (max n1 n2)
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

defaultTxResolve :: (IsolationKind i,Layer Inside inc) => Proxy i -> Proxy inc -> Proxy a -> TxResolve i inc a
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
type TxContent c = CMap.Map ThreadId (IORef (Unique :!: SList (Weak (DynTxVar c))))

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

-- (a new starttime,repairing actions over an invalid environment)
type InvalidTxRepair c = (TxId c :!: TxRepair c)

newtype TxRepair c = TxRepair (forall l . TxLayer l c => l (TxAdapton c) ())

-- a wait queue of tx locks and environments
-- thread-safe, because a retrying thread that only reads a variable places itself in the queue (and reads are not locked)
type WaitQueue = Deque Threadsafe Threadsafe SingleEnd SingleEnd Grow Safe Lock

-- a reference holding buffered data and maybe a pointer to the original dependencies reference (either the original references (keeping them alive) or a weak pointer (not keeping them alive))
-- buffered dependends EXTEND the original dependents
type BuffTxUData c l inc a = (TxUData c l inc a , (Either (TxDependencies c) (Weak (TxDependencies c))))

newtype BuffTxU (c :: TxConflict) (l :: * -> * -> *) a = BuffTxU { unBuffTxU :: IORef (BuffTxUData c l (TxAdapton c) a) } deriving Typeable

blankBuffTxU = liftM BuffTxU $ newIORef $ error "BuffTxU"
blankBuffTxM = liftM BuffTxM $ newIORef $ error "BuffTxM"

newtype BuffTxM (c :: TxConflict) (l :: * -> * -> *) a = BuffTxM { unBuffTxM :: IORef a } deriving Typeable

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
	DynTxU :: (IncK (TxAdapton c) a,TxLayer l c) => (BuffTxU c l a) -> (TxDependents c) -> !(TxU c l (TxAdapton c) a) -> !(IORef TxStatus) -> DynTxVar c
	DynTxM :: (IncK (TxAdapton c) a,TxLayer l c,IsolationKind i) => (BuffTxM c l a) -> (TxDependents c) -> !(TxM i c l (TxAdapton c) a) -> !(IORef TxStatus) -> DynTxVar c
--	DynTxL :: Maybe (BuffTxL l inc a) -> TxL l inc a -> TxStatus -> DynTxVar

data DynTxFuture c where
	DynTxFuture :: FutureK (Outside (TxAdapton c)) a => TxFuture c a -> DynTxFuture c

-- we use a lock-free map since multiple threads may be accessing it to register/unregister new txlogs
-- a persistent memo table and a family of buffered (transaction-local) memo tables indexed by a buffered tx log
-- note that added entries to the memotable are always New (hence we have TxU and not BuffTxU in buffered tables)
type TxMemoTable c k b = (MemoTable k (TxU c Inside (TxAdapton c) b) :!: CMap.Map ThreadId (TxBuffMemoTables c k b))

type TxBuffMemoTables c k b = IORef (Map Unique [TxBuffMemoTable c k b])

data DynTxMemoTable c where
	DynTxMemoTable :: (Eq k,Hashable k) => TxMemoTable c k b -> DynTxMemoTable c

-- an IORef is just fine, because the stack is transaction-local
-- the dependencies references in the stack are always buffered copies
type TxCallStack c = IORef (TxCallStack' c)
type TxCallStack' c = SList (TxStackElement c)

type TxStackElement c = (TxNodeMeta c :!: SMaybe (TxDependencies c) :!: IORef TxStatus)

-- | A table mapping variable identifiers to buffered content, together with a list of memo tables (one per memoized function) that contain transient entries for this log
-- has a unique identifier so that it can be a key in a table of buffered memotables
-- this list is local to the tx (so no concurrent handling is needed) and used to know which tables to merge with the persistent memo tables at commit time
-- a txlog is partitioned into a list of memotables, to handle nested futures
newtype TxLog c = TxLog (Unique :!: IORef [TxLogBlock c] :!: IORef [DynTxMemoTable c]) deriving Typeable
type TxLogs c = SList (TxLog c)
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

type TxLayer l c = (Monoid (RepairDynTxVar c),TxLayerImpl l,Show (InvalidTx c),Show (TxId c),MonadBlock c (ReaderT (TxEnv c) IO),MonadBlock c IO,Typeable c,TxConflictClass c,Typeable l,Layer l (TxAdapton c),MonadReader (TxEnv c) (l (TxAdapton c)))

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

isNested :: TxStatus -> Maybe Unique
isNested (TxStatus (Read n :!: _)) = n
isNested (TxStatus (Eval n :!: _)) = n
isNested (TxStatus (Write n :!: _)) = n
isNested _ = Nothing
{-# INLINE isNested #-}

dirtyStatus status1 status2 = status1 `mappend` unNew status2
{-# INLINE dirtyStatus #-}

changeStatus st = let !v = if isWriteOrNewTrue st then Write else Eval in TxStatus $! (v Nothing :!: False)
{-# INLINE changeStatus #-}

unNew :: TxStatus -> TxStatus
unNew (TxStatus (New False :!: b)) = TxStatus (Read Nothing :!: b)
unNew (TxStatus (New True :!: b)) = TxStatus (Write Nothing :!: b)
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
finalizeTxLog :: TxLog c -> IO ()
finalizeTxLog txlog = {-# SCC finalizeTxLog #-} do
	readIORef (txLogBuff txlog) >>= Control.Monad.mapM_ MemoTable.finalize
	writeIORef' (txLogBuff txlog) []

txLogId (TxLog (x :!: y :!: z)) = x
txLogBuff (TxLog (x :!: y :!: z)) = y
txLogMemo (TxLog (x :!: y :!: z)) = z
{-# INLINE txLogId #-}
{-# INLINE txLogBuff #-}
{-# INLINE txLogMemo #-}

addBlockTxLogs :: Int -> TxLogs c -> IO ()
addBlockTxLogs memosize (SCons txlog _) = do
	let mkWeak = MkWeak $ mkWeakKey (txLogMemo txlog)
	x <- MemoTable.newSizedWithMkWeak memosize mkWeak
	modifyIORef' (txLogBuff txlog) (x:)

snapshotTxLogs :: Int -> ThreadId -> TxLogs c -> IO (ThreadId -> IO (TxLogs c))
snapshotTxLogs memosize parent_thread (SCons parent_txlog parent_txlogs) = do
	(child_txlog,add) <- snapshotTxLog parent_txlog
	return $ \child_thread -> add child_thread >> return (SCons child_txlog parent_txlogs)

  where
	snapshotTxLog :: TxLog c -> IO (TxLog c,ThreadId -> IO ())
	snapshotTxLog parent_txlog@(TxLog (uid :!: buff :!: memos)) = do
		-- snapshot parent
		buff' <- copyIORef' buff
		-- add block to parent
		modifyIORefM_' buff $ \xs -> do
			let mkWeak = MkWeak $ mkWeakKey memos
			x <- MemoTable.newSizedWithMkWeak memosize mkWeak
			return (x:xs)
		-- add block to child
		modifyIORefM_' buff' $ \xs -> do
			let mkWeak = MkWeak $ mkWeakKey memos
			x <- MemoTable.newSizedWithMkWeak memosize mkWeak
			return (x:xs)
		
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
						return $ Map.insert uid [x] xs
					-- add block to child
					modifyIORefM_' buffs' $ \xs -> do
						let !memoMkWeak = MkWeak $ mkWeakKey ori_tbl
						x <- MemoTable.newSizedWithMkWeak memosize memoMkWeak
						return $ Map.insert uid [x] xs
					-- delay commit to concurrent map until we have the child's threadid
					return $ \child_thread -> io child_thread >> CMap.insert child_thread buffs' buff_tbls
		
		io <- readIORef memos >>= Foldable.foldlM snapshotMemo mempty
		return (TxLog (uid :!: buff' :!: memos'),io)

{-# INLINE emptyTxLog #-}
emptyTxLog :: IORef (TxId c) -> Int -> IO (TxLog c)
emptyTxLog start memosize = do
	uid <- newUnique
	memos <- newIORef []
	let mkWeak = MkWeak $ mkWeakKey start
	buff <- MemoTable.newSizedWithMkWeak memosize mkWeak >>= newIORef' . (:[])
	return $ TxLog (uid :!: buff :!: memos)

-- commits dependencies
{-# INLINE commitDependenciesTx #-}
commitDependenciesTx :: TxDependencies c -> (Either (TxDependencies c) (Weak (TxDependencies c))) -> IO ()
commitDependenciesTx dependencies (Left oridependencies) = do
	-- the old dependencies reference lives as long as the new dependencies reference; because some dependencies may still point to the old dependencies reference
	mkWeakRefKey dependencies oridependencies Nothing >> return ()
commitDependenciesTx dependencies (Right w) = do -- we finalize all the old dependencies, to prevent old original dependencies to remain buffered
	mb <- Weak.deRefWeak w
	case mb of
		Nothing -> return ()
		Just oridependencies -> clearTxDependencies True oridependencies			

{-# INLINE markOriginalDependenciesTx #-}
markOriginalDependenciesTx :: TxDependencies c -> IO ()
markOriginalDependenciesTx dependencies = readIORef dependencies >>= Foldable.mapM_ markDependencyTx where
	markDependencyTx (d,w) = writeIORef' (originalTxW d) True

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

dynTxMValue :: DynTxVar c -> IO (a,TxStatus)
dynTxMValue (DynTxM (BuffTxM buff_value) txdeps m txstat) = do
	stat <- readIORef' txstat
	case stat of
		(isEvalOrWrite -> Just _) -> do
			!v <- readIORef buff_value
			let !c = coerce v
			return $! (c,stat)
		otherwise -> do
			!v <- readIORef (dataTxM m)
			let !c = coerce v
			return $! (c,stat)
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
dynTxVarDependencies (DynTxU (BuffTxU buff_dta) _ u txstat) = do
	!stat <- readIORef txstat
	case stat of
		(isEvalOrWrite -> Just _) -> do
			!(dta,_) <- readIORef buff_dta
			case dta of
				TxValue dirty value force dependencies -> return $ Just dependencies
				otherwise -> return Nothing
		otherwise -> do
			(readIORef $ dataTxU u) >>= \dta -> case dta of
				TxValue dirty value force dependencies -> return $ Just dependencies
				otherwise -> return Nothing
dynTxVarDependencies _ = return Nothing

-- stores a modifiable in a transactional buffer with a minimal status and returns a buffered copy
-- Read = an entry is enough
-- Eval = a transaction-consistent entry is enough
-- Write = creates a new or inconsistent entry
bufferTxM :: (IsolationKind i,IncK (TxAdapton c) a,TxLayer l c,TxLayer Outside c) => Bool -> ThreadId -> ThreadId -> TxM i c l (TxAdapton c) a -> TxStatus -> TxLogs c -> IO (DynTxVar c)
bufferTxM isFuture rootThread thread m st@(TxStatus (Read _ :!: i)) (txlogs :: TxLogs c) = doBlock (Proxy :: Proxy c) $ do
	let !idm = idTxNM $ metaTxM m
	mb <- findTxContentEntry isFuture thread txlogs (metaTxM m)
	case mb of
		Just (!tvar@(DynTxM (BuffTxM buff_dta) txdeps u txstat),!isTop) -> do
			!status <- readIORef txstat
			case status of
				TxStatus (New _ :!: j) -> if isTop
					then do
						let !newst = status `mappend` st
						writeIORef' txstat newst
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						return tvar
					else do
						let !newst = dirtyStatus st status
						buff_dta' <- blankBuffTxM
						txdeps' <- WeakMap.new'
						txstat' <- newIORef' newst
						let tvar' = DynTxM buff_dta' txdeps' m txstat'
						addTxLogEntryUp thread txlogs idm tvar'
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						return tvar'
				otherwise -> if isTop
					then do
						let !newst = status `mappend` st
						writeIORef' txstat newst
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						return tvar
					else do
						let !newst = status `mappend` st
						buff_dta' <- if isRead status then blankBuffTxM else liftM (BuffTxM) (liftM coerceIORef $ copyIORef' buff_dta)
						txdeps' <- WeakMap.copy' txdeps
						txstat' <- newIORef' newst
						let tvar' = DynTxM buff_dta' txdeps' m txstat'
						addTxLogEntryUp thread txlogs idm tvar'
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						return tvar'
		Nothing -> do
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
		Just (!tvar@(DynTxM (BuffTxM buff_dta) txdeps m txstat),!isTop) -> do
			!status <- readIORef txstat
			case status of
				TxStatus (New _ :!: j) -> if isTop
					then do
						let !newst = status `mappend` st
						writeIORef' txstat newst
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						return tvar
					else do
						let !newst = dirtyStatus st status
						buff_dta' <- copyIORef' $ dataTxM m
						txstat' <- newIORef' newst
						txdeps' <- WeakMap.new'
						let tvar' = DynTxM (BuffTxM buff_dta') txdeps' m txstat'
						addTxLogEntryUp thread txlogs idm tvar'
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						return tvar'
				TxStatus (Read _ :!: _) -> if isTop
					then do
						let !newst = st `mappend` status
						buff_value' <- readIORef' $ dataTxM m
						writeIORef buff_dta $! buff_value'
						writeIORef' txstat newst
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						return tvar
					else do
						let !newst = st `mappend` status
						buff_value' <- readIORef' $ dataTxM m
						buff_dta' <- newIORef' buff_value'
						txdeps' <- WeakMap.copy' txdeps
						txstat' <- newIORef' newst
						let tvar' = DynTxM (BuffTxM buff_dta') txdeps' m txstat'
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
 						let tvar' = DynTxM (BuffTxM buff_dta') txdeps' m txstat'
						addTxLogEntryUp thread txlogs idm tvar'
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						return tvar'
		Nothing -> do
			buff_value' <- readIORef' (dataTxM m)
			buff_dta' <- newIORef' buff_value'
			txdeps' <- WeakMap.new'
			txstat' <- newIORef' st
			let tvar' = DynTxM (BuffTxM buff_dta') txdeps' m txstat'
			addTxLogEntryUp thread txlogs idm tvar'
			addTxNotifies rootThread Nothing st txlogs idm (notifiesTxNM $! metaTxM m)
			return tvar'
bufferTxM isFuture rootThread thread m status txlogs = changeTxM isFuture rootThread thread m Nothing status txlogs

-- changes a modifiable, or just buffers it
changeTxM :: (IsolationKind i,IncK (TxAdapton c) a,TxLayer l c,TxLayer Outside c) => Bool -> ThreadId -> ThreadId -> TxM i c l (TxAdapton c) a -> Maybe a -> TxStatus ->  TxLogs c -> IO (DynTxVar c)
changeTxM isFuture rootThread thread m mbv' st@(isEvalOrWrite -> Just (n,i)) (txlogs :: TxLogs c) = doBlock (Proxy :: Proxy c) $ do
	let !idm = idTxNM $ metaTxM m
	mb <- findTxContentEntry isFuture thread txlogs (metaTxM m)
	case mb of
		Just (!tvar@(DynTxM (BuffTxM buff_dta) txdeps _ txstat),!isTop) -> do
			!status <- readIORef txstat
			case status of
				TxStatus (Read _ :!: i) -> if isTop
					then do
						let !newst = status `mappend` st
						!old_value <- readIORef $ dataTxM m
						let !v' = maybe old_value id mbv'
						writeIORef (coerceIORef buff_dta) $! v'
						writeIORef' txstat newst
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						return tvar
					else do
						let !newst = status `mappend` st
						!old_value <- readIORef $ dataTxM m
						let !v' = maybe old_value id mbv'
						buff_dta' <- newIORef $! v'
						txdeps' <- WeakMap.copy' txdeps
						txstat' <- newIORef' newst
						let tvar' = DynTxM (BuffTxM buff_dta') txdeps' m txstat'
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
						let !newst = dirtyStatus st status
						buff_dta' <- case mbv' of
							Nothing -> blankBuffTxM
							Just v' -> liftM BuffTxM $ newIORef $! v'
						txdeps' <- WeakMap.new'
						txstat' <- newIORef' newst
						let tvar' = DynTxM buff_dta' txdeps' m txstat'
						addTxLogEntryUp thread txlogs idm tvar'
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						return tvar'
				otherwise -> if isTop
					then do
						let newst = status `mappend` st
						!buff_value <- readIORef $ coerceIORef buff_dta
						let !v' = maybe buff_value id mbv'
						writeIORef (coerceIORef buff_dta) $! v'
						writeIORef' txstat newst
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						return tvar
					else do
						let newst = status `mappend` st
						!buff_value <- readIORef $ coerceIORef buff_dta
						let !v' = maybe buff_value id mbv'
						buff_dta' <- newIORef $! v'
						txdeps' <- WeakMap.copy' txdeps
						txstat' <- newIORef' newst
						let tvar' = DynTxM (BuffTxM buff_dta') txdeps' m txstat'
						addTxLogEntryUp thread txlogs idm tvar'
						addTxNotifies rootThread (Just status) newst txlogs idm (notifiesTxNM $! metaTxM m)
						return tvar'
		Nothing -> do
			!old_value <- readIORef $ dataTxM m
			let !v' = maybe old_value id mbv'
			buff_dta <- newIORef' v'
			txdeps <- WeakMap.new'
			txstat <- newIORef' st
			let tvar = DynTxM (BuffTxM buff_dta) txdeps m txstat
			addTxLogEntryUp thread txlogs idm tvar
			addTxNotifies rootThread Nothing st txlogs idm (notifiesTxNM $! metaTxM m)
			return tvar
changeTxM isFuture rootThread thread m _ status _ = error $ "changeTxM " ++ show status

bufferTxU :: (IncK (TxAdapton c) a,TxLayer l c,TxLayer Outside c) => Bool -> ThreadId -> ThreadId -> TxU c l (TxAdapton c) a -> TxStatus -> TxLogs c -> IO (DynTxVar c)
bufferTxU isFuture rootThread thread u st@(TxStatus (Read _ :!: i)) (txlogs :: TxLogs c) = doBlock (Proxy :: Proxy c) $ do 
	let !idu = idTxNM $ metaTxU u
	mb <- findTxContentEntry isFuture thread txlogs (metaTxU u)
	case mb of
		Just (!tvar@(DynTxU (BuffTxU buff_dta) txdeps _ txstat),!isTop) -> do
			!status <- readIORef txstat
			case status of
				TxStatus (New _ :!: j) -> if isTop
					then do
						let newst = status `mappend` st
						writeIORef' txstat newst
						addTxNotifies rootThread (Just status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar
					else do
						let !newst = dirtyStatus st status
						buff_dta' <- blankBuffTxU
						txdeps' <- WeakMap.new'
						txstat' <- newIORef' newst
						let tvar' = DynTxU buff_dta' txdeps' u txstat'
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
						!buff_dta' <- if isRead status then blankBuffTxU else liftM BuffTxU (liftM coerceIORefBuffTxUData $ copyIORef' buff_dta)
						txdeps' <- WeakMap.copy' txdeps
						txstat' <- newIORef' newst
						let tvar' = DynTxU buff_dta' txdeps' u txstat'
						addTxLogEntryUp thread txlogs idu tvar'
						addTxNotifies rootThread (Just status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar'
		Nothing -> do
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
		Just (!tvar@(DynTxU (BuffTxU buff_dta) txdeps _ txstat),!isTop) -> do
			!status <- readIORef txstat
			case status of
				TxStatus (New _ :!: j) -> if isTop
					then do
						let !newst = status `mappend` st
						writeIORef' txstat newst
						addTxNotifies rootThread (Just status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar
					else do -- we need to define a layer over the new local variable
						let !newst = dirtyStatus st status
						buff_dta' <- blankBuffTxU
						txdeps' <- WeakMap.new'
						txstat' <- newIORef' newst
						let tvar' = DynTxU buff_dta' txdeps' u txstat'
						addTxLogEntryUp thread txlogs idu tvar'
						addTxNotifies rootThread (Just status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar'
				TxStatus (Read _ :!: _) -> if isTop
					then do
						let !newst = status `mappend` st
						readIORef (coerceIORefTxUData $ dataTxU u) >>= copyTxUData >>= writeIORef' buff_dta
						writeIORef' txstat newst
						addTxNotifies rootThread (Just status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar
					else do
						let !newst = status `mappend` st
						buff_dta' <- readIORef (dataTxU u) >>= copyTxUData >>= newIORef'
						txdeps' <- WeakMap.copy' txdeps
						txstat' <- newIORef' newst
						let tvar' = DynTxU (BuffTxU buff_dta') txdeps' u txstat'
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
						buff_dta' <- copyIORef' buff_dta
						txdeps' <- WeakMap.copy' txdeps
						txstat' <- newIORef' newst
						let tvar' = DynTxU (BuffTxU $ coerceIORefBuffTxUData buff_dta') txdeps' u txstat'
						addTxLogEntryUp thread txlogs idu tvar'
						addTxNotifies rootThread (Just status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar'
		Nothing -> do
			!buff_dta <- (readIORef $ dataTxU u) >>= copyTxUData >>= newIORef'
			txdeps <- WeakMap.new'
			txstat <- newIORef' st
			let tvar = DynTxU (BuffTxU buff_dta) txdeps u txstat
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
		Just (!tvar@(DynTxU (BuffTxU buff_dta) txdeps u txstat),!isTop) -> do
			!buff_status <- readIORef txstat
			case buff_status of
				TxStatus (Read _ :!: _) -> if isTop
					then do
						let !newst = buff_status `mappend` status
						readIORef (dataTxU u) >>= copyTxUData >>= writeIORef' buff_dta
						modifyIORefM_' (coerceIORefBuffTxUData buff_dta) (maybe return id mbChgDta)
						writeIORef' txstat newst
						addTxNotifies rootThread (Just buff_status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar
					else do
						let !newst = buff_status `mappend` status
						txdeps' <- WeakMap.copy' txdeps
						!buff_dta' <- readIORef (dataTxU u) >>= copyTxUData >>= newIORef'
						modifyIORefM_' (coerceIORefBuffTxUData buff_dta') (maybe return id mbChgDta)
						txstat' <- newIORef' newst
						let !tvar' = DynTxU (BuffTxU buff_dta') txdeps' u txstat'
						addTxLogEntryUp thread txlogs idu tvar'
						addTxNotifies rootThread (Just buff_status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar'
						
				TxStatus (New _ :!: j) -> if isTop
					then do
						let !newst = mappend buff_status status
						case mbChgDta of
							Nothing -> return ()
							Just chgDta -> deadWeak >>= \w -> modifyIORefM_' (coerceIORefTxUData $ dataTxU u) (liftM (Prelude.fst) . chgDta . (,Right w)) 
						writeIORef' txstat newst
						addTxNotifies rootThread (Just buff_status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar
					else do -- we can't modify new variables created at another transactional layer
						let !newst = dirtyStatus status buff_status
						!buff_dta' <- readIORef (dataTxU u) >>= copyTxUData >>= newIORef'
						modifyIORefM_' (coerceIORefBuffTxUData buff_dta') (maybe return id mbChgDta) 
						txdeps' <- WeakMap.new'
						txstat' <- newIORef' newst
						let tvar' = DynTxU (BuffTxU buff_dta') txdeps' u txstat'
						addTxLogEntryUp thread txlogs idu tvar'
						addTxNotifies rootThread (Just buff_status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar'
						
				otherwise -> if isTop
					then do
						let !newst = mappend buff_status status
						let chg = maybe return id mbChgDta
						modifyIORefM_' (coerceIORefBuffTxUData buff_dta) chg 
						writeIORef' txstat newst
						addTxNotifies rootThread (Just buff_status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar
					else do
						let !newst = mappend buff_status status
						!buff_dta' <- copyIORef' buff_dta
						let chg = maybe return id mbChgDta
						modifyIORefM_' (coerceIORefBuffTxUData buff_dta') chg 
						txstat' <- newIORef' newst
						txdeps' <- WeakMap.copy' txdeps
						let !tvar' = DynTxU (BuffTxU $ buff_dta') txdeps' u txstat'
						-- all changes are logged on the nested transaction's log
						addTxLogEntryUp thread txlogs idu tvar'
						addTxNotifies rootThread (Just buff_status) newst txlogs idu (notifiesTxNM $! metaTxU u)
						return tvar'
						
		Nothing -> do
			txdeps <- WeakMap.new'
			!buff_dta <- (readIORef $ dataTxU u) >>= copyTxUData >>= newIORef'
			modifyIORefM_' buff_dta (maybe return id mbChgDta) 
			txstat <- newIORef' status
			let !tvar = DynTxU (BuffTxU buff_dta) txdeps u txstat
			addTxLogEntryUp thread txlogs idu tvar
			addTxNotifies rootThread Nothing status txlogs idu (notifiesTxNM $! metaTxU u)
			return tvar
changeTxU isFuture rootThread thread u _ status _ = error $ "changeTxU " ++ show status

-- remembers the original dependencies reference
{-# INLINE copyTxUData #-}
copyTxUData :: TxUData c l (TxAdapton c) a -> IO (BuffTxUData c l (TxAdapton c) a)
copyTxUData dta@(TxValue dirty value force dependencies) = do
	buff_dependencies <- copyIORef' dependencies
	return $! (TxValue dirty value force buff_dependencies,Left dependencies)
copyTxUData dta = deadWeak >>= \w -> return $! (dta,Right w)

#ifndef DEBUG
coerceIORefBuffTxUData = coerce
coerceIORefTxUData = coerce
coerceTxU = coerce
coerceTxM = coerce
coerceIORef = coerce
coerce = Unsafe.unsafeCoerce
#endif
#ifdef DEBUG
{-# INLINE coerceIORefBuffTxUData #-}
coerceIORefBuffTxUData :: (Typeable c,Typeable l,Typeable a,Typeable l1,Typeable a1) => IORef (BuffTxUData c l (TxAdapton c) a) -> IORef (BuffTxUData c l1 (TxAdapton c) a1)
coerceIORefBuffTxUData = coerce
coerceIORefTxUData :: (Typeable c,Typeable l,Typeable a,Typeable l1,Typeable a1) => IORef (TxUData c l (TxAdapton c) a) -> IORef (TxUData c l1 (TxAdapton c) a1)
coerceIORefTxUData = coerce
coerceTxU :: (Typeable c,Typeable l,Typeable a,Typeable l1,Typeable a1) => TxU c l (TxAdapton c) a -> TxU c l1 (TxAdapton c) a1
coerceTxU = coerce
coerceTxM :: (Typeable c,Typeable l,Typeable a,Typeable l1,Typeable a1) => TxM i c l (TxAdapton c) a -> TxM i c l1 (TxAdapton c) a1
coerceTxM = coerce
coerceIORef :: (Typeable a,Typeable a1) => IORef a -> IORef a1
coerceIORef = coerce
coerce :: (Typeable a,Typeable b) => a -> b
coerce = coerce' Proxy where
	coerce' :: (Typeable a,Typeable b) => Proxy b -> a -> b
	coerce' b (x::a) = case cast x of
		Nothing -> error $ "failed coerce: cast " ++ show (typeRep (Proxy :: Proxy a)) ++ " into " ++ show (typeRep b)
		Just y -> y
#endif
{-# INLINE coerceIORefTxUData #-}
{-# INLINE coerceTxU #-}
{-# INLINE coerceTxM #-}
{-# INLINE coerceIORef #-}
{-# INLINE coerce #-}

{-# INLINE findTxContentEntry #-}
findTxContentEntry :: TxLayer Outside c => Bool -> ThreadId -> TxLogs c -> TxNodeMeta c -> IO (Maybe (DynTxVar c,Bool))
findTxContentEntry !isFuture !thread !txlogs@(SCons txlog _) !meta = {-# SCC findTxContentEntry #-} do
	mb_stack <- CMap.lookup thread (contentTxNM meta)
	!r <- case mb_stack of
		Nothing -> if isFuture --variable-local cache for nested threads is built on demand, to avoid explicit snapshotting when the future is created
			then do
				let !uid = idTxNM meta
				mb <- findTxLogEntry txlogs uid -- lookup in the (slower) txlogs
				case mb of
					Nothing -> return Nothing
					Just r@(tvar,isTop) -> do -- add result to the variable-local cache for the future's thread
						addTxLogEntryUp thread txlogs uid tvar 
						return mb
			else return Nothing
		Just stack -> modifyIORefM' stack $ \(topid :!: xs) -> findTxContent' topid xs
	return r
  where
	{-# INLINE findTxContent' #-}
	findTxContent' topid SNil = return $! (topid :!: SNil,Nothing)
	findTxContent' topid xs@(SCons w t) = do
		mb <- Weak.deRefWeak w
		case mb of
			Nothing -> findTxContent' topid t
			Just entry -> do
				let !isTop = txLogId txlog == topid
				return $! (topid :!: xs,Just $! (entry,isTop))

-- finds a buffered entry in any depth of a nested tx log
{-# INLINE findTxLogEntry #-}
findTxLogEntry :: TxLogs c -> Unique -> IO (Maybe (DynTxVar c,Bool))
findTxLogEntry !txlogs !uid = {-# SCC findTxLogEntry #-} findTxLogEntry' txlogs uid True where
	findTxLogEntry' SNil uid isTop = return Nothing
	findTxLogEntry' (SCons txlog txlogs) uid isTop = do
		blocks <- readIORef' $ txLogBuff txlog
		mb_tvar <- findTxBlockEntry' blocks uid isTop
		case mb_tvar of
			Just tvar -> return mb_tvar
			Nothing -> findTxLogEntry' txlogs uid False
	-- only the first block is top; we don't want to modify older blocks
	findTxBlockEntry' (block:blocks) uid isTop = do
		mb <- MemoTable.lookup block uid
		case mb of
			Just tvar -> return $! Just (tvar,isTop)
			Nothing -> findTxBlockEntry' blocks uid False

{-# INLINE addTxLogEntryUp #-}
addTxLogEntryUp :: TxLayer Outside c => ThreadId -> TxLogs c -> Unique -> DynTxVar c -> IO ()
addTxLogEntryUp !thread !txlogs@(SCons txlog _) !uid !(tvar :: DynTxVar c) = {-# SCC addTxLogEntryUp #-} doBlock (Proxy :: Proxy c) $ do
	let !logid = txLogId txlog
	let (MkWeak mkWeak) = dynTxMkWeak tvar
	wtvar <- mkWeak tvar Nothing
	block <- liftM List.head $ readIORef' $ txLogBuff txlog
	MemoTable.insertWeak block uid wtvar
	stack <- CMap.lookupOrInsert thread (return . Just) (newIORef (logid :!: SNil) >>= \x -> return $! (x,x)) (\_ -> return ()) (contentTxNM $ dynTxMeta tvar)
	atomicModifyIORef' stack $ \(_ :!: xs) -> ((logid :!: SCons wtvar xs),())

{-# INLINE addTxLogEntryDown #-}
addTxLogEntryDown :: TxLayer Outside c => ThreadId -> TxLogs c -> Unique -> Weak (DynTxVar c) -> DynTxVar c -> IO ()
addTxLogEntryDown !thread !txlogs@(SCons txlog txtail) !uid !(wtvar :: Weak (DynTxVar c)) !tvar = {-# SCC addTxLogEntryDown #-} doBlock (Proxy :: Proxy c) $ do
	let !logid = txLogId txlog
	stack <- CMap.lookupOrInsert thread (return . Just) (newIORef (logid :!: SNil) >>= \x -> return $! (x,x)) (\_ -> return ()) (contentTxNM $ dynTxMeta tvar)
	commitNestedDynTxVar tvar logid
	block <- liftM List.head $ readIORef' $ txLogBuff txlog
	MemoTable.insertWeak block uid wtvar
	modifyIORef' stack $ \(_ :!: xs) -> (logid :!: xs)

-- updates a transaction-local new entry with a nested entry
commitNestedDynTxVar :: (TxLayer Outside c) => DynTxVar c -> Unique -> IO ()
commitNestedDynTxVar (DynTxU (BuffTxU buff_dta) txdeps u txstat :: DynTxVar c) parent_id = do
	let !proxy = Proxy :: Proxy c
	stat <- readIORef' txstat	
	case stat of
		TxStatus (Read (Just ((==parent_id) -> True)) :!: b) -> do
			WeakMap.unionWithKey' (dependentsTxNM $ metaTxU u) txdeps
			WeakMap.clean' txdeps
			writeIORef' txstat $ mkNew stat
		TxStatus (Eval (Just ((==parent_id) -> True)) :!: b) -> do
			WeakMap.unionWithKey' (dependentsTxNM $ metaTxU u) txdeps
			WeakMap.clean' txdeps
			(dta,ori_dependencies) <- readIORef' buff_dta
			case dta of
				TxValue dirty value force txrdependencies -> commitDependenciesTx txrdependencies ori_dependencies
				otherwise -> return ()
			writeIORef' (dataTxU u) dta
			writeIORef buff_dta $ error "BuffTxU"
			writeIORef' txstat $ mkNew stat
		TxStatus (Write (Just ((==parent_id) -> True)) :!: b) -> do
			WeakMap.unionWithKey' (dependentsTxNM $ metaTxU u) txdeps
			WeakMap.clean' txdeps
			(dta,ori_dependencies) <- readIORef' buff_dta
			case dta of
				TxValue dirty value force txrdependencies -> commitDependenciesTx txrdependencies ori_dependencies
				otherwise -> return ()
			writeIORef' (dataTxU u) dta
			writeIORef buff_dta $ error "BuffTxU"
			writeIORef' txstat $ mkNew stat
		otherwise -> return ()
commitNestedDynTxVar (DynTxM (BuffTxM buff_dta) txdeps m txstat :: DynTxVar c) parent_id = do
	let !proxy = Proxy :: Proxy c
	stat <- readIORef' txstat	
	case stat of
		TxStatus (Read (Just ((==parent_id) -> True)) :!: b) -> do
			WeakMap.unionWithKey' (dependentsTxNM $ metaTxM m) txdeps
			WeakMap.clean' txdeps
			writeIORef' txstat $ mkNew stat
		TxStatus (Eval (Just ((==parent_id) -> True)) :!: b) -> do
			WeakMap.unionWithKey' (dependentsTxNM $ metaTxM m) txdeps
			WeakMap.clean' txdeps
			buff_value <- readIORef' buff_dta
			writeIORef' (dataTxM m) buff_value
			writeIORef buff_dta $ error "BuffTxM"
			writeIORef' txstat $ mkNew stat
		TxStatus (Write (Just ((==parent_id) -> True)) :!: b) -> do
			WeakMap.unionWithKey' (dependentsTxNM $ metaTxM m) txdeps
			WeakMap.clean' txdeps
			buff_value <- readIORef' buff_dta
			writeIORef' (dataTxM m) buff_value
			writeIORef buff_dta $ error "BuffTxM"
			writeIORef' txstat $ mkNew stat
		otherwise -> return ()

-- conflates all the blocks of a txlog
flattenTxLogBlocks :: TxLayer Outside c => TxLog c -> IO (TxLogBlock c)
flattenTxLogBlocks txlog = do
	blocks <- readIORef $ txLogBuff txlog
	block <- flattenMemoTables blocks
	writeIORef' (txLogBuff txlog) [block]
	return block

flattenMemoTables :: (WeakHash.HashTable table,WeakKey (WeakHash.IOHashTable table k v),Eq k,Hashable k) => [WeakHash.IOHashTable table k v] -> IO (WeakHash.IOHashTable table k v)
flattenMemoTables [b] = return b
flattenMemoTables (b1:b2:bs) = do
	let add (uid,wentry) = do
		mb <- Weak.deRefWeak wentry
		case mb of
			Nothing -> return ()
			Just entry -> MemoTable.insertWeak b2 uid wentry
	MemoTable.mapWeakM_ add b1
	mkWeakKey b2 b2 $! Just $! MemoTable.finalize b1
	flattenMemoTables (b2:bs)

--findTxContentEntry :: TxLayer Outside c => Bool -> ThreadId -> TxLogs c -> TxNodeMeta c -> IO (Maybe (DynTxVar c,Bool))
--findTxLogEntry :: TxLogs c -> Unique -> IO (Maybe (DynTxVar c,Bool))

findTxMContentValue :: TxLayer Outside c => Bool -> ThreadId -> TxLogs c -> TxM i c l (TxAdapton c) a -> IO a
findTxMContentValue isFuture thread txlogs m = do
	mb <- findTxContentEntry isFuture thread txlogs (metaTxM m)
	case mb of
		Just (tvar,_) -> liftM Prelude.fst $ dynTxMValue tvar
		Nothing -> readIORef' (dataTxM m)

findTxMLogValue :: TxLogs c -> TxM i c l (TxAdapton c) a -> IO a
findTxMLogValue  txlogs m = do
	mb <- findTxLogEntry txlogs (idTxNM $ metaTxM m)
	case mb of
		Just (tvar,_) -> liftM Prelude.fst $ dynTxMValue tvar
		Nothing -> readIORef' (dataTxM m)

-- merges a nested txlog with its parent txlog, by overriding parent entries
-- unbuffer writes if @doWrites@ is set to @False@
{-# INLINE mergeTxLog #-}
mergeTxLog :: TxLayer Outside c => Bool -> Bool -> ThreadId -> TxLog c -> TxLogs c -> IO ()
mergeTxLog doWrites !isFuture !thread txlog1 txlogs2@(SCons txlog2 _) = flattenTxLogBlocks txlog1 >>= MemoTable.mapWeakM_ updEntry where
	updEntry (uid,wentry) = do
		mb <- Weak.deRefWeak wentry
		case mb of
			Nothing -> return ()
			Just entry -> do
				unless doWrites $ unbufferDynTxVar'' isFuture thread (SCons txlog1 txlogs2) True entry
				addTxLogEntryDown thread txlogs2 uid wentry entry

-- merge the second variable over the first one
mergeDynTxVar :: Bool -> DynTxVar c -> DynTxVar c -> IO ()
mergeDynTxVar overwriteData (DynTxM (BuffTxM buff_dta1) buff_deps1 m1 stat1) (DynTxM (BuffTxM buff_dta2) buff_deps2 m2 stat2) = do
	when overwriteData $ readIORef' buff_dta2 >>= writeIORef' buff_dta1 . coerce
	WeakMap.unionWithKey' buff_deps1 buff_deps2
	modifyIORefM_' stat1 $ \s1 -> readIORef' stat2 >>= \s2 -> return (mappend s1 s2)
mergeDynTxVar overwriteData (DynTxU (BuffTxU buff_dta1) buff_deps1 m1 stat1) (DynTxU (BuffTxU buff_dta2) buff_deps2 m2 stat2) = do	
	when overwriteData $ readIORef' buff_dta2 >>= writeIORef' buff_dta1 . coerce
	WeakMap.unionWithKey' buff_deps1 buff_deps2
	modifyIORefM_' stat1 $ \s1 -> readIORef' stat2 >>= \s2 -> return (mappend s1 s2)

diffTxLog :: TxLog c -> TxLog c -> IO (TxLog c)
diffTxLog (TxLog (uid :!: blocks1 :!: memos)) (TxLog (_ :!: blocks2 :!: _)) = do
	xs2 <- readIORef' blocks2
	xs1 <- readIORef' blocks1
	blocks' <- newIORef' $ List.take (List.length xs1 - List.length xs2) xs1
	return $ TxLog (uid :!: blocks' :!: memos)

-- ** dirtying

{-# INLINE dirtyBufferedDynTxVar #-}
dirtyBufferedDynTxVar :: TxLayer Outside c => Bool -> ThreadId -> TxLogs c -> DynTxVar c -> IO ()
dirtyBufferedDynTxVar !isFuture !thread txlogs tvar = do
	dirtyBufferedTxData tvar
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
				Nothing -> return ()
				Just (!creator_var,!isTop) -> do
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
						Nothing -> return () -- stop when the dependent variable is not buffered
						Just (!tgt_tvar,!isTop) -> do
							dirtyBufferedTxData tgt_tvar
							dirtyRecursivelyBufferedDynTxVar isFuture thread txlogs tgt_tvar

-- dirties a buffered variable in-place without changing its status
dirtyBufferedTxData :: DynTxVar c -> IO ()
dirtyBufferedTxData (DynTxU (BuffTxU buff_dta) _ u txstat) = do
	!stat <- readIORef txstat
	case stat of
		TxStatus (New _ :!: _) -> modifyIORefM_' (dataTxU u) chgDirty
		TxStatus (Read _ :!: _) -> return ()
		otherwise -> modifyIORef' buff_dta chgDirtyBuff
  where
	chgDirty (TxValue _ value force dependencies) = return $ TxValue 1# value force dependencies
	chgDirty dta = return dta
	chgDirtyBuff (TxValue _ value force dependencies,ori) = (TxValue 1# value force dependencies,ori)
	chgDirtyBuff dta = dta
dirtyBufferedTxData _ = return ()

forgetBufferedTxData :: ThreadId -> DynTxVar c -> IO ()
forgetBufferedTxData thread (DynTxU (BuffTxU buff_dta) _ u txstat) = do
	!stat <- readIORef txstat
	-- unmemoize this thunk from the buffered memotables for this thread
	let unmemo = do
		(_ :!: buff_unmemos) <- readMVar "forgetBufferedTxData" (unmemoTxNM $ metaTxU u)
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
	forgetBuff (TxValue _ value force dependencies,ori) = do
		ori' <- case ori of
			Left deps -> liftM Right $ mkWeakRefKey deps deps Nothing
			Right wdeps -> return $ Right wdeps
		clearTxDependencies False dependencies
		return (TxThunk force,ori)
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
unbufferTopTxLog !isFuture !thread txlogs@(SCons txlog _) !onlyWrites = do
	b <- isInside
	-- we don't need to unbuffer a computation at the inner layer, because it has no writes
	unless (onlyWrites && b) $ unsafeIOToInc $ flattenTxLogBlocks txlog >>= MemoTable.mapM_ (unbufferDynTxVar' isFuture thread onlyWrites txlogs)

{-# INLINE unbufferDynTxVar #-}
unbufferDynTxVar :: TxLayer Outside c => Bool -> ThreadId -> Bool -> TxLogs c -> DynTxVar c -> IO ()
unbufferDynTxVar !isFuture !thread !onlyWrites txlogs !entry = unbufferDynTxVar' isFuture thread onlyWrites txlogs (dynTxId entry,entry)
			
unbufferTxVar :: TxLayer Outside c => Bool -> ThreadId -> Bool -> TxLogs c -> TxNodeMeta c -> IO ()
unbufferTxVar isFuture thread onlyWrites txlogs meta = do
	mb <- findTxContentEntry isFuture thread txlogs meta
	case mb of
		Just (!tvar,!isTop) -> do
--			debugTx' $ "unbufferingTx " ++ show onlyWrites ++ "  " ++ show uid ++ " " ++ show (dynTxStatus tvar)
			unbufferDynTxVar' isFuture thread onlyWrites txlogs (idTxNM meta,tvar)
		Nothing -> do
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
unbufferDynTxVar'' isFuture thread txlogs onlyWrites tvar@(DynTxU (BuffTxU dta) txdeps u txstat) = do
	!stat <- readIORef txstat
	case stat of
		-- we don't unbuffer the value of New variables; New variables cannot have original dependents
		TxStatus (New i :!: b) -> when (if onlyWrites then i else True) $ do 
--			debugTx' $ "unbuffered " ++ show stat ++ " " ++ show (idTxNM $ metaTxU u)
			dirtyBufferedDynTxVar isFuture thread txlogs tvar
			writeIORef txstat $! TxStatus (New False :!: b)
		otherwise -> when ((if onlyWrites then isWriteOrNewTrue else Prelude.const True) stat) $ do
--			debugTx' $ "unbuffered " ++ show stat ++ " " ++ show (idTxNM $ metaTxU u)
			let !nested = isNested stat
			-- keep dirtied buffered data for nested local changes
			unless (isJust nested) $ forgetBufferedTxData thread tvar
			dirtyBufferedDynTxVar isFuture thread txlogs tvar
			-- unbuffer the thunk, so that the fresher original data is used instead
			hasDeps <- WeakMap.null' txdeps
			writeIORef txstat $! TxStatus (Read nested :!: hasDeps)
			writeIORef dta $ error "BuffTxU"
unbufferDynTxVar'' isFuture thread txlogs onlyWrites tvar@(DynTxM (BuffTxM dta) txdeps m txstat) = do
	!stat <- readIORef txstat
	case stat of
		TxStatus (New i :!: b) -> when (if onlyWrites then i else True) $ do
--			debugTx' $ "unbuffered " ++ show stat ++ " " ++ show (idTxNM $ metaTxM m)
			dirtyBufferedDynTxVar isFuture thread txlogs tvar
			writeIORef txstat $! TxStatus (New False :!: b)
		otherwise -> when ((if onlyWrites then isWriteOrNewTrue else Prelude.const True) stat) $ do
--			debugTx' $ "unbuffered " ++ show stat ++ " " ++ show (idTxNM $ metaTxM m)
			let !nested = isNested stat
			hasDeps <- WeakMap.null' txdeps
			dirtyBufferedDynTxVar isFuture thread txlogs tvar
			writeIORef txstat $! TxStatus (Read nested :!: hasDeps)
			writeIORef dta $ error "BuffTxM"
	
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
txStatusType (TxStatus (Read Nothing :!: False)) = 1 
txStatusType (TxStatus (New False :!: _)) = 2
txStatusType (TxStatus (Read Nothing :!: True)) = 2 
txStatusType (TxStatus (Eval Nothing :!: _)) = 2 
txStatusType (TxStatus (Write Nothing :!: _)) = 3
txStatusType (TxStatus (New True :!: _)) = 3 
txStatusType st = error $ "txStatusType: " ++ show st
{-# INLINE txStatusType #-}

withTxLocks :: (TxLayer Outside c,TxLayer l c) => (Bool -> l (TxAdapton c) a) -> l (TxAdapton c) a
withTxLocks f = do
	readOnly <- isInside
	-- read-only transactions have optional evals
	-- read-writes transactions need all evaluated dependents to be commited
	let !next = if readOnly then False else True
	let go isFst = withTxLocks' isFst f >>= maybe (unsafeIOToInc (yield >> threadDelay 1000) >> go False) return
	go True
  where
	{-# INLINE lockType #-}
	lockType (1,_) = False
	lockType (2,True) = True
	lockType (2,False) = False
	lockType (3,_) = True

	{-# INLINE withTxLocks' #-}
	withTxLocks' :: (TxLayer Outside c,TxLayer l c) => Bool -> (Bool -> l (TxAdapton c) a) -> l (TxAdapton c) (Maybe a)
	withTxLocks' doEvals m = doBlockTx $ do
		SCons txlog SNil <- readTxLogs
		let acquireDynTxVar !(!ok,!lcks) !(uid,tvar) = do
			!lockn <- liftM txStatusType $ readIORef $ dynTxStatus tvar
			let !lck = dynTxVarLock tvar
			case lockType (lockn,doEvals) of
				False -> do
					b <- Lock.tryWait "withTxLocksWait" lck
					if b
						then return $! Right $! (ok,lcks)
						else return $! Left $! (False,lcks)
				True -> do
					b <- Lock.tryAcquire "withTxLocksAcquire" lck
					if b
						then let !lcks' = lck : lcks in return $! Right $! (ok,lcks')
						else return $! Left $! (False,lcks)
		!(ok,acquired) <- unsafeIOToInc $ flattenTxLogBlocks txlog >>= MemoTable.foldStopM acquireDynTxVar (True,[])
		if ok
			then m doEvals >>= \x -> unsafeIOToInc (Lock.releases "withTxLocksRelease" acquired) >> (return $! Just x)
			else unsafeIOToInc (Lock.releases "withTxLocksRelease" acquired) >> return Nothing





