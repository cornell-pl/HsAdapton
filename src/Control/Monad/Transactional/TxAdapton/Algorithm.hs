{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, UndecidableInstances, MultiParamTypeClasses, FlexibleInstances, MagicHash, ViewPatterns, BangPatterns, ConstraintKinds, FlexibleContexts #-}

module Control.Monad.Transactional.TxAdapton.Algorithm where

import Control.Monad.Incremental
import Control.Monad.Transactional
import Control.Monad.Incremental.Adapton
import Data.Typeable
import qualified Control.Monad.Reader as Reader
import Data.Concurrent.Deque.Class as Queue
import Data.Concurrent.Deque.Reference.DequeInstance
import Data.Maybe
import Control.Monad.Transactional.TxAdapton.Memo

import Control.Monad.Transactional.TxAdapton.Types
import Control.Monad.Transactional.TxAdapton.Layers

import System.Mem.WeakSet as WeakSet
import Data.Unique
import Control.Monad.Ref
import Control.Monad.IO.Class
import System.Mem.WeakRef as WeakRef
import Data.Strict.Maybe as Strict
import Data.Strict.List as Strict
import Data.Strict.Tuple as Strict
import System.Mem.Weak as Weak
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Set (Set(..))
import qualified Data.Set as Set
import Control.Monad
import qualified Data.HashTable.IO as HashIO
import qualified Data.HashTable.ST.Basic as HashST
import Data.Foldable as Foldable
import Data.List as List
import Control.Concurrent.MVar
import Control.Concurrent.Lock as Lock
import Data.Time.Clock
import Control.Exception
import Control.Monad.Catch as Catch
import Safe

-- ** strict inputs @M@

instance (MonadIO m,TxLayer Inside r m) => Thunk TxM Inside TxAdapton r m where
	new = modInnerTxM
	{-# INLINE new #-}
	newc = refInnerTxM
	{-# INLINE newc #-}
	read = getInnerTxM
	{-# INLINE read #-}

instance (MonadIO m,TxLayer Outside r m) => Thunk TxM Outside TxAdapton r m where
	new = modOuterTxM
	{-# INLINE new #-}
	newc = refOuterTxM
	{-# INLINE newc #-}
	read = getOuterTxM
	{-# INLINE read #-}

instance (MonadIO m,TxLayer Outside r m,TxLayer Inside r m) => Input TxM Inside TxAdapton r m where
	ref = refInnerTxM
	{-# INLINE ref #-}
	get = getInnerTxM
	{-# INLINE get #-}
	set = setTxM
	{-# INLINE set #-}
	getOutside = getOuterTxM
	{-# INLINE getOutside #-}
	refOutside = refOuterTxM
	{-# INLINE refOutside #-}
	modOutside = \c -> outside c >>= refOutside
	{-# INLINE modOutside #-}

instance (MonadIO m,TxLayer Outside r m) => Input TxM Outside TxAdapton r m where
	ref = refOuterTxM
	{-# INLINE ref #-}
	get = getOuterTxM
	{-# INLINE get #-}
	set = setTxM
	{-# INLINE set #-}
	refOutside = refOuterTxM
	{-# INLINE refOutside #-}
	modOutside = \c -> c >>= refOutside
	{-# INLINE modOutside #-}

modInnerTxM :: (Eq a,MonadIO m,TxLayer Inside r m) => Inside TxAdapton r m a -> Inside TxAdapton r m (TxM Inside TxAdapton r m a)
modInnerTxM m = m >>= refInnerTxM

modOuterTxM :: (Eq a,TxLayer Outside r m,MonadIO m) => Outside TxAdapton r m a -> Outside TxAdapton r m (TxM Outside TxAdapton r m a)
modOuterTxM m = m >>= refOuterTxM

refOuterTxM :: (Eq a,TxLayer l r m,MonadIO m,TxLayer Outside r m) => a -> Outside TxAdapton r m (TxM l TxAdapton r m a)
refOuterTxM v = do
	idU <- inL $ liftIO newUnique
	dta <- inL $ newRef v
	dependentsU <- inL $ liftIO $ WeakSet.new
	-- since the ref will never be reused, we don't need to worry about it's creator
	waitQ <- inL $ liftIO newQ
	let m = TxM (dta,(TxNodeMeta (idU,dependentsU,error "nodirty",\tbl -> return (),bufferTxM m,Nothing,waitQ)))
	newTxMLog m
	return m

refInnerTxM :: (Eq a,TxLayer l r m,MonadIO m,TxLayer Inside r m) => a -> Inside TxAdapton r m (TxM l TxAdapton r m a)
refInnerTxM v = do
	idU <- inL $ liftIO newUnique
	dta <- inL $ newRef v
	dependentsU <- inL $ liftIO $ WeakSet.new
	-- add a reference dependency (they are transitive up to the top-level calling thunk)
	creator <- mkRefCreatorTx idU
	waitQ <- inL $ liftIO newQ
	let m = TxM (dta,(TxNodeMeta (idU,dependentsU,error "nodirty",\tbl -> return (),bufferTxM m,creator,waitQ)))
	newTxMLog m
	return m

{-# INLINE getInnerTxM #-}
-- we don't need to buffer a strict input on reads
getInnerTxM :: (MonadIO m,Eq a,TxLayer Inside r m) => TxM Inside TxAdapton r m a -> Inside TxAdapton r m a
getInnerTxM = \t -> do
	value <- readTxMValue t -- read from the buffer
	addDependencyTx (metaTxM t) (checkTxM t $! value) -- updates dependencies of callers
	return value

{-# INLINE getOuterTxM #-}	
getOuterTxM :: (MonadIO m,TxLayer l r m,TxLayer Outside r m) => TxM l TxAdapton r m a -> Outside TxAdapton r m a
getOuterTxM = \t -> inL $ readRef (dataTxM t)

{-# INLINE checkTxM #-}
checkTxM :: (Eq a,TxLayer Inside r m,MonadIO m) => TxM Inside TxAdapton r m a -> a -> Inside TxAdapton r m Bool
checkTxM t oldv = do
	value <- readTxMValue t
	return $ oldv == value

setTxM :: (Eq a,TxLayer Outside r m,MonadIO m,TxLayer l r m) => TxM l TxAdapton r m a -> a -> Outside TxAdapton r m ()
setTxM t v' = do
	v <- readTxMValue t
	unless (v == v') $ do
		writeTxMValue t v'
		dirtyTx (metaTxM t)

-- ** lazy inputs @L@

-- ** lazy outputs @U@

instance (MonadIO m,TxLayer Inside r m) => Thunk TxU Inside TxAdapton r m where
	new = thunkTxU
	{-# INLINE new #-}
	newc = constTxU
	{-# INLINE newc #-}
	read = forceInnerTxU
	{-# INLINE read #-}

instance (MonadIO m,TxLayer Outside r m) => Thunk TxU Outside TxAdapton r m where
	new = thunkTxU
	{-# INLINE new #-}
	newc = constTxU
	{-# INLINE newc #-}
	read = forceOuterTxU
	{-# INLINE read #-}

-- no memoization at the outer layer
instance (TxLayer Outside r m,MonadRef r m,WeakRef r,MonadIO m) => Output TxU Outside TxAdapton r m where
	thunk = thunkTxU
	{-# INLINE thunk #-}
	const = constTxU
	{-# INLINE const #-}
	force = forceOuterTxU
	{-# INLINE force #-}
	forceOutside = forceOuterTxU
	{-# INLINE forceOutside #-}

instance (TxLayer Inside r m,MonadRef r m,WeakRef r,MonadIO m) => Output TxU Inside TxAdapton r m where
	thunk = thunkTxU
	{-# INLINE thunk #-}
	const = constTxU
	{-# INLINE const #-}
	force = forceInnerTxU
	{-# INLINE force #-}
	forceOutside = world . forceNoDependentsTxU
	{-# INLINE forceOutside #-}
--	memo = memoTxU
--	{-# INLINE memo #-}
--	gmemoQ = gmemoQTxU
--	{-# INLINE gmemoQ #-}

thunkTxU :: (Eq a,MonadIO m,TxLayer l r m,TxLayer l1 r m) => l1 TxAdapton r m a -> l TxAdapton r m (TxU l1 TxAdapton r m a)
thunkTxU c = do
	idU <- inL $ liftIO newUnique
	dta <- inL $ newRef (TxThunk c)
	dependentsU <- inL $ liftIO $ WeakSet.new
	waitQ <- inL $ liftIO $ newQ
	let u = TxU (dta,(TxNodeMeta (idU,dependentsU,changeDirtyValueTx True Write u,forgetUDataTx u,bufferTxU u,Nothing,waitQ)))
	newTxULog u
	return u

constTxU :: (Eq a,MonadIO m,TxLayer l r m,TxLayer l1 r m) => a -> l TxAdapton r m (TxU l1 TxAdapton r m a)
constTxU v = do
	idU <- inL $ liftIO newUnique
	dta <- inL $ newRef (TxConst v)
	waitQ <- inL $ liftIO $ newQ
	let u = TxU (dta,(TxNodeMeta (idU,error "no dependents",error "no dirty",forgetUDataTx u,bufferTxU u,Nothing,waitQ)))
	newTxULog u
	return u

forceOuterTxU :: (MonadIO m,Eq a,TxLayer Outside r m) => TxU Outside TxAdapton r m a -> Outside TxAdapton r m a
forceOuterTxU = error "forceOuter"

forceInnerTxU :: (MonadIO m,Eq a,TxLayer Inside r m) => TxU Inside TxAdapton r m a -> Inside TxAdapton r m a
forceInnerTxU = \t -> do
	value <- forceNoDependentsTxU t
	has <- hasDependenciesTxU t 
	if has
		then addDependencyTx (metaTxU t) (checkTxU t $! value)
		else writeTxUValue t (TxConst value) Eval
	return value

hasDependenciesTxU :: (Eq a,TxLayer Inside r m) => TxU Inside TxAdapton r m a -> Inside TxAdapton r m Bool
hasDependenciesTxU t = do
	d <- readTxUValue t
	case d of
		TxValue _ value force dependencies -> liftM (not . null) $ inL $ readRef dependencies
		TxThunk force -> error "cannot test dependencies of unevaluated thunk"
		TxConst value -> return False

{-# INLINE forceNoDependentsTxU #-}
forceNoDependentsTxU :: (MonadIO m,Eq a,TxLayer Inside r m) => TxU Inside TxAdapton r m a -> Inside TxAdapton r m a
forceNoDependentsTxU = \t -> do
	d <- readTxUValue t
	case d of
		TxValue 0# value force dependencies -> return value 
		TxValue 1# value force dependencies -> repairInnerTxU t value force 
		TxThunk force -> inL (newRef []) >>= evaluateInnerTxU t force 
		TxConst value -> return value

checkTxU :: (MonadIO m,Eq a,TxLayer Inside r m) => TxU Inside TxAdapton r m a -> a -> Inside TxAdapton r m Bool
checkTxU t oldv = do
	d <- readTxUValue t
	case d of
		TxValue 0# value force txdependencies -> return (oldv==value)
		TxValue 1# value force txdependencies -> liftM (oldv ==) (repairInnerTxU t value force)
		TxThunk _ -> return False 
		TxConst value -> return False

repairInnerTxU :: (Eq a,MonadIO m,TxLayer Inside r m) => TxU Inside TxAdapton r m a -> a -> Inside TxAdapton r m a -> Inside TxAdapton r m a
repairInnerTxU t value force = do
		tbl <- readTxLog
		txdependencies <- inL $ getTxDependencies tbl (metaTxU t) Eval -- we are just evaluating
		inL (readRef txdependencies) >>= Prelude.foldr (repair' t force tbl txdependencies) (norepair' t value tbl) . reverse --we need to reverse the dependency list to respect evaluation order
	where
	{-# INLINE norepair' #-}
	norepair' :: (Eq a,TxLayer Inside r m) => TxU Inside TxAdapton r m a -> a -> [TxLog r m] -> Inside TxAdapton r m a
	norepair' t value tbl = inL (changeDirtyValueTx False Eval t tbl) >> return value
	{-# INLINE repair' #-}
	repair' :: (Eq a,TxLayer Inside r m) => TxU Inside TxAdapton r m a -> Inside TxAdapton r m a -> [TxLog r m] -> TxDependencies r m -> (TxDependency r m,IO ()) -> Inside TxAdapton r m a -> Inside TxAdapton r m a
	repair' t force tbl txdependencies (d,fin) m = do
		isDirty <- inL $ readRef (dirtyTxW d)
		if isDirty
			then do
				txdependents <- inL $ getTxDependents tbl (srcMetaTxW d)
				inL $ changeTxDependency txdependencies txdependents d False
				ok <- checkTxW d
				if ok then m else inL (clearDependenciesTx txdependencies >> newRef []) >>= evaluateInnerTxU t force
			else m

{-# INLINE evaluateInnerTxU #-}
evaluateInnerTxU :: (Eq a,MonadIO m,TxLayer Inside r m) => TxU Inside TxAdapton r m a -> Inside TxAdapton r m a -> TxDependencies r m -> Inside TxAdapton r m a
evaluateInnerTxU t force txdependencies = do
	pushTxStack (metaTxU t :!: SJust txdependencies)
	value <- force
	writeTxUValue t (TxValue 0# value force txdependencies) Eval
	popTxStack
	return value

-- ** auxiliary functions
	
{-# INLINE clearDependenciesTx #-}
-- clear only buffered dependencies
clearDependenciesTx :: (MonadRef r m,MonadIO m) => TxDependencies r m -> m ()
clearDependenciesTx = \r -> readRef r >>= Foldable.mapM_ clearDependency where
	clearDependency (d,f) = do
		isOriginal <- readRef (flagTxW d)
		unless isOriginal $ liftIO f
	
-- makes the new node an eval or a write
changeDirtyValueTx :: (Eq a,TxLayer l r m,MonadRef r m,MonadIO m) => Bool -> TxStatus -> TxU l TxAdapton r m a -> [TxLog r m] -> m ()
changeDirtyValueTx dirty newstatus u txlog = changeTxU u (Just chgDirty) newstatus txlog >> return () where
	chgDirty (TxValue _ value force dependencies) = return $ TxValue (if dirty then 1# else 0#) value force dependencies

forgetUDataTx :: (Eq a,TxLayer l r m,MonadRef r m,MonadIO m) => TxU l TxAdapton r m a -> [TxLog r m] -> m ()
forgetUDataTx u txlog = changeTxU u (Just forget) Write txlog >> return () where
	forget (TxValue _ _ force dependencies) = clearDependenciesTx dependencies >> return (TxThunk force)
	forget dta = return dta

{-# INLINE mkRefCreatorTx #-}
mkRefCreatorTx :: (WeakRef r,MonadIO m,TxLayer l r m) => Unique -> l TxAdapton r m (Maybe (TxCreator r m))
mkRefCreatorTx = \idU -> do
	top <- topTxStack
	case top of
		Just (callermeta :!: SJust txcallerdependencies) -> do
			-- its ok to point to the buffered transaction dependencies reference, because the creator never changes
			weak <- inL $ liftIO $ mkWeakWithRefKey txcallerdependencies callermeta Nothing
			return $ Just weak
		otherwise -> return Nothing

{-# INLINE addDependencyTx #-}
-- multiple dependencies on the same source node are combined into one
addDependencyTx :: (MonadIO m,MonadRef r m,WeakRef r,TxLayer Inside r m) => TxNodeMeta r m -> Inside TxAdapton r m Bool -> Inside TxAdapton r m ()
addDependencyTx calleemeta check = do
	top <- topThunkTxStack
	case top of
		Just (callermeta :!: SJust txcallerdependencies) -> do
			tbl <- readTxLog
			dirtyW <- newRef False 
			originalW <- newRef False
			let dependencyW = TxDependency (calleemeta,dirtyW,check,callermeta,originalW)
			txcalleedependents <- inL $ getTxDependents tbl calleemeta
			-- purging does not affect consistency of reads, so we don't need to care about race conditions on the original data
			let !purge = WeakSet.purge (dependentsTxNM calleemeta) >> WeakSet.purge txcalleedependents
			weak <- inL $ liftIO $ mkWeakWithRefKey txcallerdependencies dependencyW (Just purge)
			inL $ insertTxDependency (dependencyW,finalize weak) txcallerdependencies
			inL $ liftIO $ WeakSet.insertWeak txcalleedependents weak
		otherwise -> return ()

changeTxDependency :: (MonadRef r m,MonadIO m,WeakRef r) => TxDependencies r m -> TxDependents r m -> TxDependency r m -> Bool -> m ()
changeTxDependency txdependencies txdependents d@(TxDependency (srcMetaW,dirtyW,checkW,tgtMetaW,originalW)) isDirty = do
	isOriginal <- readRef originalW
	if isOriginal
		then do -- when the dependency is not buffered, make a buffered copy
			dirtyW' <- newRef isDirty
			originalW' <- newRef False
			let dependencyW' = TxDependency (srcMetaW,dirtyW',checkW,tgtMetaW,originalW')
			let !purge = WeakSet.purge (dependentsTxNM srcMetaW) >> WeakSet.purge txdependents
			weak' <- liftIO $ mkWeakWithRefKey txdependencies dependencyW' (Just purge)
			insertTxDependency (dependencyW',Weak.finalize weak') txdependencies -- overrides the original dependency
			liftIO $ WeakSet.insertWeak txdependents weak'
		else writeRef dirtyW isDirty

{-# INLINE dirtyTx #-}
dirtyTx :: (TxLayer l r m) => TxNodeMeta r m -> l TxAdapton r m ()
dirtyTx = \umeta -> do
	tbl <- readTxLog
	inL $ dirtyCreatorTx tbl (creatorTxNM umeta)
	inL $ dirtyRecursivelyTx tbl umeta

dirtyCreatorTx :: (MonadIO m,MonadRef r m,WeakRef r) => [TxLog r m] -> Maybe (TxCreator r m) -> m ()
dirtyCreatorTx tbl Nothing = return ()
dirtyCreatorTx tbl (Just wcreator) = do
	mb <- liftIO $ deRefWeak wcreator
	case mb of
		Just creatorMeta -> do
			forgetTxNM creatorMeta tbl
			dirtyRecursivelyTx tbl creatorMeta
		Nothing -> return ()

dirtyRecursivelyTx :: (WeakRef r,MonadIO m,MonadRef r m) => [TxLog r m] -> TxNodeMeta r m -> m ()
dirtyRecursivelyTx tbl meta = do
	txdependents <- getTxDependents tbl meta -- marks the buffered dependents as an eval
	dependents <- WeakSet.toListPurge txdependents
	Control.Monad.mapM_ (dirtyTx' txdependents) dependents
  where
	{-# INLINE dirtyTx' #-}
	dirtyTx' txdependents = \d -> do
		isDirty <- readRef (dirtyTxW d)
		unless isDirty $ do
			txdependencies <- getTxDependencies tbl (tgtMetaTxW d) Write -- marks the buffered dependencies as a write
			changeTxDependency txdependencies txdependents d True
			dirtyTxNM (tgtMetaTxW d) tbl
			dirtyRecursivelyTx tbl meta

-- inserts or updates a new dependency in a dependency list
insertTxDependency :: MonadRef r m => (TxDependency r m,IO ()) -> TxDependencies r m -> m ()
insertTxDependency d rds = mapRef updateTxDependency' rds where
	updateTxDependency' [] = [d]
	updateTxDependency' (x:xs) = if did == xid then d:xs else updateTxDependency' xs
		where did = idTxNM $ srcMetaTxW $ Prelude.fst d
		      xid = idTxNM $ srcMetaTxW $ Prelude.fst x

-- ** Transactional support

-- | commits local buffered changes to the original thunk, and returns a log of writes (ignores reads, evals and news)
-- when @onlyAllocs@ is turned on we simply mark new dependencies as original
-- the second returned action wakes up sleeping txs that are listening to changed modifiables
commitDynTxVar :: (TxLayer Outside r m,MonadIO m,MonadRef r m) => Bool -> DynTxVar r m -> m (TxWrite r m,TxWake m)
commitDynTxVar onlyAllocs (DynTxU (Just (BuffTxU (txudta,txrdependents))) u txstatus@(isEvalOrWrite -> True)) = do
	-- for dependencies we change the reference itself
	case txudta of
		TxValue dirty value force txrdependencies -> do
			markOriginalTxDependencies txrdependencies
			writeRef (dataTxU u) txudta
		otherwise -> writeRef (dataTxU u) txudta
	-- for dependents we keep the reference and update the contents
	deps <- WeakSet.toWeakSList txrdependents
	WeakSet.modifyWeak (dependentsTxNM $ metaTxU u) $ Prelude.const deps
	case txstatus of
		Eval -> return ((Set.empty,return ()),(return (),return ()))
		Write -> if onlyAllocs
			then return ((Set.empty,return ()),(return (),return ()))
			else do
				wakes <- wakeUpWaits (return ()) (metaTxU u)
				return ((Set.singleton $ idTxNM $ metaTxU u,return ()),wakes) -- the thunk didn't change directly, so no correction action is due
commitDynTxVar onlyAllocs (DynTxU Nothing u New) = do
	-- in this case we simply need to mark the dependencies of the variable as original
	dta <- readRef (dataTxU u)
	case dta of
		TxValue dirty value force dependencies -> do
			markOriginalTxDependencies dependencies
		otherwise -> return ()
	return ((Set.empty,return ()),(return (),return ()))
commitDynTxVar onlyAllocs (DynTxM (Just (BuffTxM (value,txrdependents))) m txstatus@(isEvalOrWrite -> True)) = do
	writeRef (dataTxM m) value
	deps <- WeakSet.toWeakSList txrdependents
	WeakSet.modifyWeak (dependentsTxNM $ metaTxM m) $ Prelude.const deps
	case txstatus of
		Eval -> return ((Set.empty,return ()),(return (),return ()))
		Write -> if onlyAllocs
			then return ((Set.empty,return ()),(return (),return ()))
			else do
				wakes <- wakeUpWaits (setTxM m value) (metaTxM m)
				return ((Set.singleton $ idTxNM $ metaTxM m,setTxM m value),wakes) -- store an operation that will correct the modifiable and propagate to its dependents
commitDynTxVar onlyAllocs _ = return ((Set.empty,return ()),(return (),return ()))

-- applies a buffered log to the global state
-- note that we report changes as a whole, since thunks don't report modification on their own
commitTxLog :: (TxLayer Outside r m,MonadIO m,MonadRef r m) => Bool -> TxLog r m -> m (TxWrite r m,TxWake m)
commitTxLog onlyAllocs txlog = do
	liftIO $ commitTxLogMemoTables txlog
	list <- liftIO $ HashIO.toList (txLogBuff txlog)
	Control.Monad.foldM (\((xs,ops),(buffs,wakes)) (uid,dyntxvar) -> commitDynTxVar onlyAllocs dyntxvar >>= \((x,op),(buff,wake)) -> return ((xs `Set.union` x,ops >> op),(buffs >> buff,wakes >> wake))) ((Set.empty,return ()),(return (),return ())) list
	-- makes the memoized thunks created in this log persistent

-- | registers a thunk to be woken up by modifications on the variables that it reads
-- we need to delete writes on retry, otherwise the effects of a failed retry may become visible (for nested txs this would even lead to inconsistencies!)
-- note that we nevertheless wait on writes, since they could have read the variable before writting to it
waitDynTxVar :: (TxLayer Outside r m,MonadIO m,MonadRef r m) => TxEnv r m -> Lock -> Unique -> DynTxVar r m -> m ()
waitDynTxVar txenv lck uid (DynTxU buff u ((<=Eval) -> True)) = enqueueWait txenv lck (metaTxU u)
waitDynTxVar txenv@(_,_,[txlog]) lck uid (DynTxU buff u Write) = do
	enqueueWait txenv lck (metaTxU u)
	liftIO $ HashIO.delete (txLogBuff txlog) uid
waitDynTxVar txenv lck uid (DynTxU buff u New) = return ()
waitDynTxVar txenv lck uid (DynTxM buff m ((<=Eval) -> True)) = enqueueWait txenv lck (metaTxM m)
waitDynTxVar txenv@(_,_,[txlog]) lck uid (DynTxM buff m Write) = do
	enqueueWait txenv lck (metaTxM m)
	liftIO $ HashIO.delete (txLogBuff txlog) uid
waitDynTxVar txenv lck uid (DynTxM buff m New) = return ()

-- | registers waits for a transaction's reads
waitTxLog :: (TxLayer Outside r m,MonadIO m,MonadRef r m) => Lock -> TxEnv r m -> m ()
waitTxLog lck txenv@(_,_,[txlog]) = do
	list <- liftIO $ HashIO.toList (txLogBuff txlog)
	Control.Monad.mapM_ (\(uid,dyntxvar) -> waitDynTxVar txenv lck uid dyntxvar) list

instance (TxLayer Outside r m,MonadIO m,Incremental TxAdapton r m) => Transactional TxAdapton r m where
	
	atomically = atomicallyTx
	retry = retryTx
	orElse = orElseTx
	throw = throwTx
	catch = catchTx

throwTx :: (TxLayer Outside r m,MonadThrow m,Exception e) => e -> Outside TxAdapton r m a
throwTx = Catch.throwM

catchTx :: (TxLayer Outside r m,MonadCatch m,Exception e) => Outside TxAdapton r m a -> (e -> Outside TxAdapton r m a) -> Outside TxAdapton r m a
catchTx stm h = stm `Catch.catches` [Catch.Handler catchInvalid,Catch.Handler catchRetry,Catch.Handler catchSome]
	where
	catchInvalid (e::InvalidTx) = throwM e
	catchRetry (e::BlockedOnRetry) = throwM e
	catchSome (e::SomeException) = do
		-- in case the computation raises an exception, discard all its visible (write) effects
		readTxLog >>= inL . unbufferWrites . head
		h $ fromJust $ fromException e

atomicallyTx :: TxLayer Outside r m => Outside TxAdapton r m a -> m a
atomicallyTx stm = runIncremental try where
	try = do
		-- run the tx
		x <- stm `Catch.catches` [Catch.Handler catchInvalid ,Catch.Handler catchRetry ,Catch.Handler catchSome]
		-- tries to commit the current tx, otherwise repairs it incrementally
		success <- atomicTx $ validateAndCommitTopTx False
		if success then return x else try
	catchInvalid InvalidTx = try
	catchRetry BlockedOnRetry = do
		-- if the retry was invoked on an inconsistent state, we incrementally repair and run again, otherwise we place the tx in the waiting queue
		mbsuccess <- atomicTx $ validateAndRetryTopTx
		case mbsuccess of
			Just lck -> do-- retried txs are always in a consistent state, because we apply all affecting updates before releasing the lock
				-- wait for the lock to be released (whenever some variables that it depends on are changed)
				inL (liftIO $ Lock.acquire lck)
				try
			Nothing -> try
	catchSome (e::SomeException) = do
		-- we still need to validate on exceptions, otherwise repair incrementally; transaction-local allocations still get committed
		success <- atomicTx $ validateAndCommitTopTx True
		if success then throwM e else try

-- if an inner tx validation fails, then we throw an @InvalidTx@ exception to retry the whole atomic block
data InvalidTx = InvalidTx deriving (Show,Typeable)
instance Exception InvalidTx
data BlockedOnRetry = BlockedOnRetry deriving (Show,Typeable)
instance Exception BlockedOnRetry

retryTx :: TxLayer Outside r m => Outside TxAdapton r m a
retryTx = inL $ liftIO $ throwIO BlockedOnRetry

-- if an alternative retries, its non-write effects are merged with the parent tx log, to allow IC reuse; when the alternative is retried, the parent log will already contain its previous data.
-- if both alternatives retry, then both their logs will be merged with the parent, as with STM
orElseTx :: TxLayer Outside r m => Outside TxAdapton r m a -> Outside TxAdapton r m a -> Outside TxAdapton r m a
orElseTx stm1 stm2 = do1 `Catch.catches` [Catch.Handler catchInvalid,Catch.Handler catchSome]
	where
	try1 = do { x <- stm1; atomicTx (validateAndCommitNestedTx Nothing); return x }
	try2 = do { x <- stm2; atomicTx (validateAndCommitNestedTx Nothing); return x }
	do1 = inL (liftIO emptyTxLog) >>= \txlog1 -> startNestedTx txlog1 (try1 `Catch.catch` catchRetry1)
	do2 = inL (liftIO emptyTxLog) >>= \txlog2 -> startNestedTx txlog2 (try2 `Catch.catch` catchRetry2)
	catchRetry1 BlockedOnRetry = atomicTx validateAndRetryNestedTx >> do2
	catchRetry2 BlockedOnRetry = atomicTx validateAndRetryNestedTx >> throwM BlockedOnRetry
	catchInvalid (e::InvalidTx) = throwM e
	catchSome (e::SomeException) = atomicTx (validateAndCommitNestedTx (Just e)) >> throwM e

startNestedTx txlog = Reader.local (\(starttime,stack,txlogs) -> (starttime,stack,txlog:txlogs))

-- validates a nested tx and its enclosign txs
validateTxs :: MonadIO m => UTCTime -> TxCallStack r m -> [TxLog r m] -> m (Maybe (m ()))
validateTxs starttime callstack [] = return Nothing
validateTxs starttime callstack env@(txlog:txlogs) = do
	mb0 <- liftIO $ validateTx starttime txlog
	mb2 <- validateTxs starttime callstack txlogs
	let mb1 = fmap (\conflicts -> Reader.runReaderT (runTxOuter conflicts) (starttime,callstack,env)) mb0
	return $ concatMaybesM [mb1,mb2]

validateTx :: Monad m => UTCTime -> TxLog r m -> IO (Maybe (TxWrite' r m))
validateTx starttime txlog = Lock.with txLock $ do
	-- gets the transactions that committed after the current transaction's start time
	finished <- liftM (List.map Prelude.snd . Map.toAscList . Map.filterWithKey (\k v -> k > starttime)) $ readMVar doneTxs
	checkTx txlog finished

commitTopTx :: (TxLayer Outside r m,MonadIO m,MonadRef r m) => Bool -> UTCTime -> TxLog r m -> m ()
commitTopTx onlyAllocs starttime txlog = do
	-- deletes this transaction from the running list and gets the earliest running tx 
	mbearliestTx <- liftIO $ modifyMVar runningTxs (\xs -> return (List.delete starttime xs,lastMay xs))
	-- commits the log and gets a sequence of performed writes
	(writes,(buffwrites,wakeups)) <- commitTxLog onlyAllocs txlog
	-- finishes the current tx and deletes txs that finished before the start of the earliest running tx
	case mbearliestTx of
		Just earliestTx -> liftIO $ modifyMVar_ doneTxs (\m -> getCurrentTime >>= \now -> return $ Map.filterWithKey (\t _ -> t > earliestTx) $ Map.insert now writes m)
		Nothing -> liftIO $ modifyMVar_ doneTxs (\m -> getCurrentTime >>= \now -> return $ Map.insert now writes m)
	-- updates buffered content of the transactions to be retried
	buffwrites
	-- wakes up the transactions (note that this needs to be done in a second step, to ensure that all updates have been performed)
	liftIO $ wakeups

-- performed as a single atomic operation, by using a global lock
-- returns a bool stating whether the transaction was committed or needs to be incrementally repaired
-- no exceptions should be raised inside this block
validateAndCommitTopTx :: TxLayer Outside r m => Bool -> Outside TxAdapton r m Bool
validateAndCommitTopTx onlyAllocs = do
	(starttime,callstack,[txlog]) <- Reader.ask
	mbsuccess <- inL $ validateTxs starttime callstack [txlog]
	case mbsuccess of
		Nothing -> do
			inL $ commitTopTx onlyAllocs starttime txlog
			return True
		Just conflicts -> do
			-- delete the running tx; it will get a new timestamp once it is retried
			inL $ liftIO $ modifyMVar_ runningTxs (return . List.delete starttime)
			-- applies the conflicting changes to repair the current tx
			inL conflicts
			return False

validateAndCommitNestedTx :: TxLayer Outside r m => Maybe SomeException -> Outside TxAdapton r m ()
validateAndCommitNestedTx mbException = do
	txenv@(starttime,callstack,txlogs@(txlog1:txlog2:_)) <- Reader.ask
	case mbException of
		Just e -> do -- only merge allocations with the parent, since validation will be performed by the top-level tx
			inL $ mergeAllocsTxLog txlog1 txlog2
			throwM e
		Nothing -> do
			-- validates the current and enclosing txs
			mbsuccess <- inL $ validateTxs starttime callstack txlogs
			case mbsuccess of
				Nothing -> do
					-- merge the modifications with the parent log
					inL $ mergeTxLog txlog1 txlog2
					-- merges the buffered memo table entries for a txlog with its parent
					inL $ mergeTxLogMemos txlog1 txlog2
				Just conflicts -> do
					-- delete the running tx; it will get a new timestamp once it is retried
					inL $ liftIO $ modifyMVar_ runningTxs (return . List.delete starttime)
					-- applies the conflicting changes (under their respective environments) to repair the current and enclosing txs
					inL conflicts
					throwM InvalidTx

-- validates a transaction and places it into the waiting queue for retrying
validateAndRetryTopTx :: TxLayer Outside r m => Outside TxAdapton r m (Maybe Lock)
validateAndRetryTopTx = do
	txenv@(starttime,callstack,[txlog]) <- Reader.ask
	mbsuccess <- inL $ validateTxs starttime callstack [txlog]
	case mbsuccess of
		Nothing -> do
			lck <- inL $ liftIO $ Lock.newAcquired
			inL $ waitTxLog lck txenv
			return $ Just lck
		Just conflicts -> do
			-- delete the running tx; it will get a new timestamp once it is retried
			inL $ liftIO $ modifyMVar_ runningTxs (return . List.delete starttime)
			-- applies the conflicting changes to repair the current tx
			inL conflicts
			return Nothing

-- validates a nested transaction and merges its log with its parent, so that the parent
-- note that merging discards the tx's writes
validateAndRetryNestedTx :: TxLayer Outside r m => Outside TxAdapton r m ()
validateAndRetryNestedTx = do
	(starttime,callstack,txlogs@(txlog1:txlog2:_)) <- Reader.ask
	mbsuccess <- inL $ validateTxs starttime callstack txlogs
	case mbsuccess of
		Nothing -> do
			-- makes the parent log sensitive to the variables used in the nested branch
			inL $ extendTxLog txlog1 txlog2
			-- merges the buffered memo table entries for a txlog with its parent
			inL $ mergeTxLogMemos txlog1 txlog2
		Just conflicts -> do
			-- delete the running tx; it will get a new timestamp once it is retried
			inL $ liftIO $ modifyMVar_ runningTxs (return . List.delete starttime)
			-- applies the conflicting changes to repair the current tx
			inL conflicts
			throwM InvalidTx

-- runs transaction-specific code atomically in respect to a global state
atomicTx :: TxLayer Outside r m => Outside TxAdapton r m a -> Outside TxAdapton r m a
atomicTx stm = do
	inL $ liftIO $ Lock.acquire txLock
	x <- stm
	inL $ liftIO $ release txLock
	return x


