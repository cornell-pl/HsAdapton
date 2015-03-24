{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, DeriveDataTypeable, ScopedTypeVariables, UndecidableInstances, MultiParamTypeClasses, FlexibleInstances, MagicHash, ViewPatterns, BangPatterns, ConstraintKinds, FlexibleContexts #-}

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
import Control.Applicative
import Control.Concurrent.Chan
import System.IO.Unsafe

import Control.Monad.Transactional.TxAdapton.Types
import Control.Monad.Transactional.TxAdapton.Layers

import qualified Control.Concurrent.STM as STM

import System.Mem.WeakSet as WeakSet
import Data.Unique
import Control.Monad.Ref
import Control.Monad.IO.Class
import System.Mem.WeakKey as WeakKey
import Data.Strict.Maybe as Strict
import Data.Strict.List as Strict
import Data.Strict.Tuple as Strict
import System.Mem.Weak as Weak
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map
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
import Control.Monad.Catch (MonadCatch,MonadMask,MonadThrow)
import qualified Control.Monad.Catch as Catch
import System.Mem.WeakTable as WeakTable
import System.Mem.Concurrent.WeakMap as CWeakMap
import Safe
import Control.Monad.Trans
import Data.IORef
import Control.Monad.Reader (ReaderT(..),MonadReader(..))
import qualified Control.Monad.Reader
import Control.Monad.Lazy
import Control.Monad.Catch (MonadCatch,MonadThrow)
import qualified Control.Monad.Catch as Catch

import Debug

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

modInnerTxM :: (IncK TxAdapton a,MonadIO m,TxLayer Inside r m) => Inside TxAdapton r m a -> Inside TxAdapton r m (TxM Inside TxAdapton r m a)
modInnerTxM m = m >>= refInnerTxM

modOuterTxM :: (IncK TxAdapton a,TxLayer Outside r m,MonadIO m) => Outside TxAdapton r m a -> Outside TxAdapton r m (TxM Outside TxAdapton r m a)
modOuterTxM m = m >>= refOuterTxM

refOuterTxM :: (IncK TxAdapton a,TxLayer l r m,MonadIO m,TxLayer Outside r m) => a -> Outside TxAdapton r m (TxM l TxAdapton r m a)
refOuterTxM v = do
	idU <- inL $ liftIO newUnique
	dta <- inL $ newRef v
	dependentsU <- inL $ liftIO $ CWeakMap.new
	-- since the ref will never be reused, we don't need to worry about it's creator
	waitQ <- inL $ liftIO newQ
	lck <- inL $ liftIO newTLockIO
	let m = TxM (dta,(TxNodeMeta (idU,dependentsU,(Prelude.const $ return ()),(Prelude.const $ return ()),bufferTxM m,Nothing,waitQ,lck)))
	newTxMLog m
	return m

refInnerTxM :: (IncK TxAdapton a,TxLayer l r m,MonadIO m,TxLayer Inside r m) => a -> Inside TxAdapton r m (TxM l TxAdapton r m a)
refInnerTxM v = do
	idU <- inL $ liftIO newUnique
	dta <- inL $ newRef v
	dependentsU <- inL $ liftIO $ CWeakMap.new
	-- add a reference dependency (they are transitive up to the top-level calling thunk)
	creator <- mkRefCreatorTx idU
	waitQ <- inL $ liftIO newQ
	lck <- inL $ liftIO newTLockIO
	let m = TxM (dta,(TxNodeMeta (idU,dependentsU,(Prelude.const $ return ()),(Prelude.const $ return ()),bufferTxM m,creator,waitQ,lck)))
	newTxMLog m
	return m

{-# INLINE getInnerTxM #-}
getInnerTxM :: (IncK TxAdapton a,MonadIO m,TxLayer Inside r m) => TxM Inside TxAdapton r m a -> Inside TxAdapton r m a
getInnerTxM = \t -> do
	value <- readTxMValue t -- read from the buffer
	addDependencyTx (metaTxM t) (checkTxM t $! value) -- updates dependencies of callers
	return value

{-# INLINE getOuterTxM #-}	
getOuterTxM :: (IncK TxAdapton a,MonadIO m,TxLayer l r m,TxLayer Outside r m) => TxM l TxAdapton r m a -> Outside TxAdapton r m a
getOuterTxM = \t -> readTxMValue t

{-# INLINE checkTxM #-}
checkTxM :: (IncK TxAdapton a,TxLayer Inside r m,MonadIO m) => TxM Inside TxAdapton r m a -> a -> Inside TxAdapton r m Bool
checkTxM t oldv = do
	value <- readTxMValue t
	return $ oldv == value

setTxM :: (IncK TxAdapton a,TxLayer Outside r m,MonadIO m,TxLayer l r m) => TxM l TxAdapton r m a -> a -> Outside TxAdapton r m ()
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
	memo = memoTxU
	{-# INLINE memo #-}
	memoNamed = memoTxUNamed
	{-# INLINE memoNamed #-}
--	gmemoQ = gmemoQTxU
--	{-# INLINE gmemoQ #-}

memoTxU :: (IncK TxAdapton a,TxLayer Inside r m,Memo arg) => ((arg -> Inside TxAdapton r m (TxU Inside TxAdapton r m a)) -> arg -> Inside TxAdapton r m a) -> (arg -> Inside TxAdapton r m (TxU Inside TxAdapton r m a))
memoTxU f = let memo_func = memoNonRecTxU MemoLinear (thunkTxU . f memo_func) in memo_func

memoTxUNamed :: (Memo name,IncK TxAdapton a,TxLayer Inside r m,Memo arg) => name -> ((arg -> Inside TxAdapton r m (TxU Inside TxAdapton r m a)) -> arg -> Inside TxAdapton r m a) -> (arg -> Inside TxAdapton r m (TxU Inside TxAdapton r m a))
memoTxUNamed name f = let memo_func = memoNonRecTxUNamed MemoLinear name (thunkTxU . f memo_func) in memo_func

thunkTxU :: (IncK TxAdapton a,MonadIO m,TxLayer l r m,TxLayer l1 r m) => l1 TxAdapton r m a -> l TxAdapton r m (TxU l1 TxAdapton r m a)
thunkTxU c = do
	idU <- inL $ liftIO newUnique
	dta <- inL $ newRef (TxThunk c)
	dependentsU <- inL $ liftIO $ CWeakMap.new
	waitQ <- inL $ liftIO $ newQ
	lck <- inL $ liftIO newTLockIO
	let u = TxU (dta,(TxNodeMeta (idU,dependentsU,changeDirtyValueTx True Write u,forgetUDataTx u,bufferTxU u,Nothing,waitQ,lck)))
	newTxULog u
	return u

constTxU :: (IncK TxAdapton a,MonadIO m,TxLayer l r m,TxLayer l1 r m) => a -> l TxAdapton r m (TxU l1 TxAdapton r m a)
constTxU v = do
	idU <- inL $ liftIO newUnique
	dta <- inL $ newRef (TxConst v)
	dependentsU <- inL $ liftIO $ CWeakMap.new
	waitQ <- inL $ liftIO $ newQ
	lck <- inL $ liftIO newTLockIO
	let u = TxU (dta,(TxNodeMeta (idU,dependentsU,(Prelude.const $ return ()),forgetUDataTx u,bufferTxU u,Nothing,waitQ,lck)))
	newTxULog u
	return u

forceOuterTxU :: (IncK TxAdapton a,MonadIO m,TxLayer Outside r m) => TxU Outside TxAdapton r m a -> Outside TxAdapton r m a
forceOuterTxU = error "forceOuter"

forceInnerTxU :: (IncK TxAdapton a,MonadIO m,TxLayer Inside r m) => TxU Inside TxAdapton r m a -> Inside TxAdapton r m a
forceInnerTxU = \t -> do
	value <- forceNoDependentsTxU t
	addDependencyTx (metaTxU t) (checkTxU t $! value)
	return value

hasDependenciesTxU :: (IncK TxAdapton a,TxLayer Inside r m) => TxU Inside TxAdapton r m a -> Inside TxAdapton r m Bool
hasDependenciesTxU t = do
	d <- readTxUValue t (Read 1)
	case d of
		TxValue _ value force dependencies -> liftM (not . Strict.null) $ inL $ readRef dependencies
		TxThunk force -> error "cannot test dependencies of unevaluated thunk"
		TxConst value -> return False

-- in case we repair the thunks, we need to make sure that the cached value/dependencies match
{-# INLINE forceNoDependentsTxU #-}
forceNoDependentsTxU :: (IncK TxAdapton a,MonadIO m,TxLayer Inside r m) => TxU Inside TxAdapton r m a -> Inside TxAdapton r m a
forceNoDependentsTxU = forceNoDependentsTxU' (Read 1) where
	forceNoDependentsTxU' status = \t -> do
		d <- readTxUValue t status
		case d of
			TxValue 0# value force dependencies -> return value 
			TxValue 1# value force dependencies -> if (status==Eval) then repairInnerTxU t value force dependencies else forceNoDependentsTxU' Eval t
			TxThunk force -> inL (newRef SNil) >>= evaluateInnerTxU t force
			TxConst value -> return value

-- in case we repair the thunks, we need to make sure that the cached value/dependencies match
checkTxU :: (IncK TxAdapton a,MonadIO m,TxLayer Inside r m) => TxU Inside TxAdapton r m a -> a -> Inside TxAdapton r m Bool
checkTxU = checkTxU' (Read 1) where
	checkTxU' status t oldv = do
		d <- readTxUValue t status
		case d of
			TxValue 0# value force dependencies -> return (oldv==value) -- since the variable may have been dirtied and re-evaluated since the last time we looked at it
			TxValue 1# value force dependencies -> if (status==Eval) then liftM (oldv ==) (repairInnerTxU t value force dependencies) else checkTxU' Eval t oldv
			TxThunk _ -> return False 
			TxConst value -> return False

repairInnerTxU :: (IncK TxAdapton a,MonadIO m,TxLayer Inside r m) => TxU Inside TxAdapton r m a -> a -> Inside TxAdapton r m a -> TxDependencies r m -> Inside TxAdapton r m a
repairInnerTxU t value force txdependencies = do
		tbl <- readTxLog
		inL (readRef txdependencies) >>= Foldable.foldr (repair' t force tbl txdependencies) (norepair' t value tbl) . Strict.reverse --we need to reverse the dependency list to respect evaluation order
	where
	{-# INLINE norepair' #-}
	norepair' :: (IncK TxAdapton a,TxLayer Inside r m) => TxU Inside TxAdapton r m a -> a -> TxLogs r m -> Inside TxAdapton r m a
	norepair' t value tbl = inL (changeDirtyValueTx False Eval t tbl) >> return value
	{-# INLINE repair' #-}
	repair' :: (IncK TxAdapton a,TxLayer Inside r m) => TxU Inside TxAdapton r m a -> Inside TxAdapton r m a -> TxLogs r m -> TxDependencies r m -> (TxDependency r m,Weak (TxDependency r m)) -> Inside TxAdapton r m a -> Inside TxAdapton r m a
	repair' t force tbl txdependencies (d,w) m = do
		isDirty <- inL $ readRef (dirtyTxW d)
		if isDirty
			then do
				txdependents <- inL $ getTxDependents tbl (srcMetaTxW d) (Read 3) -- we only modify the dependents
				inL $ changeDependencyTx txdependencies txdependents d False
				ok <- checkTxW d
				if ok then m else inL (clearDependenciesTx False txdependencies >> newRef SNil) >>= evaluateInnerTxU t force
			else m

{-# INLINE evaluateInnerTxU #-}
evaluateInnerTxU :: (IncK TxAdapton a,MonadIO m,TxLayer Inside r m) => TxU Inside TxAdapton r m a -> Inside TxAdapton r m a -> TxDependencies r m -> Inside TxAdapton r m a
evaluateInnerTxU t force txdependencies = do
	pushTxStack (metaTxU t :!: SJust txdependencies)
	value <- force
	writeTxUValue t (TxValue 0# value force txdependencies , Nothing) Eval -- we modify the value
	popTxStack
	return value

isDirtyUnevaluatedTxU :: (IncK TxAdapton a,TxLayer l1 r m,TxLayer l r m) => TxU l1 TxAdapton r m a -> l TxAdapton r m (Maybe Bool)
isDirtyUnevaluatedTxU t = do
	d <- readTxUValue t $ Read 1
	case d of
		TxThunk force -> return Nothing --unevaluated thunk
		TxConst value -> return $ Just False -- constant value
		TxValue 1# value force dependencies -> return $ Just True -- dirty
		TxValue 0# value force dependencies -> return $ Just False -- not dirty

isUnevaluatedTxU :: (IncK TxAdapton a,TxLayer l1 r m,TxLayer l r m) => TxU l1 TxAdapton r m a -> l TxAdapton r m Bool
isUnevaluatedTxU t = do
	d <- readTxUValue t $ Read 1
	case d of
		TxThunk force -> return True --unevaluated thunk
		otherwise -> return False

oldvalueTxU :: (IncK TxAdapton a,TxLayer l r m,TxLayer l1 r m) => TxU l1 TxAdapton r m a -> l TxAdapton r m a
oldvalueTxU t = do
	d <- readTxUValue t $ Read 1
	case d of
		TxValue dirty value force dependencies -> return value
		TxThunk force -> error "no old value available"
		TxConst value -> return value

-- ** auxiliary functions
	
-- makes the new node an eval or a write
changeDirtyValueTx :: (IncK TxAdapton a,TxLayer l r m,MonadRef r m,MonadIO m) => Bool -> TxStatus -> TxU l TxAdapton r m a -> TxLogs r m -> m ()
changeDirtyValueTx dirty newstatus u txlog = changeTxU u (Just chgDirty) newstatus txlog >> return () where
	chgDirty (TxValue _ value force dependencies , ori) = return (TxValue (if dirty then 1# else 0#) value force dependencies , ori)
	
forgetUDataTx :: (IncK TxAdapton a,TxLayer l r m,MonadRef r m,MonadIO m) => TxU l TxAdapton r m a -> TxLogs r m -> m ()
forgetUDataTx u txlog = changeTxU u (Just forget) Write txlog >> return () where
	forget (TxValue _ _ force dependencies , _) = clearDependenciesTx False dependencies >> return (TxThunk force , Nothing)
	forget dta = return dta

{-# INLINE mkRefCreatorTx #-}
mkRefCreatorTx :: (WeakRef r,MonadIO m,TxLayer l r m) => Unique -> l TxAdapton r m (Maybe (TxCreator r m))
mkRefCreatorTx = \idU -> do
	top <- topTxStack
	case top of
		Just (callermeta :!: SJust txcallerdependencies) -> do
			-- its ok to point to the buffered transaction dependencies reference, because the creator never changes
			weak <- inL $ liftIO $ mkWeakRefKey txcallerdependencies callermeta Nothing
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
			originalW <- newRef False -- dependencies are created within a transaction
			let dependencyW = TxDependency (calleemeta,dirtyW,check,callermeta,originalW,MkWeak (mkWeakRefKey txcallerdependencies))
			txcalleedependents <- inL $ getTxDependents tbl calleemeta (Read 3) -- only adding dependency
			let purge = CWeakMap.deleteFinalized (dependentsTxNM calleemeta) (idTxNM callermeta) >> CWeakMap.deleteFinalized txcalleedependents (idTxNM callermeta)
			weak <- inL $ liftIO $ mkWeakRefKey txcallerdependencies dependencyW (Just purge)
			inL $ insertTxDependency (idTxNM calleemeta) (dependencyW,weak) txcallerdependencies
			inL $ insertTxDependent (idTxNM callermeta) weak txcalleedependents
		otherwise -> return ()

changeDependencyTx :: (MonadRef r m,MonadIO m,WeakRef r) => TxDependencies r m -> TxDependents r m -> TxDependency r m -> Bool -> m ()
changeDependencyTx txdependencies txdependents (TxDependency (srcMetaW,dirtyW,checkW,tgtMetaW,originalW,_)) isDirty = do
	isOriginal <- readRef originalW
	if isOriginal
		then do -- when the dependency is not buffered, make a buffered non-dirty copy
			dirtyW' <- newRef isDirty
			originalW' <- newRef False -- dependencies are created within a transaction
			let dependencyW' = TxDependency (srcMetaW,dirtyW',checkW,tgtMetaW,originalW',MkWeak (mkWeakRefKey txdependencies))
			let purge = CWeakMap.deleteFinalized (dependentsTxNM srcMetaW) (idTxNM tgtMetaW) >> CWeakMap.deleteFinalized txdependents (idTxNM tgtMetaW)
			weak' <- liftIO $ mkWeakRefKey txdependencies dependencyW' (Just purge)
			insertTxDependency (idTxNM srcMetaW) (dependencyW',weak') txdependencies -- overrides the original dependency
			insertTxDependent (idTxNM tgtMetaW) weak' txdependents
		else do
			writeRef dirtyW isDirty

{-# INLINE dirtyTx #-}
dirtyTx :: (TxLayer l r m) => TxNodeMeta r m -> l TxAdapton r m ()
dirtyTx = \umeta -> do
	tbl <- readTxLog
	inL $ dirtyCreatorTx tbl (creatorTxNM umeta)
	inL $ dirtyRecursivelyTx tbl umeta

dirtyTxWeak :: (TxLayer l r m) => Weak (TxNodeMeta r m) -> l TxAdapton r m ()
dirtyTxWeak wmeta = do
	mb <- inL $ liftIO $ Weak.deRefWeak wmeta
	case mb of
		Nothing -> return ()
		Just meta -> dirtyTx meta

dirtyCreatorTx :: (MonadIO m,MonadRef r m,WeakRef r) => TxLogs r m -> Maybe (TxCreator r m) -> m ()
dirtyCreatorTx tbl Nothing = return ()
dirtyCreatorTx tbl (Just wcreator) = do
	mb <- liftIO $ deRefWeak wcreator
	case mb of
		Just creatorMeta -> do
			forgetTxNM creatorMeta tbl
			dirtyRecursivelyTx tbl creatorMeta
		Nothing -> return ()

dirtyRecursivelyTx :: (WeakRef r,MonadIO m,MonadRef r m) => TxLogs r m -> TxNodeMeta r m -> m ()
dirtyRecursivelyTx tbl meta = do
	txdependents <- getTxDependents tbl meta Write -- marks the buffered dependents as a write, since we will dirty its dependencies
	dependents <- CWeakMap.toMap txdependents
	Foldable.mapM_ (dirtyTx' txdependents) dependents
  where
	{-# INLINE dirtyTx' #-}
	dirtyTx' txdependents = \w -> do
		mb <- liftIO $ Weak.deRefWeak w
		case mb of
			Nothing -> return ()
			Just d -> do
				isDirty <- readRef (dirtyTxW d)
				unless isDirty $ do
					txdependencies <- getTxDependencies tbl (tgtMetaTxW d) Write -- marks the buffered dependencies as a write, since dirtying results from a change
					changeDependencyTx txdependencies txdependents d True
					dirtyTxNM (tgtMetaTxW d) tbl
					dirtyRecursivelyTx tbl (tgtMetaTxW d)
			
-- inserts or updates a dependency in a dependency list
insertTxDependency :: MonadRef r m => Unique -> (TxDependency r m,Weak (TxDependency r m)) -> TxDependencies r m -> m ()
insertTxDependency did d rds = mapRef updateTxDependency rds where
	updateTxDependency SNil = SCons d SNil
	updateTxDependency (SCons x xs) = if did == xid then SCons d xs else SCons x (updateTxDependency xs)
		where xid = idTxNM $ srcMetaTxW $ Prelude.fst x

insertTxDependent :: (MonadIO m,MonadRef r m) => Unique -> Weak (TxDependent r m) -> TxDependents r m -> m ()
insertTxDependent did d deps = CWeakMap.insertWeak deps did d

-- ** Transactional support

-- | commits local buffered changes to the original thunk, and returns a log of writes (ignores reads, evals and news)
-- when @doWrites@ is turned off we simply mark new dependencies as original
-- the second returned action wakes up sleeping txs that are listening to changed modifiables
commitDynTxVar :: (TxLayer Outside r m,MonadIO m,MonadRef r m) => Bool -> DynTxVar r m -> m (TxWrite,Wakes)
commitDynTxVar doWrites (DynTxU Nothing mbtxdeps u (Read i)) = do
	-- add buffered dependents on top of persistent dependents
	case mbtxdeps of
		Just txdeps -> case i of
			2 -> CWeakMap.unionWithKey' (dependentsTxNM $ metaTxU u) txdeps
			3 -> do
				!deps <- CWeakMap.toMap txdeps
				CWeakMap.atomicModifyWeakMap_ (dependentsTxNM $ metaTxU u) $ Prelude.const $! deps
		Nothing -> return ()
	return (Map.empty,Map.empty) 
commitDynTxVar doWrites (DynTxU (Just (BuffTxU (buff_dta , txrdependents))) Nothing u txstatus@(isEvalOrWrite -> True)) = do
	-- commit the buffered data to the original thunk
	-- for dependencies we change the reference itself
	let commit = do
		(dta,ori_dependencies) <- readRef buff_dta
		case dta of
			TxValue dirty value force txrdependencies -> commitDependenciesTx txrdependencies ori_dependencies
			otherwise -> return ()
		liftIO mfence >> (writeRef (dataTxU u) $! dta)
		-- for dependents we keep the dependents reference and update its contents
		!deps <- CWeakMap.toMap txrdependents
		CWeakMap.atomicModifyWeakMap_ (dependentsTxNM $ metaTxU u) $ Prelude.const $! deps
	
	case txstatus of
		Eval -> do
			commit
			return (Map.singleton (idTxNM $ metaTxU u) False,Map.empty)
		Write -> if doWrites
			then do
				commit
				wakes <- wakeUpWaits (metaTxU u)
				let idu = (idTxNM $ metaTxU u)
				return (Map.singleton idu True,wakes)
			else return (Map.empty,Map.empty)
commitDynTxVar doWrites (DynTxU Nothing Nothing u New) = do
	return (Map.empty,Map.empty)
commitDynTxVar doWrites (DynTxM Nothing mbtxdeps m (Read i)) = do
	-- add buffered dependents on top of persistent dependents
	case mbtxdeps of
		Just txdeps -> case i of
			2 -> CWeakMap.unionWithKey' (dependentsTxNM $ metaTxM m) txdeps
			3 -> do
				!deps <- CWeakMap.toMap txdeps
				CWeakMap.atomicModifyWeakMap_ (dependentsTxNM $ metaTxM m) $ Prelude.const $! deps
		Nothing -> return ()
	return (Map.empty,Map.empty)
commitDynTxVar doWrites (DynTxM (Just (BuffTxM (value , txrdependents))) Nothing m txstatus@(isEvalOrWrite -> True)) = do
	let commit = do
		liftIO mfence >> (writeRef (dataTxM m) $! value)
		-- we do not use CAS because we only modify the original dependents under locks
		!deps <- CWeakMap.toMap txrdependents
		CWeakMap.atomicModifyWeakMap_ (dependentsTxNM $ metaTxM m) $ Prelude.const $! deps
		
	case txstatus of
		Eval -> do
			commit
			let idm = idTxNM $ metaTxM m
			return (Map.singleton idm False,Map.empty)
		Write -> if doWrites
			then do
				commit
				wakes <- wakeUpWaits (metaTxM m)
				wmeta <- liftIO $ mkWeakRefKey (dataTxM m) (metaTxM m) Nothing
				let idm = idTxNM $ metaTxM m
				return (Map.singleton idm True,wakes)
			else return (Map.empty,Map.empty)
commitDynTxVar doWrites (DynTxM Nothing Nothing m New) = do
	return (Map.empty,Map.empty)
commitDynTxVar doWrites tvar = error $ "commitDynTxVar " ++ show (dynTxStatus tvar)

-- marks a variable (its dependencies) as original
-- also ensures that @New@ variables are seen by concurrent threads in their committed version
markDynTxVar :: (TxLayer Outside r m,MonadIO m,MonadRef r m) => DynTxVar r m -> m ()
markDynTxVar (DynTxU (Just (BuffTxU (buff_dta , txrdependents))) _ u (isEvalOrWrite -> True)) = do
	(dta,_) <- readRef buff_dta
	case dta of
		TxValue dirty value force txrdependencies -> markOriginalDependenciesTx txrdependencies
		otherwise -> return ()
markDynTxVar (DynTxU Nothing _ u New) = do
	dta <- readRef (dataTxU u)
	case dta of
		TxValue dirty value force dependencies -> markOriginalDependenciesTx dependencies
		otherwise -> return ()
markDynTxVar tvar = return ()

-- applies a buffered log to the global state
-- note that we report changes as a whole, since dependent output thunks don't report modifications on their own
commitTxLog :: (TxLayer Outside r m,MonadIO m,MonadRef r m) => UTCTime -> Bool -> TxLog r m -> m ((TxUnmemo r m,TxWrite),Wakes)
commitTxLog starttime doWrites txlog = do
	-- when we do not commit writes, we need to dirty dependents of variables whose writes won't be committed
	let unwrite (uid,dyntxvar) = when (dynTxStatus dyntxvar == Write) $ dirtyBufferedDynTxVar txlog dyntxvar
	unless doWrites $ WeakTable.mapMGeneric_ unwrite (txLogBuff txlog)
	-- marks buffered data as original
	-- this needs to be done in a separate phase because commits don't follow the dependency order, i.e., dependencies may be committed before their variables, and therefore we need to make sure that all commited buffered data is seen as original by other threads
	let mark (uid,dyntxvar) = markDynTxVar dyntxvar
	WeakTable.mapMGeneric_ mark (txLogBuff txlog)
	liftIO CWeakMap.mfence
	-- commits buffered modifiable/thunk data
	let add (xs,wakes) (uid,dyntxvar) = do
		(x,wake) <- commitDynTxVar doWrites dyntxvar
--		debugTx $ "[" ++ show starttime ++ "] commited " ++ show (dynTxId dyntxvar)
		return ((xs `Map.union` x),wakes `Map.union` wake)
	(writes,wakes) <- WeakTable.foldM add (Map.empty,Map.empty) (txLogBuff txlog)
	
	-- commits transaction-local memo tables
	txunmemo <- liftIO $ commitTxLogMemoTables txlog
	-- finalize the whole buffered table
	finalizeTxLog txlog
	return ((txunmemo,writes),wakes)
	
-- | registers a thunk to be woken up by modifications on the variables that it reads
-- we need to delete writes on retry, otherwise the effects of a failed tx may become visible or lead to inconsistencies, e.g., if the flow of the program changed
-- note that we nevertheless wait on writes, since they could have read the variable before writing to it (we don't distinguish these two cases)
retryDynTxVar :: (TxLayer Outside r m,MonadIO m,MonadRef r m) => TxLog r m -> Lock -> Unique -> DynTxVar r m -> m ()
retryDynTxVar txlog lck uid tvar = when (dynTxStatus tvar < New) $ enqueueWait lck (dynTxMeta tvar)

---- | registers waits for a transaction's reads
--retryTxLogs :: (TxLayer Outside r m,MonadIO m,MonadRef r m) => Maybe Lock -> TxEnv r m -> m ()
--retryTxLogs mblck txenv@(starttime :!: stack :!: txlogs) = do
--	-- since all the enclosing txlogs have already been validated, we merge them with the top-level txlog
--	toptxlog <- flattenTxLogs txlogs
--	let toptxenv = starttime :!: stack :!: SCons toptxlog SNil
--	retryTxLog mblck toptxenv

retryTxLog :: (TxLayer Outside r m,MonadIO m,MonadRef r m) => Lock -> TxLog r m -> m ()
retryTxLog lck txlog = WeakTable.mapMGeneric_ (\(uid,dyntxvar) -> retryDynTxVar txlog lck uid dyntxvar) (txLogBuff txlog)

-- extends a base txlog with all its enclosing txlogs
flattenTxLogs :: (TxLayer Outside r m) => TxLogs r m -> m (TxLog r m)
flattenTxLogs (SCons toptxlog SNil) = return toptxlog
flattenTxLogs (SCons txlog txlogs) = do
	toptxlog <- flattenTxLogs txlogs
	commitNestedTx False txlog toptxlog -- does not perform writes
	return toptxlog

instance (Typeable r,Typeable m,TxLayer Outside r m,MonadIO m,Incremental TxAdapton r m) => Transactional TxAdapton r m where
	atomically = atomicallyTx ""
	retry = retryTx
	orElse = orElseTx
	throw = throwTx
	catch = catchTx

throwTx :: (TxLayer Outside r m,MonadThrow m,Exception e) => e -> Outside TxAdapton r m a
throwTx = Catch.throwM

catchTx :: (Typeable r,Typeable m,TxLayer Outside r m,MonadCatch m,Exception e) => Outside TxAdapton r m a -> (e -> Outside TxAdapton r m a) -> Outside TxAdapton r m a
catchTx (stm :: Outside TxAdapton r m a) (h :: e -> Outside TxAdapton r m a) = stm `Catch.catches` [Catch.Handler catchInvalid,Catch.Handler catchRetry,Catch.Handler catchSome] where
	catchInvalid (e::InvalidTx r m) = throwM e
	catchRetry (e::BlockedOnRetry) = throwM e
	catchSome (e::e) = do
		validateCatchTx "catchTx"
		h e

atomicallyTx :: (Typeable r,Typeable m,TxLayer Outside r m) => String -> Outside TxAdapton r m a -> m a
atomicallyTx msg stm = initializeTx try where
	try = flip Catch.catches [Catch.Handler catchInvalid,Catch.Handler catchRetry,Catch.Handler catchSome] $ do
		debugTx $ "started tx " ++ msg
		-- run the tx
		x <- stm
		-- tries to commit the current tx, otherwise repairs it incrementally
		mbsuccess <- validateAndCommitTopTx msg True
		case mbsuccess of
			Nothing -> do
				debugTx $ "finished tx " ++ msg
				return x
			Just (newtime,repair) -> throwM $ InvalidTx (newtime,repair)
	catchInvalid (InvalidTx (newtime,repair)) = readTxTime >>= \oldtime -> restartTx newtime $ do
		debugTx $ "caught InvalidTx: retrying tx previously known as " ++ show oldtime
		repair
		try
	catchRetry BlockedOnRetry = do
		debugTx "caught BlockedOnRetry"
		-- if the retry was invoked on an inconsistent state, we incrementally repair and run again, otherwise we place the tx in the waiting queue
		mbsuccess <- validateAndRetryTopTx msg
		case mbsuccess of
			Left lck -> do -- retried txs are always in a consistent state, because we apply all affecting updates before releasing the lock
				debugTx "put tx to sleep"
				-- wait for the lock to be released (whenever some variables that it depends on are changed)
				-- we don't consume the contents of the mvar to avoid further puts to succeeed; a new MVar is created for each retry
				inL $ liftIO $ Lock.acquire lck
				debugTx $ "woke up tx"
				readTxTime >>= \oldtime -> resetTx $ do
					debugTx $ "try: BlockedOnRetry retrying invalid tx previously known as " ++ show oldtime
					try
			Right (newtime,repair) -> restartTx newtime $ do
				repair
				try
	catchSome (e::SomeException) = do
	 	debugTx "caught SomeException"
		-- we still need to validate on exceptions, otherwise repair incrementally; transaction-local allocations still get committed
		mbsuccess <- validateAndCommitTopTx msg False
		case mbsuccess of
			Nothing -> do
				debugTx $ "finished exceptional tx " ++ msg
				throwM e
			Just (newtime,repair) -> readTxTime >>= \oldtime -> restartTx newtime $ do
				debugTx $ "try: SomeException retrying invalid tx previously known as " ++ show oldtime
				repair
				try

-- if an inner tx validation fails, then we throw an @InvalidTx@ exception to retry the whole atomic block
data InvalidTx r m = InvalidTx (TxRepair r m) deriving (Typeable)
instance Show (InvalidTx r m) where
	show (InvalidTx (time,repair)) = "InvalidTx " ++ show time
instance (Typeable r,Typeable m) => Exception (InvalidTx r m)
data BlockedOnRetry = BlockedOnRetry deriving (Show,Typeable)
instance Exception BlockedOnRetry

retryTx :: TxLayer Outside r m => Outside TxAdapton r m a
retryTx = inL $ liftIO $ throwIO BlockedOnRetry

-- if an alternative retries, its non-write effects are merged with the parent tx log, to allow IC reuse; when the alternative is retried, the parent log will already contain its previous data.
-- if both alternatives retry, then both their logs will be merged with the parent, as with STM
orElseTx :: (Typeable r,Typeable m,TxLayer Outside r m) => Outside TxAdapton r m a -> Outside TxAdapton r m a -> Outside TxAdapton r m a
orElseTx (stm1 :: Outside TxAdapton r m a) stm2 = do1 where
	try1 = do { x <- stm1; validateAndCommitNestedTx "orElse1" Nothing; return x }
	try2 = do { x <- stm2; validateAndCommitNestedTx "orElse2" Nothing; return x }
	do1 = startNestedTx $ try1 `Catch.catches` [Catch.Handler catchRetry1,Catch.Handler catchInvalid,Catch.Handler catchSome]
	do2 = startNestedTx $ try2 `Catch.catches` [Catch.Handler catchRetry2,Catch.Handler catchInvalid,Catch.Handler catchSome]
	catchRetry1 BlockedOnRetry = validateAndRetryNestedTx "orElseRetry1" >> do2
	catchRetry2 BlockedOnRetry = validateAndRetryNestedTx "orElseRetry2" >> throwM BlockedOnRetry
	catchInvalid (e::InvalidTx r m) = throwM e
	catchSome (e::SomeException) = validateAndCommitNestedTx "orElseSome" (Just e) >> throwM e

-- appends a freshly created txlog for the inner tx
startNestedTx :: TxLayer Outside r m => Outside TxAdapton r m a -> Outside TxAdapton r m a
startNestedTx m = inL (liftIO emptyTxLog) >>= \txlog -> Reader.local (\(starttime :!: stack :!: txlogs) -> (starttime :!: stack :!: SCons txlog txlogs)) m

-- validates a nested tx and its enclosing txs up the tx tree
-- the repairing action unmemoizes possible conflicting buffered memo entries and repairs variables that were conflicting
validateTxs :: (Typeable m,Typeable r,MonadIO m) => UTCTime -> TxLogs r m -> m (Maybe (TxRepair r m))
validateTxs starttime txlogs = do
	-- gets the transactions that committed after the current transaction's start time
	finished <- liftM (Map.toAscList . Map.filterWithKey (\k v -> k > starttime)) $ liftIO $ readMVar doneTxs
	mbrepairs <- validateTxs' starttime txlogs finished
	case mbrepairs of
		Nothing -> return Nothing
		Just repairs -> return $ Just (Prelude.fst $ last finished,repairs)

validateTxs' :: (Typeable m,Typeable r,MonadIO m) => UTCTime -> TxLogs r m -> [(UTCTime,(TxUnmemo r m,TxWrite))] -> m (Maybe (TxRepair' r m))
validateTxs' starttime SNil finished = return Nothing
validateTxs' starttime env@(SCons txlog txlogs) finished = do
	mb1 <- validateTx starttime txlog finished
	mb2 <- validateTxs' starttime txlogs finished
	return $ concatMaybesM [mb1,mb2]

validateTx :: (Typeable m,Typeable r,MonadIO m) => UTCTime -> TxLog r m -> [(UTCTime,(TxUnmemo r m,TxWrite))] -> m (Maybe (TxRepair' r m))
validateTx starttime txlog finished = do
	Control.Monad.mapM_ (\(txtime,(_,(txwrites))) -> debugTx' $ "[" ++ show starttime ++ "] validating against " ++ show txtime ++" "{- ++ show (Map.keys txwrites)-}) finished
	checkTx txlog finished

commitTopTx :: (TxLayer Outside r m,MonadIO m,MonadRef r m) => Bool -> UTCTime -> TxLog r m -> m ()
commitTopTx doWrites starttime txlog = do
	-- deletes this transaction from the running list and gets the earliest running tx 
	mbearliestTx <- liftIO $ modifyMVarMasked runningTxs (\xs -> return (List.delete starttime xs,lastMay xs))
	-- commits the log and gets a sequence of performed writes
	(writes@(txunmemo,txvars),wakeups) <- commitTxLog starttime doWrites txlog
	-- finishes the current tx and deletes txs that finished before the start of the earliest running tx
	-- we don't need to log transactions with empty commits (no @Eval@s or @Write@s)
	let addDone time m = if Map.null txvars then m else Map.insert time writes m
	now <- case mbearliestTx of
		Just earliestTx -> liftIO $ modifyMVarMasked doneTxs (\m -> getCurrentTime >>= \now -> let m' = Map.filterWithKey (\t _ -> t > earliestTx) (addDone now m) in m' `seq` return (m',now))
		Nothing -> liftIO $ modifyMVarMasked doneTxs (\m -> getCurrentTime >>= \now -> let m' = addDone now m in m' `seq` return (m',now))
	debugTx' $ "["++show starttime ++ "] FINISHED as " ++ show now ++ " in " ++ show (diffUTCTime now starttime) -- ++ show (Map.keys txvars)
	-- wakes up the transactions after updating their buffered content
	liftIO $ Foldable.mapM_ tryRelease wakeups

-- makes the parent log sensitive to the variables used in the nested branch
commitNestedTx :: (TxLayer Outside r m,MonadIO m,MonadRef r m) => Bool -> TxLog r m -> TxLog r m -> m ()
commitNestedTx doWrites txlog_child txlog_parent = do
	-- merge the modifications with the parent log
	if doWrites
		then mergeTxLog txlog_child txlog_parent
		else extendTxLog True txlog_child txlog_parent -- we don't need to discard @Evals@
	-- merges the buffered memo table entries for a txlog with its parent
	mergeTxLogMemos txlog_child txlog_parent
--	finalizeTxLog txlog_child

-- returns a bool stating whether the transaction was committed or needs to be incrementally repaired
-- no exceptions should be raised inside this block
validateAndCommitTopTx :: TxLayer Outside r m => String -> Bool -> Outside TxAdapton r m (Maybe (TxRepair r m))
validateAndCommitTopTx msg doWrites = atomicTx ("validateAndCommitTopTx "++msg) $ do
	txenv@(timeref :!: callstack :!: txlogs@(SCons txlog SNil)) <- Reader.ask
	starttime <- inL $ readRef timeref
	mbsuccess <- inL $ validateTxs starttime txlogs
	case mbsuccess of
		Nothing -> do
			inL $ commitTopTx doWrites starttime txlog
			return Nothing
		Just (newtime,conflicts) -> do
			-- delete the running tx; it will get a new timestamp once it is retried
			inL $ liftIO $ updateRunningTx starttime newtime
			-- discards writes and applies the conflicting changes to locally repair the current tx
			return $ Just (newtime,inL (unbufferTxLog True txlog) >> conflicts)

validateAndCommitNestedTx :: (Typeable r,Typeable m,TxLayer Outside r m) => String -> Maybe SomeException -> Outside TxAdapton r m ()
validateAndCommitNestedTx msg mbException = atomicTx ("validateAndCommitNestedTx "++msg) $ do
	txenv@(timeref :!: callstack :!: txlogs@(SCons txlog1 (SCons txlog2 _))) <- Reader.ask
	starttime <- inL $ readRef timeref
	case mbException of
		Just e -> do -- throwing an exception exits the chain of txs one by one
			inL $ commitNestedTx False txlog1 txlog2 -- does not perform @Write@s
		Nothing -> do
			-- validates the current and enclosing txs up the tx tree
			mbsuccess <- inL $ validateTxs starttime txlogs
			case mbsuccess of
				Nothing -> inL $ commitNestedTx True txlog1 txlog2 -- performs @Write@s
				Just (newtime,conflicts) -> do
					-- delete the running tx; it will get a new timestamp once it is retried
					inL $ liftIO $ updateRunningTx starttime newtime
					-- re-start from the top
					throwM $ InvalidTx (newtime,inL (flattenTxLogs txlogs >>= unbufferTxLog True) >> conflicts)

validateCatchTx :: (Typeable r,Typeable m,TxLayer Outside r m) => String -> Outside TxAdapton r m ()
validateCatchTx msg = atomicTx ("validateAndCommitCatchTx "++msg) $ do
	txenv@(timeref :!: callstack :!: txlogs@(SCons txlog _)) <- Reader.ask
	starttime <- inL $ readRef timeref
	mbsuccess <- inL $ validateTxs starttime txlogs
	case mbsuccess of
		Nothing -> do
			-- in case the computation raises an exception, discard all its visible (write) effects
			-- unbuffer only the top-level log
			inL $ unbufferTxLog True txlog
		Just (newtime,conflicts) -> do
			-- delete the running tx; it will get a new timestamp once it is retried
			inL $ liftIO $ updateRunningTx starttime newtime
			-- re-start from the top
			throwM $ InvalidTx (newtime,inL (flattenTxLogs txlogs >>= unbufferTxLog True) >> conflicts)

-- validates a transaction and places it into the waiting queue for retrying
validateAndRetryTopTx :: (Typeable r,Typeable m,TxLayer Outside r m) => String -> Outside TxAdapton r m (Either Lock (TxRepair r m))
validateAndRetryTopTx msg = atomicTx ("validateAndRetryTopTx "++msg) $ do
	txenv@(timeref :!: callstack :!: txlogs@(SCons txlog SNil)) <- Reader.ask
	starttime <- inL $ readRef timeref
	-- validates the current and enclosing txs up the tx tree
	mbsuccess <- inL $ validateTxs starttime txlogs
	case mbsuccess of
		Nothing -> do
			lck <- inL $ liftIO $ Lock.newAcquired -- sets the tx lock as acquired; the tx will be resumed when the lock is released
			inL $ commitTopTx False starttime txlog -- commit @Eval@ and @New@ computations
			inL $ retryTxLog lck txlog -- wait on changes to retry (only registers waits, does not actually wait)
			return $ Left lck
		Just (newtime,conflicts) -> do
			-- delete the running tx; it will get a new timestamp once it is retried
			inL $ liftIO $ updateRunningTx starttime newtime
			return $ Right (newtime,inL (unbufferTxLog True txlog) >> conflicts)

-- validates a nested transaction and merges its log with its parent
-- note that retrying discards the tx's writes
validateAndRetryNestedTx :: (Typeable r,Typeable m,TxLayer Outside r m) => String -> Outside TxAdapton r m ()
validateAndRetryNestedTx msg = atomicTx ("validateAndRetryNestedTx "++msg) $ do
	txenv@(timeref :!: callstack :!: txlogs@(SCons txlog1 (SCons txlog2 _))) <- Reader.ask
	starttime <- inL $ readRef timeref
	mbsuccess <- inL $ validateTxs starttime txlogs
	case mbsuccess of
		Nothing -> inL $ commitNestedTx False txlog1 txlog2 -- does not perform @Write@s on @retry@
		Just (newtime,conflicts) -> do
			-- delete the running tx; it will get a new timestamp once it is retried
			inL $ liftIO $ updateRunningTx starttime newtime
			-- discards writes and applies the conflicting changes to locally repair the current tx
			throwM $ InvalidTx (newtime,inL (flattenTxLogs txlogs >>= unbufferTxLog True) >> conflicts)

-- like STM, our txs are:
-- same as transaction repair: within transactions, we use no locks; we use locks for commit
-- 1) disjoint-access parallel: non-overlapping writes done in parallel
-- 2) read-parallel: reads done in parallel
-- 3) eval-semi-parallel: evals done in parallel, with the updates of the latest eval being discarded; both evals succeed, but their commits are not parallel though (one commits first and the other discards its evaluated data, and commits only dependents)
-- runs transaction-specific code atomically in respect to a global state
atomicTx :: (TxLayer Outside r m) => String -> Outside TxAdapton r m a -> Outside TxAdapton r m a
atomicTx msg m = do
	txlogs <- readTxLog
	(write_lcks,read_lcks) <- liftM (Map.partition Prelude.snd) $ inL $ liftIO $ txLocks txlogs
	-- wait on currently acquired read locks (to ensure that concurrent writes are seen by this tx's validation step)
	debugTx $ "waiting " ++ msg ++ " " -- ++ show (Map.keys write_lcks)
	withLocksTx (Map.map Prelude.fst read_lcks) (Map.map Prelude.fst write_lcks) $ do
		debugTx $ "locked " ++ msg
		x <- m
		debugTx $ "unlocked " ++ msg
		return x

-- acquiring the locks in sorted order is essential to avoid deadlocks!
withLocksTx :: (TxLayer l r m) => Map Unique TLock -> Map Unique TLock -> l TxAdapton r m a -> l TxAdapton r m a
withLocksTx reads writes m = do
	
	let waitAndAcquire wcks = inL $ liftIO $ STM.atomically $ do
		-- wait on read locks
		Foldable.mapM_ waitOrRetryTLock reads
		-- acquire write locks or retry
		Foldable.mapM_ acquireOrRetryTLock writes
	
	liftA2 Catch.bracket_ waitAndAcquire (inL . liftIO . STM.atomically . Foldable.mapM_ releaseTLock) writes m
	
throwM e = Catch.throwM e


instance (MonadCatch m,MonadMask m,Typeable m,Typeable r,MonadRef r m,WeakRef r,MonadIO m) => Incremental TxAdapton r m where
	
	-- a monad reader is enough since the callstack and hashtables are mutable
	newtype Outside TxAdapton r m a = TxOuter { runTxOuter :: ReaderT (TxEnv r m) m a }
		deriving (Functor,Applicative,MonadLazy,Monad,MonadRef r,MonadReader (TxEnv r m),MonadThrow,MonadCatch,MonadMask) 
	newtype Inside TxAdapton r m a = TxInner { runTxInner :: ReaderT (TxEnv r m) m a }
		deriving (Functor,Applicative,MonadLazy,Monad,MonadRef r,MonadReader (TxEnv r m),MonadThrow,MonadCatch,MonadMask)
	
	world = TxOuter . runTxInner
	{-# INLINE world #-}
	unsafeWorld = TxInner . runTxOuter
	{-# INLINE unsafeWorld #-}

	runIncremental = atomicallyTx ""
	{-# INLINE runIncremental #-}

initializeTx m = do
		starttime <- liftIO startTx >>= newRef
		stack <- liftIO $ newIORef SNil
		tbl <- liftIO emptyTxLog
		Reader.runReaderT (runTxOuter m) (starttime :!: stack :!: SCons tbl SNil)

instance (Typeable m,Typeable r,Monad m) => InLayer Outside TxAdapton r m where
	inL m = TxOuter $ lift m
instance (Typeable r,Typeable m,Monad m) => InLayer Inside TxAdapton r m where
	inL m = TxInner $ lift m

-- checks if the current txlog is consistent with a sequence of concurrent modifications
-- Nothing = success
-- Just = some conflicting changes happened simultaneously to the current tx
-- if the current tx uses a variable that has been written to before, there is a conflict
-- note that we also consider write-write conflicts, since we don't log written but not read variables differently from read-then-written ones
checkTx :: (Typeable m,Typeable r,MonadIO m) => TxLog r m -> [(UTCTime,(TxUnmemo r m,TxWrite))] -> m (Maybe (TxRepair' r m))
checkTx txlog wrts = liftM concatMaybesM $ Prelude.mapM (checkTx' txlog) wrts where
	checkTx' :: (Typeable m,Typeable r,MonadIO m) => TxLog r m -> (UTCTime,(TxUnmemo r m,TxWrite)) -> m (Maybe (TxRepair' r m))
	checkTx' txlog (txtime,(unmemo,writtenIDs)) = do
		ok <- Map.foldrWithKey (\uid isWrite m1 -> m1 >>= \mb1 -> checkTxWrite txlog uid isWrite >>= \mb2 -> return $ concatMaybesM [mb1,mb2]) (return Nothing) writtenIDs
		case ok of
			Nothing -> return Nothing
			Just repair -> return $ Just $ inL $ (liftIO $ unmemo txlog) >> repair
	-- Nothing = no conflict
	checkTxWrite txlog uid isWrite = do
		mb <- liftIO $ WeakTable.lookup (txLogBuff txlog) uid
		case mb of
			Nothing -> return Nothing
			Just tvar -> do
				-- Write-XXX are always conflicts
				-- Eval-Eval and Eval-Read are not conflicts
				-- if there was a previous @Eval@ or @Write@ on a buffered variable we discard buffered content but keep buffered dependents
				if (not isWrite) && (dynTxStatus tvar /= Write)
					then do -- not a conflict, unbuffer right away without dirtying
						unbufferDynTxVar False False txlog tvar
						return Nothing
					else do -- a conflict, unbuffer only later when tx is retried with dirtying
					 	return $ Just $ unbufferDynTxVar True False txlog tvar

-- restarts a tx with a new starting time
-- note that the time has already been added to the @runningTxs@
restartTx :: TxLayer Outside r m => UTCTime -> Outside TxAdapton r m a -> Outside TxAdapton r m a
restartTx newtime m = do
 	(timeref :!: stack :!: logs) <- Reader.ask
	writeRef timeref newtime
	m
