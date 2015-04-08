{-# LANGUAGE CPP, TupleSections, GeneralizedNewtypeDeriving, TypeFamilies, DeriveDataTypeable, ScopedTypeVariables, UndecidableInstances, MultiParamTypeClasses, FlexibleInstances, MagicHash, ViewPatterns, BangPatterns, ConstraintKinds, FlexibleContexts #-}

module Control.Monad.Transactional.TxAdapton.Algorithm where

import Debug.Trace

import System.Mem.Concurrent.WeakMap as CWeakMap
import Control.Concurrent
import Control.Monad.Incremental
import Data.Monoid
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
import Control.DeepSeq as Seq
import System.Mem

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
import System.Mem.WeakMap as WeakMap
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
refOuterTxM v = doBlock $ do
	idU <- inL $ liftIO newUnique
	dta <- inL $ newRef v
	dependentsU <- inL $ liftIO $ WeakMap.new
	-- since the ref will never be reused, we don't need to worry about it's creator
	waitQ <- inL $ liftIO newQ
	lck <- inL $ liftIO newTLockIO
#ifndef CSHF
	let m = TxM (dta,(TxNodeMeta (idU,dependentsU,(Prelude.const $ return ()),(Prelude.const $ return ()),bufferTxM m,Nothing,waitQ,lck)))
#endif
#ifdef CSHF
	notifies <- inL $ liftIO $ newMVar Map.empty
	let m = TxM (dta,(TxNodeMeta (idU,dependentsU,(Prelude.const $ return ()),(Prelude.const $ return ()),bufferTxM m,Nothing,waitQ,lck,notifies)))
#endif
	newTxMLog m
	return m

refInnerTxM :: (IncK TxAdapton a,TxLayer l r m,MonadIO m,TxLayer Inside r m) => a -> Inside TxAdapton r m (TxM l TxAdapton r m a)
refInnerTxM v = doBlock $ do
	idU <- inL $ liftIO newUnique
	dta <- inL $ newRef v
	dependentsU <- inL $ liftIO $ WeakMap.new
	-- add a reference dependency (they are transitive up to the top-level calling thunk)
	creator <- mkRefCreatorTx idU
	waitQ <- inL $ liftIO newQ
	lck <- inL $ liftIO newTLockIO
#ifndef CSHF
	let m = TxM (dta,(TxNodeMeta (idU,dependentsU,(Prelude.const $ return ()),(Prelude.const $ return ()),bufferTxM m,creator,waitQ,lck)))
#endif
#ifdef CSHF
	notifies <- inL $ liftIO $ newMVar Map.empty
	let m = TxM (dta,(TxNodeMeta (idU,dependentsU,(Prelude.const $ return ()),(Prelude.const $ return ()),bufferTxM m,creator,waitQ,lck,notifies)))
#endif
	newTxMLog m
	return m

{-# INLINE getInnerTxM #-}
getInnerTxM :: (IncK TxAdapton a,MonadIO m,TxLayer Inside r m) => TxM Inside TxAdapton r m a -> Inside TxAdapton r m a
getInnerTxM = \t -> doBlock $ do
	(value,status) <- readTxMValue t -- read from the buffer
	addDependencyTx (metaTxM t) (checkTxM t $! value) status -- updates dependencies of callers
--	str <- showIncK value
--	debugTx2 $ "getInnerTxM " ++ show (idTxNM $ metaTxM t) ++ " " ++ str
	return value

{-# INLINE getOuterTxM #-}	
getOuterTxM :: (IncK TxAdapton a,MonadIO m,TxLayer l r m,TxLayer Outside r m) => TxM l TxAdapton r m a -> Outside TxAdapton r m a
getOuterTxM = \t -> doBlock $ liftM Prelude.fst $ readTxMValue t

{-# INLINE checkTxM #-}
checkTxM :: (IncK TxAdapton a,TxLayer Inside r m,MonadIO m) => TxM Inside TxAdapton r m a -> a -> Inside TxAdapton r m (Bool,TxStatus)
checkTxM t oldv = doBlock $ do
	(value,status) <- readTxMValue t
	return (oldv == value,status)

setTxM :: (IncK TxAdapton a,TxLayer Outside r m,MonadIO m,TxLayer l r m) => TxM l TxAdapton r m a -> a -> Outside TxAdapton r m ()
setTxM t v' = doBlock $ do
	(v,_) <- readTxMValue t
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
	forceOutside = world . liftM Prelude.fst . forceNoDependentsTxU
	{-# INLINE forceOutside #-}
	memo = memoTxU
	{-# INLINE memo #-}
	memoAs = memoTxUAs
	{-# INLINE memoAs #-}
--	gmemoQ = gmemoQTxU
--	{-# INLINE gmemoQ #-}

memoTxU :: (IncK TxAdapton a,TxLayer Inside r m,Memo arg) => ((arg -> Inside TxAdapton r m (TxU Inside TxAdapton r m a)) -> arg -> Inside TxAdapton r m a) -> (arg -> Inside TxAdapton r m (TxU Inside TxAdapton r m a))
memoTxU f = let memo_func = memoNonRecTxU MemoLinear (thunkTxU . f memo_func) in memo_func

memoTxUAs :: (Memo name,IncK TxAdapton a,TxLayer Inside r m,Memo arg) => name -> ((arg -> Inside TxAdapton r m (TxU Inside TxAdapton r m a)) -> arg -> Inside TxAdapton r m a) -> (arg -> Inside TxAdapton r m (TxU Inside TxAdapton r m a))
memoTxUAs name f = let memo_func = memoNonRecTxUAs MemoLinear name (thunkTxU . f memo_func) in memo_func

thunkTxU :: (IncK TxAdapton a,MonadIO m,TxLayer l r m,TxLayer l1 r m) => l1 TxAdapton r m a -> l TxAdapton r m (TxU l1 TxAdapton r m a)
thunkTxU c = doBlock $ do
	idU <- inL $ liftIO newUnique
	dta <- inL $ newRef (TxThunk c)
	dependentsU <- inL $ liftIO $ WeakMap.new
	waitQ <- inL $ liftIO $ newQ
	lck <- inL $ liftIO newTLockIO
#ifndef CSHF
	let u = TxU (dta,(TxNodeMeta (idU,dependentsU,\txlog -> changeDirtyValueTx True (TxStatus (Write,False)) u txlog >> return (),\txlog -> forgetUDataTx u txlog >> return (),bufferTxU u,Nothing,waitQ,lck)))
#endif
#ifdef CSHF
	notifies <- inL $ liftIO $ newMVar Map.empty
	let u = TxU (dta,(TxNodeMeta (idU,dependentsU,\txlog -> changeDirtyValueTx True (TxStatus (Write,False)) u txlog >> return (),\txlog -> forgetUDataTx u txlog >> return (),bufferTxU u,Nothing,waitQ,lck,notifies)))
#endif
	newTxULog u
	return u

constTxU :: (IncK TxAdapton a,MonadIO m,TxLayer l r m,TxLayer l1 r m) => a -> l TxAdapton r m (TxU l1 TxAdapton r m a)
constTxU v = doBlock $ do
	idU <- inL $ liftIO newUnique
	dta <- inL $ newRef (TxConst v)
	dependentsU <- inL $ liftIO $ WeakMap.new
	waitQ <- inL $ liftIO $ newQ
	lck <- inL $ liftIO newTLockIO
#ifndef CSHF
	let u = TxU (dta,(TxNodeMeta (idU,dependentsU,(Prelude.const $ return ()),\txlog -> forgetUDataTx u txlog >> return (),bufferTxU u,Nothing,waitQ,lck)))
#endif
#ifdef CSHF
	notifies <- inL $ liftIO $ newMVar Map.empty
	let u = TxU (dta,(TxNodeMeta (idU,dependentsU,(Prelude.const $ return ()),\txlog -> forgetUDataTx u txlog >> return (),bufferTxU u,Nothing,waitQ,lck,notifies)))	
#endif
	newTxULog u
	return u

forceOuterTxU :: (IncK TxAdapton a,MonadIO m,TxLayer Outside r m) => TxU Outside TxAdapton r m a -> Outside TxAdapton r m a
forceOuterTxU = error "forceOuter"

forceInnerTxU :: (IncK TxAdapton a,MonadIO m,TxLayer Inside r m) => TxU Inside TxAdapton r m a -> Inside TxAdapton r m a
forceInnerTxU = \t -> doBlock $ do
	(value,status) <- forceNoDependentsTxU t
	addDependencyTx (metaTxU t) (checkTxU t $! value) status
	return value

hasDependenciesTxU :: (IncK TxAdapton a,TxLayer Inside r m) => TxU Inside TxAdapton r m a -> Inside TxAdapton r m Bool
hasDependenciesTxU t = doBlock $ do
	(d,_) <- readTxUValue t $ TxStatus (Read,False)
	case d of
		TxValue _ value force dependencies -> liftM (not . Strict.null) $ inL $ readRef dependencies
		TxThunk force -> error "cannot test dependencies of unevaluated thunk"
		TxConst value -> return False

-- in case we repair the thunks, we need to make sure that the cached value/dependencies match
{-# INLINE forceNoDependentsTxU #-}
forceNoDependentsTxU :: (IncK TxAdapton a,MonadIO m,TxLayer Inside r m) => TxU Inside TxAdapton r m a -> Inside TxAdapton r m (a,TxStatus)
forceNoDependentsTxU = \t -> doBlock $ do
	v <- forceNoDependentsTxU' Read t
--	str <- showIncK v
--	debugTx2 $ "forceNoDependentsTxU " ++ show (idTxNM $ metaTxU t) ++ " " ++ str
	return v
  where
	forceNoDependentsTxU' status = \t -> do
		(d,status') <- readTxUValue t $ TxStatus (status,False)
		case d of
			TxValue 0# value force dependencies -> return (value,status')
			TxValue 1# value force dependencies -> if (status==Eval)
				then repairInnerTxU t value force dependencies
				else forceNoDependentsTxU' Eval t
			TxThunk force -> inL (newRef SNil) >>= \deps -> evaluateInnerTxU t force deps status'
			TxConst value -> return (value,status')

-- in case we repair the thunks, we need to make sure that the cached value/dependencies match
checkTxU :: (IncK TxAdapton a,MonadIO m,TxLayer Inside r m) => TxU Inside TxAdapton r m a -> a -> Inside TxAdapton r m (Bool,TxStatus)
checkTxU t v = checkTxU' Read t v where
	checkTxU' status t oldv = do
		(d,status') <- readTxUValue t $ TxStatus (status,False)
		case d of
			TxValue 0# value force dependencies -> return (oldv==value,status') -- since the variable may have been dirtied and re-evaluated since the last time we looked at it
			TxValue 1# value force dependencies -> if (status==Eval)
				then do
					(v,status'') <- repairInnerTxU t value force dependencies
					return (oldv == v,status'') 
				else checkTxU' Eval t oldv
			TxThunk _ -> return (False,status')
			TxConst value -> return (False,status')

repairInnerTxU :: (IncK TxAdapton a,MonadIO m,TxLayer Inside r m) => TxU Inside TxAdapton r m a -> a -> Inside TxAdapton r m a -> TxDependencies r m -> Inside TxAdapton r m (a,TxStatus)
repairInnerTxU t value force txdependencies = do
		debugTx2 $ "repairing thunk "++ show (hashUnique $ idTxNM $ metaTxU t)
		tbl <- readTxLog
		(v,status') <- inL (readRef txdependencies) >>= Foldable.foldr (repair' t force tbl txdependencies) (norepair' t value tbl) . Strict.reverse --we need to reverse the dependency list to respect evaluation order
--		str <- showIncK v
--		debugTx2 $ "repaired thunk "++ show (hashUnique $ idTxNM $ metaTxU t) ++ " " ++ str
		return (v,status')
	where
	-- finishes by dirtying the node
	{-# INLINE norepair' #-}
	norepair' :: (IncK TxAdapton a,TxLayer Inside r m) => TxU Inside TxAdapton r m a -> a -> TxLogs r m -> Inside TxAdapton r m (a,TxStatus)
	norepair' t value tbl = liftM (value,) $ inL (changeDirtyValueTx False (TxStatus (Eval,False)) t tbl)
	
	-- repairs a dependency
	{-# INLINE repair' #-}
	repair' :: (IncK TxAdapton a,TxLayer Inside r m) => TxU Inside TxAdapton r m a -> Inside TxAdapton r m a -> TxLogs r m -> TxDependencies r m -> (TxDependency r m,Weak (TxDependency r m)) -> Inside TxAdapton r m (a,TxStatus) -> Inside TxAdapton r m (a,TxStatus)
	repair' t force tbl txdependencies (d,w) m = do
		isDirty <- inL $ readRef (dirtyTxW d)
		if isDirty
			then do
				txdependents <- inL $ getBufferedTxDependents tbl (srcMetaTxW d) (TxStatus (Read,True)) -- we only modify the dependents
				inL $ changeDependencyTx txdependencies txdependents d False
				(ok,src_status) <- checkTxW d
				if ok
					then liftM (\(v,s) -> (v,mappend src_status s)) m
					else inL (clearDependenciesTx False txdependencies >> newRef SNil) >>= \deps -> evaluateInnerTxU t force deps src_status
			else m

{-# INLINE evaluateInnerTxU #-}
evaluateInnerTxU :: (IncK TxAdapton a,MonadIO m,TxLayer Inside r m) => TxU Inside TxAdapton r m a -> Inside TxAdapton r m a -> TxDependencies r m -> TxStatus -> Inside TxAdapton r m (a,TxStatus)
evaluateInnerTxU t force txdependencies status = do
	txstatus <- inL $ newRef (status `mappend` (TxStatus (Eval,False)))
	pushTxStack (metaTxU t :!: SJust txdependencies :!: txstatus)
	value <- force
	-- update the status of the thunk with the children statuses (a thunk with written dependents is written)
	inner_status <- inL $ readRef txstatus 
	let newstatus = changeStatus inner_status
	-- write the value
	status' <- writeTxUValue t (TxValue 0# value force txdependencies) newstatus -- forgets the original dependencies
	popTxStack
	return (value,status')

isDirtyUnevaluatedTxU :: (IncK TxAdapton a,TxLayer l1 r m,TxLayer l r m) => TxU l1 TxAdapton r m a -> l TxAdapton r m (Maybe Bool)
isDirtyUnevaluatedTxU t = doBlock $ do
	(d,_) <- readTxUValue t $ (TxStatus (Read,False))
	case d of
		TxThunk force -> return Nothing --unevaluated thunk
		TxConst value -> return $ Just False -- constant value
		TxValue 1# value force dependencies -> return $ Just True -- dirty
		TxValue 0# value force dependencies -> return $ Just False -- not dirty

isUnevaluatedTxU :: (IncK TxAdapton a,TxLayer l1 r m,TxLayer l r m) => TxU l1 TxAdapton r m a -> l TxAdapton r m Bool
isUnevaluatedTxU t = doBlock $ do
	(d,_) <- readTxUValue t $ (TxStatus (Read,False))
	case d of
		TxThunk force -> return True --unevaluated thunk
		otherwise -> return False

oldvalueTxU :: (IncK TxAdapton a,TxLayer l r m,TxLayer l1 r m) => TxU l1 TxAdapton r m a -> l TxAdapton r m a
oldvalueTxU t = doBlock $ do
	(d,_) <- readTxUValue t $ (TxStatus (Read,False))
	case d of
		TxValue dirty value force dependencies -> return value
		TxThunk force -> error "no old value available"
		TxConst value -> return value

-- ** auxiliary functions
	
-- makes the new node an eval or a write
changeDirtyValueTx :: (IncK TxAdapton a,TxLayer l r m,MonadRef r m,MonadIO m) => Bool -> TxStatus -> TxU l TxAdapton r m a -> TxLogs r m -> m TxStatus
changeDirtyValueTx dirty newstatus u txlog = liftM dynTxStatus $ changeTxU u (Just chgDirty) newstatus txlog where
	chgDirty (TxValue _ value force dependencies , ori) = return (TxValue (if dirty then 1# else 0#) value force dependencies , ori)
	
forgetUDataTx :: (IncK TxAdapton a,TxLayer l r m,MonadRef r m,MonadIO m) => TxU l TxAdapton r m a -> TxLogs r m -> m TxStatus
forgetUDataTx u txlog = liftM dynTxStatus $ changeTxU u (Just forget) (TxStatus (Write,False)) txlog where
	forget (TxValue _ _ force dependencies , ori) = do
		clearDependenciesTx False dependencies
		ori' <- case ori of
			Left deps -> liftM Right $ liftIO $ mkWeakRefKey deps deps Nothing
			Right wdeps -> return $ Right wdeps
		return (TxThunk force , ori') -- forget original dependencies
	forget dta = return dta

{-# INLINE mkRefCreatorTx #-}
mkRefCreatorTx :: (WeakRef r,MonadIO m,TxLayer l r m) => Unique -> l TxAdapton r m (Maybe (TxCreator r m))
mkRefCreatorTx = \idU -> do
	top <- topTxStack
	case top of
		Just (callermeta :!: SJust txcallerdependencies :!: _) -> do
			-- its ok to point to the buffered transaction dependencies reference, because the creator never changes
			weak <- inL $ liftIO $ mkWeakRefKey txcallerdependencies callermeta Nothing
			return $ Just weak
		otherwise -> return Nothing

{-# INLINE addDependencyTx #-}
-- multiple dependencies on the same source node are combined into one
addDependencyTx :: (MonadIO m,MonadRef r m,WeakRef r,TxLayer Inside r m) => TxNodeMeta r m -> Inside TxAdapton r m (Bool,TxStatus) -> TxStatus -> Inside TxAdapton r m ()
addDependencyTx calleemeta check calleestatus = do
	top <- topThunkTxStack
	case top of
		Just (callermeta :!: SJust txcallerdependencies :!: callerstatus) -> do
			tbl <- readTxLog
			dirtyW <- newRef False 
			originalW <- newRef False -- dependencies are created within a transaction
			let dependencyW = TxDependency (calleemeta,dirtyW,check,callermeta,originalW,MkWeak $ mkWeakRefKey $! txcallerdependencies)
			txcalleedependents <- inL $ getBufferedTxDependents tbl calleemeta (TxStatus (Read,True)) -- only adding dependency
			let purge = WeakMap.deleteFinalized (dependentsTxNM calleemeta) (idTxNM callermeta) >> WeakMap.deleteFinalized txcalleedependents (idTxNM callermeta)
			weak <- inL $ liftIO $ mkWeakRefKey txcallerdependencies dependencyW (Just purge)
			inL $ insertTxDependency (idTxNM calleemeta) (dependencyW,weak) txcallerdependencies
			inL $ insertTxDependent (idTxNM callermeta) weak txcalleedependents
			inL $ modifyRef callerstatus (mappend calleestatus) -- join the status of the callee with the calller thunk
		otherwise -> return ()

changeDependencyTx :: (MonadRef r m,MonadIO m,WeakRef r) => TxDependencies r m -> TxDependents r m -> TxDependency r m -> Bool -> m ()
changeDependencyTx txdependencies txdependents (TxDependency (srcMetaW,dirtyW,checkW,tgtMetaW,originalW,_)) isDirty = do
	isOriginal <- readRef originalW
	if isOriginal
		then do -- when the dependency is not buffered, make a buffered non-dirty copy
			dirtyW' <- newRef isDirty
			originalW' <- newRef False -- dependencies are created within a transaction
			let dependencyW' = TxDependency (srcMetaW,dirtyW',checkW,tgtMetaW,originalW',MkWeak $ mkWeakRefKey $! txdependencies)
			let purge = WeakMap.deleteFinalized (dependentsTxNM srcMetaW) (idTxNM tgtMetaW) >> WeakMap.deleteFinalized txdependents (idTxNM tgtMetaW)
			weak' <- liftIO $ mkWeakRefKey txdependencies dependencyW' (Just purge)
			insertTxDependency (idTxNM srcMetaW) (dependencyW',weak') txdependencies -- overrides the original dependency
			insertTxDependent (idTxNM tgtMetaW) weak' txdependents
		else do
			writeRef dirtyW isDirty

{-# INLINE dirtyTx #-}
dirtyTx :: (TxLayer l r m) => TxNodeMeta r m -> l TxAdapton r m ()
dirtyTx = \umeta -> do
--	inL $ liftIO $ performGC -- try to remove dependents that can be killed and don't need to be dirtied
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
	-- we need to get ALL the dependents (original + buffered) and dirty them
	(txdependents,dependents) <- getTxDependents tbl meta (TxStatus (Write,True)) -- marks the buffered dependents as a write, we will also dirty its dependencies
	Foldable.mapM_ (dirtyTx' txdependents) dependents
  where
	{-# INLINE dirtyTx' #-}
	dirtyTx' txdependents = \d -> do
		isDirty <- readRef (dirtyTxW d)
		unless isDirty $ do -- stop if the dependency is already dirty
			txdependencies <- getTxDependencies tbl (tgtMetaTxW d) (TxStatus (Write,False)) -- marks the buffered dependencies as a write, since dirtying results from a change
			changeDependencyTx txdependencies txdependents d True
			dirtyTxNM (tgtMetaTxW d) tbl -- dirty the thunk itself
			dirtyRecursivelyTx tbl (tgtMetaTxW d)
			
-- inserts or updates a dependency in a dependency list
insertTxDependency :: MonadRef r m => Unique -> (TxDependency r m,Weak (TxDependency r m)) -> TxDependencies r m -> m ()
insertTxDependency did d rds = mapRef updateTxDependency rds where
	updateTxDependency SNil = SCons d SNil
	updateTxDependency (SCons x xs) = if did == xid then SCons d xs else SCons x (updateTxDependency xs)
		where xid = idTxNM $ srcMetaTxW $ Prelude.fst x

insertTxDependent :: (MonadIO m,MonadRef r m) => Unique -> Weak (TxDependent r m) -> TxDependents r m -> m ()
insertTxDependent did d deps = WeakMap.insertWeak deps did d

-- ** Transactional support

-- | commits local buffered changes to the original thunk, and returns a log of writes (ignores reads, evals and news)
-- when @doWrites@ is turned off we simply mark new dependencies as original
-- the second returned action wakes up sleeping txs that are listening to changed modifiables
-- we can commit buffered dependents of Writes even when doEvals is False because: if X=Write and X -buffered->Y then Y=Write
#ifndef CSHF
commitDynTxVar :: (TxLayer Outside r m,MonadIO m,MonadRef r m) => Bool -> Bool -> DynTxVar r m -> m (TxWrite,Wakes)
commitDynTxVar doWrites doEvals (DynTxU Nothing mbtxdeps u (TxStatus (Read,b))) = do
	let idu = (idTxNM $ metaTxU u)
	-- add buffered dependents on top of persistent dependents
	case (b,mbtxdeps) of
		(True,Just txdeps) -> when doEvals $ -- we only commit dependents if we have an Eval lock
			WeakMap.unionWithKey' (dependentsTxNM $ metaTxU u) txdeps
		(False,Nothing) -> return ()
	return (if (doEvals && b) then Map.singleton idu False else Map.empty,Map.empty) 
commitDynTxVar doWrites doEvals (DynTxU (Just (BuffTxU buff_dta)) mbtxdeps u (TxStatus (Eval,b))) = do
	-- commit the buffered data to the original thunk
	-- for dependencies we change the reference itself
	case (b,mbtxdeps) of
		(True,Just txdeps) -> when doEvals $ -- we only commit dependents if we have an Eval lock
			WeakMap.unionWithKey' (dependentsTxNM $ metaTxU u) txdeps
		(False,Nothing) -> return ()
	let idu = (idTxNM $ metaTxU u)
	let commit = do
		(dta,ori_dependencies) <- readRef buff_dta
		case dta of
			TxValue dirty value force txrdependencies -> commitDependenciesTx txrdependencies ori_dependencies
			otherwise -> return ()
		writeRef (dataTxU u) $! dta
		return ()
	when doEvals commit
	return (if (doEvals && b) then Map.singleton idu False else Map.empty,Map.empty)
commitDynTxVar doWrites doEvals (DynTxU (Just (BuffTxU buff_dta)) mbtxdeps u (TxStatus (Write,b))) = do
	case (b,mbtxdeps) of
		(True,Just txdeps) -> when doWrites $ -- all buffered dependents of a write must be writes
			WeakMap.unionWithKey' (dependentsTxNM $ metaTxU u) txdeps
		(False,Nothing) -> return ()
	-- commit the buffered data to the original thunk
	-- for dependencies we change the reference itself
	let commit = do
		(dta,ori_dependencies) <- readRef buff_dta
		case dta of
			TxValue dirty value force txrdependencies -> commitDependenciesTx txrdependencies ori_dependencies
			otherwise -> return ()
		writeRef (dataTxU u) $! dta
	
	let idu = (idTxNM $ metaTxU u)
	if doWrites
		then do
			commit
			wakes <- wakeUpWaits (metaTxU u)
			return (Map.singleton idu True,wakes)
		else do
			return (if (doEvals && b) then Map.singleton idu False else Map.empty,Map.empty)
commitDynTxVar doWrites doEvals (DynTxU Nothing Nothing u (TxStatus (New i,b))) = do
	let idu = (idTxNM $ metaTxU u)
	let forget ref = flip mapRefM_ ref $ \dta -> case dta of
		(TxValue _ _ force dependencies) -> clearDependenciesTx True dependencies >> return (TxThunk force)
		otherwise -> return dta
	if i
		then do
			-- if the new thunk depends on a non-committed write, we forget its value
			unless doWrites $ forget (dataTxU u)
			-- we commits its dependents anyway
			return (if doWrites then Map.singleton idu True else (if b then Map.singleton idu False else Map.empty),Map.empty) -- there are no wakeups, since no previous tx that reads this variable may have committed
		else do
			-- if the new thunk is an eval but we can't commit its dependencies to dependent evaluated variables, we forget its value
			unless doEvals $ forget (dataTxU u)
			return (if b then Map.singleton idu False else Map.empty,Map.empty)
commitDynTxVar doWrites doEvals (DynTxM Nothing mbtxdeps m (TxStatus (Read,b))) = do
	-- add buffered dependents on top of persistent dependents
	case (b,mbtxdeps) of
		(True,Just txdeps) -> when doEvals $ WeakMap.unionWithKey' (dependentsTxNM $ metaTxM m) txdeps
		(False,Nothing) -> return ()
	let idm = idTxNM $ metaTxM m
	return (if (doEvals && b) then Map.singleton idm False else Map.empty,Map.empty)
commitDynTxVar doWrites doEvals (DynTxM (Just (BuffTxM value)) mbtxdeps m (TxStatus (Eval,b))) = do
	case (b,mbtxdeps) of
		(True,Just txdeps) -> when doEvals $ -- we only commit dependents if we have an Eval lock
			WeakMap.unionWithKey' (dependentsTxNM $ metaTxM m) txdeps
		(False,Nothing) -> return ()
	
	let idm = idTxNM $ metaTxM m
	let commit = do
		writeRef (dataTxM m) $! value
		return ()
	when doEvals commit
	return (if (doEvals && b) then Map.singleton idm False else Map.empty,Map.empty)
commitDynTxVar doWrites doEvals (DynTxM (Just (BuffTxM value)) mbtxdeps m (TxStatus (Write,b))) = do
	case (b,mbtxdeps) of
		(True,Just txdeps) -> when doWrites $ -- all buffered dependents of a write must be writes
			WeakMap.unionWithKey' (dependentsTxNM $ metaTxM m) txdeps
		(False,Nothing) -> return ()
	let commit = do
		writeRef (dataTxM m) $! value
		return ()
		
	let idm = idTxNM $ metaTxM m
	if doWrites
		then do
			commit
			wakes <- wakeUpWaits (metaTxM m)
			wmeta <- liftIO $ mkWeakRefKey (dataTxM m) (metaTxM m) Nothing
			return (Map.singleton idm True,wakes)
	else return (if b then Map.singleton idm False else Map.empty,Map.empty)
commitDynTxVar doWrites doEvals (DynTxM Nothing Nothing m (TxStatus (New i,b))) = do
	let idm = (idTxNM $ metaTxM m)
	if i
		then return (Map.singleton idm True,Map.empty) -- there are no wakeups, since no previous tx that reads may have committed
		else return (if b then Map.singleton idm False else Map.empty,Map.empty)
commitDynTxVar doWrites doEvals (DynTxM dta deps m status) = error $ "commitDynTxVarM " ++ show (isJust dta) ++" "++ show (isJust deps) ++" "++ show status
commitDynTxVar doWrites doEvals (DynTxU dta deps m status) = error $ "commitDynTxVarU " ++ show (isJust dta) ++" "++ show (isJust deps) ++" "++ show status
#endif

#ifdef CSHF
type ThreadRepair r m = Map ThreadId (TxRepair' r m)

-- removes the current thread from the notify lists
unnotifyDynTxVar :: MonadIO m => ThreadId -> DynTxVar r m -> m ()
unnotifyDynTxVar tid tvar = liftIO $ modifyMVarMasked_ (notifiesTxNM $ dynTxMeta tvar) (return . Map.delete tid)

commitDynTxVar :: (TxLayer Outside r m,MonadIO m,MonadRef r m) => Bool -> Bool -> DynTxVar r m -> m (ThreadRepair r m,Wakes)
commitDynTxVar doWrites doEvals (DynTxU Nothing mbtxdeps u (TxStatus (Read,b))) = do
	let idu = (idTxNM $ metaTxU u)
	-- add buffered dependents on top of persistent dependents
	case (b,mbtxdeps) of
		(True,Just txdeps) -> when doEvals $ -- we only commit dependents if we have an Eval lock
			WeakMap.unionWithKey' (dependentsTxNM $ metaTxU u) txdeps
		(False,Nothing) -> return ()
	checks <- if doEvals && b
		then checkVarTx False idu (notifiesTxNM $ metaTxU u)
		else return Map.empty
	return (checks,Map.empty) 
commitDynTxVar doWrites doEvals (DynTxU (Just (BuffTxU buff_dta)) mbtxdeps u (TxStatus (Eval,b))) = do
	-- commit the buffered data to the original thunk
	-- for dependencies we change the reference itself
	case (b,mbtxdeps) of
		(True,Just txdeps) -> when doEvals $ -- we only commit dependents if we have an Eval lock
			WeakMap.unionWithKey' (dependentsTxNM $ metaTxU u) txdeps
		(False,Nothing) -> return ()
	let idu = (idTxNM $ metaTxU u)
	let commit = do
		(dta,ori_dependencies) <- readRef buff_dta
		case dta of
			TxValue dirty value force txrdependencies -> commitDependenciesTx txrdependencies ori_dependencies
			otherwise -> return ()
		writeRef (dataTxU u) $! dta
		return ()
	when doEvals commit
	checks <- if doEvals && b
		then checkVarTx False idu (notifiesTxNM $ metaTxU u)
		else return Map.empty
	return (checks,Map.empty)
commitDynTxVar doWrites doEvals (DynTxU (Just (BuffTxU buff_dta)) mbtxdeps u (TxStatus (Write,b))) = do
	case (b,mbtxdeps) of
		(True,Just txdeps) -> when doWrites $ -- all buffered dependents of a write must be writes
			WeakMap.unionWithKey' (dependentsTxNM $ metaTxU u) txdeps
		(False,Nothing) -> return ()
	-- commit the buffered data to the original thunk
	-- for dependencies we change the reference itself
	let commit = do
		(dta,ori_dependencies) <- readRef buff_dta
		case dta of
			TxValue dirty value force txrdependencies -> commitDependenciesTx txrdependencies ori_dependencies
			otherwise -> return ()
		writeRef (dataTxU u) $! dta
	
	let idu = (idTxNM $ metaTxU u)
	if doWrites
		then do
			commit
			wakes <- wakeUpWaits (metaTxU u)
			checks <- checkVarTx True idu (notifiesTxNM $ metaTxU u)
			return (checks,wakes)
		else do
			checks <- if doEvals && b
				then checkVarTx False idu (notifiesTxNM $ metaTxU u)
				else return Map.empty
			return (checks,Map.empty)
commitDynTxVar doWrites doEvals (DynTxU Nothing Nothing u (TxStatus (New i,b))) = do
	let idu = (idTxNM $ metaTxU u)
	let forget ref = flip mapRefM_ ref $ \dta -> case dta of
		(TxValue _ _ force dependencies) -> clearDependenciesTx True dependencies >> return (TxThunk force)
		otherwise -> return dta
	if i
		then do
			-- if the new thunk depends on a non-committed write, we forget its value
			unless doWrites $ forget (dataTxU u)
			-- we commits its dependents anyway
			checks <- if doWrites
				then checkVarTx True idu (notifiesTxNM $ metaTxU u)
				else if b
					then checkVarTx False idu (notifiesTxNM $ metaTxU u)
					else return Map.empty
			return (checks,Map.empty) -- there are no wakeups, since no previous tx that reads this variable may have committed
		else do
			-- if the new thunk is an eval but we can't commit its dependencies to dependent evaluated variables, we forget its value
			unless doEvals $ forget (dataTxU u)
			checks <- if b
				then checkVarTx False idu (notifiesTxNM $ metaTxU u)
				else return Map.empty
			return (checks,Map.empty)
commitDynTxVar doWrites doEvals (DynTxM Nothing mbtxdeps m (TxStatus (Read,b))) = do
	-- add buffered dependents on top of persistent dependents
	case (b,mbtxdeps) of
		(True,Just txdeps) -> when doEvals $ WeakMap.unionWithKey' (dependentsTxNM $ metaTxM m) txdeps
		(False,Nothing) -> return ()
	let idm = idTxNM $ metaTxM m
	checks <- if doEvals && b
		then checkVarTx False idm (notifiesTxNM $ metaTxM m)
		else return Map.empty
	return (checks,Map.empty)
commitDynTxVar doWrites doEvals (DynTxM (Just (BuffTxM value)) mbtxdeps m (TxStatus (Eval,b))) = do
	case (b,mbtxdeps) of
		(True,Just txdeps) -> when doEvals $ -- we only commit dependents if we have an Eval lock
			WeakMap.unionWithKey' (dependentsTxNM $ metaTxM m) txdeps
		(False,Nothing) -> return ()
	
	let idm = idTxNM $ metaTxM m
	let commit = do
		writeRef (dataTxM m) $! value
		return ()
	when doEvals commit
	checks <- if doEvals && b
		then checkVarTx False idm (notifiesTxNM $ metaTxM m)
		else return Map.empty
	return (checks,Map.empty)
commitDynTxVar doWrites doEvals (DynTxM (Just (BuffTxM value)) mbtxdeps m (TxStatus (Write,b))) = do
	case (b,mbtxdeps) of
		(True,Just txdeps) -> when doWrites $ -- all buffered dependents of a write must be writes
			WeakMap.unionWithKey' (dependentsTxNM $ metaTxM m) txdeps
		(False,Nothing) -> return ()
	let commit = do
		writeRef (dataTxM m) $! value
		return ()
		
	let idm = idTxNM $ metaTxM m
	if doWrites
		then do
			commit
			wakes <- wakeUpWaits (metaTxM m)
			wmeta <- liftIO $ mkWeakRefKey (dataTxM m) (metaTxM m) Nothing
			checks <- checkVarTx True idm (notifiesTxNM $ metaTxM m)
			return (checks,wakes)
	else do
		checks <- if b
			then checkVarTx False idm (notifiesTxNM $ metaTxM m)
			else return Map.empty
		return (checks,Map.empty)
commitDynTxVar doWrites doEvals (DynTxM Nothing Nothing m (TxStatus (New i,b))) = do
	let idm = (idTxNM $ metaTxM m)
	if i
		then do
			checks <- checkVarTx True idm (notifiesTxNM $ metaTxM m)
			return (checks,Map.empty) -- there are no wakeups, since no previous tx that reads may have committed
		else do
			checks <- if b
				then checkVarTx False idm (notifiesTxNM $ metaTxM m)
				else return Map.empty
			return (checks,Map.empty)
commitDynTxVar doWrites doEvals (DynTxM dta deps m status) = error $ "commitDynTxVarM " ++ show (isJust dta) ++" "++ show (isJust deps) ++" "++ show status
commitDynTxVar doWrites doEvals (DynTxU dta deps m status) = error $ "commitDynTxVarU " ++ show (isJust dta) ++" "++ show (isJust deps) ++" "++ show status
#endif

-- marks a variable (its dependencies) as original
-- also ensures that @New@ variables are seen by concurrent threads in their committed version
markDynTxVar :: (TxLayer Outside r m,MonadIO m,MonadRef r m) => DynTxVar r m -> m ()
markDynTxVar (DynTxU (Just (BuffTxU buff_dta)) _ u (isEvalOrWrite -> Just _)) = do
	(dta,_) <- readRef buff_dta
	case dta of
		TxValue dirty value force txrdependencies -> markOriginalDependenciesTx txrdependencies
		otherwise -> return ()
markDynTxVar (DynTxU _ _ u (TxStatus (New _,_))) = do
	dta <- readRef (dataTxU u)
	case dta of
		TxValue dirty value force dependencies -> markOriginalDependenciesTx dependencies
		otherwise -> return ()
markDynTxVar tvar = return ()

-- applies a buffered log to the global state
-- note that we report changes as a whole, since dependent output thunks don't report modifications on their own
#ifndef CSHF
commitTxLog :: (TxLayer Outside r m,MonadIO m,MonadRef r m) => UTCTime -> Bool -> Bool -> TxLog r m -> m ((TxUnmemo r m,TxWrite),Wakes)
commitTxLog starttime doWrites doEvals txlog = do
#endif
#ifdef CSHF
commitTxLog :: (TxLayer Outside r m,MonadIO m,MonadRef r m) => ThreadId -> UTCTime -> Bool -> Bool -> TxLog r m -> m ((TxUnmemo r m,ThreadRepair r m),Wakes)
commitTxLog tid starttime doWrites doEvals txlog = do
#endif
	-- marks buffered data as original
	-- this needs to be done in a separate phase because commits don't follow the dependency order, i.e., dependencies may be committed before their variables, and therefore we need to make sure that all commited buffered data is seen as original by other threads
	WeakTable.mapMGeneric_ (markDynTxVar . Prelude.snd) (txLogBuff txlog)
	-- commits buffered modifiable/thunk data
	let add (xs,wakes) (uid,dyntxvar) = do
#ifdef CSHF
		unnotifyDynTxVar tid dyntxvar
#endif
		(x,wake) <- commitDynTxVar doWrites doEvals dyntxvar
--		debugTx $ "[" ++ show starttime ++ "] commited " ++ show (dynTxId dyntxvar)
#ifndef CSHF
		return ((Map.union xs x),wakes `Map.union` wake)
#endif
#ifdef CSHF
		return ((Map.unionWith joinTxRepair' xs x),wakes `Map.union` wake)
#endif
	(writes,wakes) <- WeakTable.foldM add (Map.empty,Map.empty) (txLogBuff txlog)
	
	-- commits transaction-local memo tables
	txunmemo <- if (doWrites && doEvals)
		then liftIO $ commitTxLogMemoTables txlog
		else return (Prelude.const $ return ())
	-- finalize the whole buffered table
	finalizeTxLog txlog
	return ((txunmemo,writes),wakes)
	
-- | registers a thunk to be woken up by modifications on the variables that it reads
-- we need to delete writes on retry, otherwise the effects of a failed tx may become visible or lead to inconsistencies, e.g., if the flow of the program changed
-- note that we nevertheless wait on writes, since they could have read the variable before writing to it (we don't distinguish these two cases)
retryDynTxVar :: (TxLayer Outside r m,MonadIO m,MonadRef r m) => TxLog r m -> Lock -> Unique -> DynTxVar r m -> m ()
retryDynTxVar txlog lck uid tvar = when (not $ isNew $ dynTxStatus tvar) $ enqueueWait lck (dynTxMeta tvar)

retryTxLog :: (TxLayer Outside r m,MonadIO m,MonadRef r m) => Lock -> TxLog r m -> m ()
retryTxLog lck txlog = WeakTable.mapMGeneric_ (\(uid,dyntxvar) -> retryDynTxVar txlog lck uid dyntxvar) (txLogBuff txlog)

-- lifts all writes to the innermost nested txlog
liftTxLogsWrites :: (TxLayer Outside r m) => TxLogs r m -> m ()
liftTxLogsWrites (SCons txlog txlogs) = liftTxLogsWrites' txlog txlogs
  where
	liftTxLogsWrites' :: (TxLayer Outside r m) => TxLog r m -> TxLogs r m -> m ()
	liftTxLogsWrites' txlog SNil = return ()
	liftTxLogsWrites' txlog (SCons txlog1 txlogs1) = do
		liftIO $ WeakTable.mapM_ (liftWrite txlog) (txLogBuff txlog1)
		liftTxLogsWrites' txlog txlogs1
	liftWrite :: (TxLayer Outside r m) => TxLog r m -> (Unique,DynTxVar r m) -> IO ()
	liftWrite txlog (uid,tvar) = when (isWriteOrNewTrue $ dynTxStatus tvar) $ do
		mb <- WeakTable.lookup (txLogBuff txlog) uid
		case mb of
			Nothing -> WeakTable.insertWithMkWeak (txLogBuff txlog) (dynTxMkWeak tvar) uid tvar
			Just _ -> return ()

-- extends a base txlog with all its enclosing txlogs, ignoring writes in all of them
flattenTxLogs :: (TxLayer Outside r m) => TxLogs r m -> m (TxLog r m)
flattenTxLogs txlogs@(SCons toptxlog SNil) = unbufferTopTxLog txlogs True >> return toptxlog
flattenTxLogs (SCons txlog txlogs) = do
	commitNestedTx False txlog txlogs
	flattenTxLogs txlogs

instance (Typeable r,Typeable m,TxLayer Outside r m,MonadIO m,Incremental TxAdapton r m) => Transactional TxAdapton r m where
	atomically = runIncremental
	retry = retryTx
	orElse = orElseTx
	throw = throwTx
	catch = catchTx

throwTx :: (TxLayer Outside r m,MonadThrow m,Exception e) => e -> Outside TxAdapton r m a
throwTx = Catch.throwM

catchTx :: (Typeable r,Typeable m,TxLayer Outside r m,MonadCatch m,Exception e) => Outside TxAdapton r m a -> (e -> Outside TxAdapton r m a) -> Outside TxAdapton r m a
catchTx (stm :: Outside TxAdapton r m a) (h :: e -> Outside TxAdapton r m a) = stm `Catch.catches` [Catch.Handler catchInvalid,Catch.Handler catchRetry,Catch.Handler catchSome] where
	catchInvalid (e::InvalidTx r m) = Catch.throwM e
	catchRetry (e::BlockedOnRetry) = Catch.throwM e
	catchSome (e::e) = do
		validateCatchTx "catchTx"
		h e

--
atomicallyTx :: (Typeable r,Typeable m,TxLayer Outside r m) => Bool -> String -> Outside TxAdapton r m a -> m a
atomicallyTx doRepair msg stm = initializeTx try where
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
			Just (newtime,repair) -> Catch.throwM $ InvalidTx (newtime,repair)
	catchInvalid (InvalidTx (newtime,repair)) = do
		starttime <- readTxTime
		let withRepair = if doRepair then Just repair else Nothing
		let msg = "caught InvalidTx: retrying tx previously known as " ++ show starttime
		restartTxWithRepair withRepair msg starttime newtime try
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
				starttime <- readTxTime
#ifndef CSHF
				-- delete the runningTx; resetTx will generate a new starting time
				inL $ liftIO $ deleteRunningTx starttime
#endif
				resetTx $ do
					debugTx $ "try: BlockedOnRetry retrying invalid tx previously known as " ++ show starttime
					try
			Right (newtime,repair) -> do
				starttime <- readTxTime
				let withRepair = if doRepair then Just repair else Nothing
				let msg = "caughtRetry InvalidTx: retrying tx previously known as " ++ show starttime
				restartTxWithRepair withRepair msg starttime newtime try
	catchSome (e::SomeException) = do
	 	debugTx $ "caught SomeException " ++ show e
		-- we still need to validate on exceptions, otherwise repair incrementally; transaction-local allocations still get committed
		mbsuccess <- validateAndCommitTopTx msg False
		case mbsuccess of
			Nothing -> do
				debugTx $ "finished exceptional tx " ++ msg
				Catch.throwM e
			Just (newtime,repair) -> do
				starttime <- readTxTime
				let withRepair = if doRepair then Just repair else Nothing
				let msg = "try: SomeException retrying invalid tx previously known as " ++ show starttime
				restartTxWithRepair withRepair msg starttime newtime try

-- if an inner tx validation fails, then we throw an @InvalidTx@ exception to retry the whole atomic block
data InvalidTx r m = InvalidTx (InvalidTxRepair r m) deriving (Typeable)
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
	catchRetry2 BlockedOnRetry = validateAndRetryNestedTx "orElseRetry2" >> Catch.throwM BlockedOnRetry
	catchInvalid (e::InvalidTx r m) = Catch.throwM e
	catchSome (e::SomeException) = validateAndCommitNestedTx "orElseSome" (Just e) >> Catch.throwM e

-- appends a freshly created txlog for the inner tx
startNestedTx :: TxLayer Outside r m => Outside TxAdapton r m a -> Outside TxAdapton r m a
startNestedTx m = inL (liftIO emptyTxLog) >>= \txlog -> Reader.local (\(starttime :!: stack :!: txlogs) -> (starttime :!: stack :!: SCons txlog txlogs)) m

-- validates a nested tx and its enclosing txs up the tx tree
-- the repairing action unmemoizes possible conflicting buffered memo entries and repairs variables that were conflicting
validateTxs :: (Typeable m,Typeable r,MonadIO m) => UTCTime -> TxLogs r m -> m (Maybe (TxRepair r m))
#ifndef CSHF
validateTxs starttime txlogs = do
	-- gets the transactions that committed after the current transaction's start time
	txs <- liftIO $ readMVar doneTxs
	let finished = Map.toAscList $ Map.filterWithKey (\k v -> k > starttime) txs
	let finishtime = if List.null finished then starttime else Prelude.fst (List.last finished)
	mbrepairs <- validateTxs' starttime txlogs finished
	case mbrepairs of
		Nothing -> return Nothing
		Just repairs -> return $ Just (finishtime,repairs)
#endif
#ifdef CSHF
validateTxs starttime txlogs = return Nothing
#endif

validateTxs' :: (Typeable m,Typeable r,MonadIO m) => UTCTime -> TxLogs r m -> [(UTCTime,(TxUnmemo r m,TxWrite))] -> m (Maybe (TxRepair' r m))
validateTxs' starttime txlogs finished = do
	let (txtimes,txunmemos,txwrites) = compactFinished finished
	debugTx' $ "[" ++ show starttime ++ "] validating against " ++ show txtimes {- ++ ""  ++ show txwrites-}
	checkTxs txlogs (txunmemos,txwrites)

compactFinished :: [(UTCTime,(TxUnmemo r m,TxWrite))] -> ([UTCTime],TxUnmemo r m,TxWrite)
compactFinished [] = ([],Prelude.const $ return (),Map.empty)
compactFinished ((t1,(u1,w1)):xs) = let (t2,u2,w2) = compactFinished xs in (t1 : t2,u1 >> u2,Map.unionWith max w1 w2)

commitTopTx :: (TxLayer Outside r m,MonadIO m,MonadRef r m) => Bool -> Bool -> UTCTime -> TxLog r m -> m ()
#ifndef CSHF
commitTopTx doWrites doEvals starttime txlog = do
	-- deletes this transaction from the running list and gets the earliest running tx 
	mbearliestTx <- liftIO $ modifyMVarMasked runningTxs (\xs -> let xs' = List.delete starttime xs in return (xs',lastMay xs'))
	-- commits the log and gets a sequence of performed writes
	(writes@(txunmemo,txvars),wakeups) <- commitTxLog starttime doWrites doEvals txlog
	-- finishes the current tx and deletes txs that finished before the start of the earliest running tx
	-- we don't need to log transactions with empty commits (no @Eval@s or @Write@s)
	let addDone time m = if Map.null txvars then m else Map.insertWith mergeDoneTxs time writes m
	now <- case mbearliestTx of
		Just earliestTx -> liftIO $ modifyMVarMasked doneTxs (\m -> getCurrentTime >>= \now -> let m' = Map.filterWithKey (\t _ -> t > earliestTx) (addDone now m) in m' `seq` return (m',now))
		Nothing -> liftIO $ modifyMVarMasked doneTxs (\m -> getCurrentTime >>= \now -> let m' = addDone now m in m' `seq` return (m',now))
	debugTx' $ "["++show starttime ++ "] FINISHED as " ++ show now ++ " in " ++ show (diffUTCTime now starttime)  {- ++ show txvars -}
	-- wakes up the transactions after updating their buffered content
	liftIO $ Foldable.mapM_ tryRelease wakeups
#endif
#ifdef CSHF
commitTopTx doWrites doEvals starttime txlog = do
	tid <- liftIO myThreadId
	-- commits the log and gets a sequence of performed writes
	((txunmemo,notifies),wakeups) <- commitTxLog tid starttime doWrites doEvals txlog
	-- finishes the current tx and deletes txs that finished before the start of the earliest running tx
	-- we don't need to log transactions with empty commits (no @Eval@s or @Write@s)
	now <- liftIO getCurrentTime
	debugTx' $ "["++show starttime ++ "] FINISHED as " ++ show now ++ " in " ++ show (diffUTCTime now starttime)  {- ++ show txvars -}
	
	-- invalidates and repairs concurrent transactions
	let invalidate thread repair m = do
		debugTx' $ "throwing InvalidTx to " ++ show thread
		liftIO $ throwTo thread $
			InvalidTx (now,readTxLog >>= inL . flattenTxLogs >>= \txlog -> repair txlog >> inL (liftIO $ txunmemo txlog))
		m
	Map.foldrWithKey invalidate (return ()) notifies
	debugTx' $ "thrown all"
	
	-- wakes up the transactions after updating their buffered content
	liftIO $ Foldable.mapM_ tryRelease wakeups
#endif

-- makes the parent log sensitive to the variables used in the nested branch
commitNestedTx :: (TxLayer Outside r m,MonadIO m,MonadRef r m) => Bool -> TxLog r m -> TxLogs r m -> m ()
commitNestedTx doWrites txlog_child txlogs_parent@(SCons txlog_parent _) = do
	-- merge the modifications with the parent log
	if doWrites
		then mergeTxLog txlog_child txlog_parent
		else extendTxLog txlog_child txlogs_parent -- we don't need to discard @Evals@
	-- merges the buffered memo table entries for a txlog with its parent
	mergeTxLogMemos txlog_child txlog_parent
	finalizeTxLog txlog_child

-- returns a bool stating whether the transaction was committed or needs to be incrementally repaired
-- no exceptions should be raised inside this block
validateAndCommitTopTx :: TxLayer Outside r m => String -> Bool -> Outside TxAdapton r m (Maybe (InvalidTxRepair r m))
validateAndCommitTopTx msg doWrites = atomicTx ("validateAndCommitTopTx "++msg) $ \doEvals -> do
	txenv@(timeref :!: callstack :!: txlogs@(SCons txlog SNil)) <- Reader.ask
	starttime <- inL $ readRef timeref
	mbsuccess <- inL $ validateTxs starttime txlogs
	case mbsuccess of
		Nothing -> do
			inL $ commitTopTx doWrites doEvals starttime txlog
			return Nothing
		Just (newtime,conflicts) -> 
			return $ Just (newtime,inL (flattenTxLogs txlogs) >>= conflicts)

validateAndCommitNestedTx :: (Typeable r,Typeable m,TxLayer Outside r m) => String -> Maybe SomeException -> Outside TxAdapton r m ()
validateAndCommitNestedTx msg mbException = do
	txenv@(timeref :!: callstack :!: txlogs@(SCons txlog1 txlogs1)) <- Reader.ask
	starttime <- inL $ readRef timeref
	case mbException of
		Just e -> do -- throwing an exception exits the chain of txs one by one
			doBlock $ inL $ commitNestedTx False txlog1 txlogs1 -- does not perform @Write@s
		Nothing -> do
			-- validates the current and enclosing txs up the tx tree
			mbsuccess <- inL $ validateTxs starttime txlogs
			case mbsuccess of
				Nothing -> doBlock $ inL $ commitNestedTx True txlog1 txlogs1 -- performs @Write@s
				Just (newtime,conflicts) -> 
					-- re-start from the top
					Catch.throwM $ InvalidTx (newtime,inL (flattenTxLogs txlogs) >>= conflicts)

validateCatchTx :: (Typeable r,Typeable m,TxLayer Outside r m) => String -> Outside TxAdapton r m ()
validateCatchTx msg = do
	txenv@(timeref :!: callstack :!: txlogs@(SCons txlog _)) <- Reader.ask
	starttime <- inL $ readRef timeref
	mbsuccess <- inL $ validateTxs starttime txlogs
	case mbsuccess of
		Nothing -> do
			-- in case the computation raises an exception, discard all its visible (write) effects
			-- unbuffer all writes at the innermost log
			doBlock $ inL $ liftTxLogsWrites txlogs >> unbufferTopTxLog txlogs True
		Just (newtime,conflicts) -> 
			Catch.throwM $ InvalidTx (newtime,inL (flattenTxLogs txlogs) >>= conflicts)

-- validates a transaction and places it into the waiting queue for retrying
validateAndRetryTopTx :: (Typeable r,Typeable m,TxLayer Outside r m) => String -> Outside TxAdapton r m (Either Lock (InvalidTxRepair r m))
validateAndRetryTopTx msg = atomicTx ("validateAndRetryTopTx "++msg) $ \doEvals -> do
	txenv@(timeref :!: callstack :!: txlogs@(SCons txlog SNil)) <- Reader.ask
	starttime <- inL $ readRef timeref
	-- validates the current and enclosing txs up the tx tree
	mbsuccess <- inL $ validateTxs starttime txlogs
	case mbsuccess of
		Nothing -> do
			lck <- inL $ liftIO $ Lock.newAcquired -- sets the tx lock as acquired; the tx will be resumed when the lock is released
			inL $ commitTopTx False doEvals starttime txlog -- commit @Eval@ and @New@ computations
			inL $ retryTxLog lck txlog -- wait on changes to retry (only registers waits, does not actually wait)
			return $ Left lck
		Just (newtime,conflicts) -> 
			return $ Right (newtime,inL (flattenTxLogs txlogs) >>= conflicts)

-- validates a nested transaction and merges its log with its parent
-- note that retrying discards the tx's writes
validateAndRetryNestedTx :: (TxLayer Outside r m) => String -> Outside TxAdapton r m ()
validateAndRetryNestedTx msg = do
	txenv@(timeref :!: callstack :!: txlogs@(SCons txlog1 txlogs1)) <- Reader.ask
	starttime <- inL $ readRef timeref
	mbsuccess <- inL $ validateTxs starttime txlogs
	case mbsuccess of
		Nothing -> doBlock $ inL $ commitNestedTx False txlog1 txlogs1 -- does not perform @Write@s on @retry@
		Just (newtime,conflicts) ->
			-- discards writes and applies the conflicting changes to locally repair the current tx
			Catch.throwM $ InvalidTx (newtime,inL (flattenTxLogs txlogs) >>= conflicts)

-- like STM, our txs are:
-- same as transaction repair: within transactions, we use no locks; we use locks for commit
-- 1) disjoint-access parallel: non-overlapping writes done in parallel
-- 2) read-parallel: reads done in parallel
-- 3) eval-semi-parallel: evals done in parallel, with the updates of the latest eval being discarded; both evals succeed, but their commits are not parallel though (one commits first and the other discards its evaluated data, and commits only dependents)
-- runs transaction-specific code atomically in respect to a global state
atomicTx :: (TxLayer Outside r m) => String -> (Bool -> Outside TxAdapton r m a) -> Outside TxAdapton r m a
atomicTx msg m = do
	txlogs <- readTxLog
	lcks <- inL $ liftIO $ txLocks txlogs
	
	let (read_lcks,eval_lcks,write_lcks) = Map.foldrWithKey
		(\k (lck,st) (rs,es,ws) -> case writeLock st of
			1 -> (Map.insert k lck rs,es,ws)
			2 -> (rs,Map.insert k lck es,ws)
			3 -> (rs,es,Map.insert k lck ws)
		) (Map.empty,Map.empty,Map.empty) lcks
	
	let reads = Map.keysSet read_lcks
	let evals = Map.keysSet eval_lcks
	let writes = Map.keysSet write_lcks
	
	debugTx $ "waiting " ++ msg ++ " " -- ++ show reads ++ " " ++ show evals ++ " " ++ show writes
	withLocksTx read_lcks eval_lcks write_lcks $ \doEval -> do
		debugTx $ "locked " ++ show doEval ++ " " -- ++ msg ++ if doEval then show evals else "" ++ " " ++ show writes
		x <- m doEval
		debugTx $ "unlocked " ++ msg -- ++ if doEval then show evals else "" ++ " " ++ show writes
		return x

-- we don't need to acquire locks in sorted order because we acquire sets of locks atomically
withLocksTx :: (TxLayer l r m) => Map Unique TLock -> Map Unique TLock -> Map Unique TLock -> (Bool -> l TxAdapton r m a) -> l TxAdapton r m a
#ifndef CSHF
withLocksTx reads evals writes m = do
	
	let waitAndAcquire1 wcks = inL $ liftIO $ STM.atomically $ do
		-- wait on concurrently acquired read locks (to ensure that concurrent writes are seen by this tx's validation step)
		Foldable.mapM_ waitOrRetryTLock reads
		-- try to acquire eval locks
		doEval <- tryAcquireTLocks evals
		-- acquire write locks or retry
		when doEval $ Foldable.mapM_ acquireOrRetryTLock wcks
		return doEval
	let waitAndAcquire2 wcks = inL $ liftIO $ STM.atomically $ do
		-- wait on concurrently acquired read locks (to ensure that concurrent writes are seen by this tx's validation step)
		Foldable.mapM_ waitOrRetryTLock reads
		-- wait on eval locks
		Foldable.mapM_ waitOrRetryTLock evals
		-- acquire write locks or retry
		Foldable.mapM_ acquireOrRetryTLock wcks
	let waitAndAcquire wcks = do
		doEval <- waitAndAcquire1 wcks -- try to acquire eval locks
		unless doEval $ waitAndAcquire2 wcks -- otherwise only wait on them
		return doEval
	let release wcks doEval = inL $ liftIO $ STM.atomically $ do
		when doEval $ Foldable.mapM_ releaseTLock evals
		Foldable.mapM_ releaseTLock wcks
	
	liftA2 Catch.bracket waitAndAcquire release writes m
#endif
#ifdef CSHF
withLocksTx'' :: (TxLayer l r m) => Bool -> Map Unique TLock -> Map Unique TLock -> Map Unique TLock -> (Bool -> l TxAdapton r m a) -> l TxAdapton r m (Maybe a)
withLocksTx'' isFst reads evals writes m = doBlock $ do
	
	let waitAndAcquire1 = inL $ liftIO $ do
		-- wait on concurrently acquired read locks (to ensure that concurrent writes are seen by this tx's validation step)
		doRead <- tryWaitTLocks reads
		-- try to acquire eval locks
		doEvalWrite <- tryAcquireTLocks $ Map.toAscList $ evals `Map.union` writes
		return (doRead && doEvalWrite)
	let waitAndAcquire2 = inL $ liftIO $ do
		-- wait on concurrently acquired read locks (to ensure that concurrent writes are seen by this tx's validation step)
		doRead <- tryWaitTLocks reads
		-- wait on eval locks
		doEval <- tryWaitTLocks evals
		-- acquire write locks or retry
		doWrite <- tryAcquireTLocks $ Map.toAscList writes
		return (doRead && doEval && doWrite)
	
	let waitAndAcquire = if isFst
		then waitAndAcquire1 -- try to acquire eval locks
		else waitAndAcquire2 -- try to acquire eval locks
	let release = inL $ liftIO $ do
		when isFst $ releaseTLocks $ Map.toAscList evals
		releaseTLocks $ Map.toAscList writes
	
	ok <- waitAndAcquire
	if ok
		then do
			v <- m isFst
			release
			return $ Just v
		else return Nothing

withLocksTx = withLocksTx' True
withLocksTx' :: (TxLayer l r m) => Bool -> Map Unique TLock -> Map Unique TLock -> Map Unique TLock -> (Bool -> l TxAdapton r m a) -> l TxAdapton r m a
withLocksTx' isFst reads evals writes m = do
	mb <- withLocksTx'' isFst reads evals writes m
	case mb of
		Just v -> return v
		Nothing -> do
--			inL $ liftIO $ yield
			inL $ liftIO $ threadDelay 1000
			withLocksTx' False reads evals writes m
#endif

instance (MonadCatch m,MonadMask m,Typeable m,Typeable r,MonadRef r m,WeakRef r,MonadIO m) => Incremental TxAdapton r m where
	
	-- a monad reader is enough since the callstack and hashtables are mutable
	newtype Outside TxAdapton r m a = TxOuter { runTxOuter :: ReaderT (TxEnv r m) m a }
		deriving (Functor,Applicative,MonadLazy,Monad,MonadRef r,MonadReader (TxEnv r m),MonadThrow,MonadCatch,MonadMask) 
	newtype Inside TxAdapton r m a = TxInner { runTxInner :: ReaderT (TxEnv r m) m a }
		deriving (Functor,Applicative,MonadLazy,Monad,MonadRef r,MonadReader (TxEnv r m),MonadThrow,MonadCatch,MonadMask)
	
	showIncK x = return $! Seq.force $ show x
	
	world = TxOuter . runTxInner
	{-# INLINE world #-}
	unsafeWorld = TxInner . runTxOuter
	{-# INLINE unsafeWorld #-}

#ifdef TXREPAIR
	runIncremental = atomicallyTx True ""
#endif
#ifndef TXREPAIR
	runIncremental = atomicallyTx False ""
#endif
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
checkTxs :: (Typeable m,Typeable r,MonadIO m) => TxLogs r m -> (TxUnmemo r m,TxWrite) -> m (Maybe (TxRepair' r m))
checkTxs txlogs (unmemo,writtenIDs) = do
	ok <- Map.foldrWithKey (\uid st m2 -> checkTxWrite txlogs uid st >>= \mb1 -> m2 >>= \mb2 -> return $ joinTxRepairMb' mb1 mb2) (return Nothing) writtenIDs
	case ok of
		Nothing -> return Nothing
		Just repair -> return $ Just $ \txlog -> repair txlog >> inL (liftIO $ unmemo txlog)
  where
	checkTxWrite :: (Typeable m,Typeable r,MonadIO m) => TxLogs r m -> Unique -> Bool -> m (Maybe (TxRepair' r m))
	checkTxWrite txlogs uid True = do -- concurrent write
		mb <- liftIO $ findTxLogEntry txlogs uid
		case mb of
			Nothing -> return Nothing
			Just tvar -> do
				-- Write-XXX are always conflicts
				-- if there was a previous @Eval@ or @Write@ on a buffered variable we discard buffered content but keep buffered dependents
				debugTx2' $ "write conflict " ++ show uid
				return $ Just $ \txlog -> inL $ unbufferTxVar False (SCons txlog SNil) uid -- a conflict, unbuffer only later when tx is retried with dirtying
	checkTxWrite txlogs uid False = do -- concurrent dependent write
		mb <- liftIO $ findTxLogEntry txlogs uid
		case mb of
			Nothing -> return Nothing
			Just tvar -> do
				if isWriteOrNewTrue (dynTxStatus tvar)
					then do
						-- if a dependent was added concurrently while this tx dirtied a variable
						debugTx2' $ "write dependents conflict " ++ show uid
						return $ Just $ \txlog -> inL $ unbufferTxVar False (SCons txlog SNil) uid -- a conflict, unbuffer only later when tx is retried with dirtying
					else return Nothing

#ifdef CSHF
-- True = write, False = dependent write
checkVarTx :: (Typeable r,Typeable m,MonadIO m) => Bool -> Unique -> MVar (Map ThreadId Bool) -> m (ThreadRepair r m)
checkVarTx True uid xs = liftIO $ readMVar xs >>= Map.foldrWithKey checkThread (return Map.empty) where
	checkThread thread _ m = liftM (Map.insert thread (\txlog -> inL $ unbufferTxVar False (SCons txlog SNil) uid)) m
checkVarTx False uid xs = liftIO $ readMVar xs >>= Map.foldrWithKey checkThread (return Map.empty) where
	checkThread thread isWriteOrNewTrue m = do
		if isWriteOrNewTrue
			then liftM (Map.insert thread (\txlog -> inL $ unbufferTxVar False (SCons txlog SNil) uid)) m
			else m
#endif		

-- restarts a transaction with a configurable reset parameter
restartTxWithRepair (Just repair) msg starttime newtime m = do
#ifndef CSHF
	inL $ liftIO $ updateRunningTx starttime newtime
#endif
	restartTx newtime $ debugTx msg >> repair >> m
restartTxWithRepair Nothing msg starttime newtime m = do
#ifndef CSHF
	inL $ liftIO $ deleteRunningTx starttime
#endif
	resetTx $ debugTx msg >> m





