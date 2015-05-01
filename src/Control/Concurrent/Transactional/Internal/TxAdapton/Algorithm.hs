{-# LANGUAGE DoAndIfThenElse, DataKinds, StandaloneDeriving, ImpredicativeTypes, Rank2Types, TupleSections, GeneralizedNewtypeDeriving, TypeFamilies, DeriveDataTypeable, ScopedTypeVariables, UndecidableInstances, MultiParamTypeClasses, FlexibleInstances, MagicHash, ViewPatterns, BangPatterns, ConstraintKinds, FlexibleContexts #-}

module Control.Concurrent.Transactional.Internal.TxAdapton.Algorithm where

import Debug.Trace

import Control.Concurrent.Transactional.TMVar
import Control.Concurrent.Promise
import Control.Concurrent.Future
import Control.Concurrent hiding (readMVar,takeMVar,putMVar)
import Control.Monad.Incremental
import Data.Monoid
import Control.Concurrent.Transactional
import Control.Monad.Incremental.Adapton hiding (txAdaptonMemoSize)
import Data.Typeable
import qualified Control.Monad.Reader as Reader

import Data.Concurrent.Deque.Class as Queue
import Data.Concurrent.Deque.Reference.DequeInstance
import Data.Maybe
import Control.Concurrent.Transactional.Internal.TxAdapton.Memo
import Control.Applicative
import Control.Concurrent.Chan
import qualified Control.Concurrent.Map.Exts as CMap
import System.IO.Unsafe
import Control.DeepSeq as Seq
import System.Mem

import Control.Concurrent.Transactional.Internal.TxAdapton.Types
import Control.Concurrent.Transactional.Internal.TxAdapton.Layers

import qualified Control.Concurrent.STM as STM

import System.Mem.MemoTable (MemoTable(..))
import qualified System.Mem.MemoTable as MemoTable
import Data.Unique
import Data.IORef.Exts
import Control.Monad.IO.Class
import Data.Strict.Maybe as Strict
import Data.Strict.List as Strict
import Data.Strict.Tuple as Strict
import System.Mem.Weak.Exts as Weak
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map
import Data.Set (Set(..))
import qualified Data.Set as Set
import Control.Monad
import Data.Foldable as Foldable
import Data.List as List
import Control.Concurrent.MVar.Exts
import Control.Concurrent.Lock.Exts as Lock
import Data.Time.Clock
import Control.Exception
import Control.Monad.Catch (MonadCatch,MonadMask,MonadThrow)
import qualified Control.Monad.Catch as Catch
import System.Mem.WeakMap as WeakMap
import Safe
import Control.Monad.Trans
import Data.IORef
import Control.Monad.Reader (ReaderT(..),MonadReader(..))
import qualified Control.Monad.Reader

import Control.Monad.Catch (MonadCatch,MonadThrow)
import qualified Control.Monad.Catch as Catch

import Debug

-- ** strict inputs @M@

instance (IsolationKind i,TxLayer Outside c,TxLayer Inside c) => Thunk (TxM i c) Inside (TxAdapton c) where
	new = modInnerTxM
	{-# INLINE new #-}
	newc = refInnerTxM
	{-# INLINE newc #-}
	read = getInnerTxM
	{-# INLINE read #-}

instance (IsolationKind i,TxLayer Inside c,TxLayer Outside c) => Thunk (TxM i c) Outside (TxAdapton c) where
	new = modOuterTxM
	{-# INLINE new #-}
	newc = refOuterTxM
	{-# INLINE newc #-}
	read = getOuterTxM
	{-# INLINE read #-}

instance (IsolationKind i,TxLayer Outside c,TxLayer Inside c) => Input (TxM i c) Inside (TxAdapton c) where
	ref = refInnerTxM
	{-# INLINE ref #-}
	get = getInnerTxM
	{-# INLINE get #-}
	set = setInnerTxM
	{-# INLINE set #-}
	getOutside = getOuterTxM
	{-# INLINE getOutside #-}
	refOutside = refOuterTxM
	{-# INLINE refOutside #-}
	modOutside = \c -> outside c >>= refOutside
	{-# INLINE modOutside #-}

instance (IsolationKind i,TxLayer Inside c,TxLayer Outside c) => Input (TxM i c) Outside (TxAdapton c) where
	ref = refOuterTxM
	{-# INLINE ref #-}
	get = getOuterTxM
	{-# INLINE get #-}
	set = setOuterTxM
	{-# INLINE set #-}
	refOutside = refOuterTxM
	{-# INLINE refOutside #-}
	modOutside = \c -> c >>= refOutside
	{-# INLINE modOutside #-}

instance (TxLayer Outside c,TxLayer Inside c) => CumulativeInput (TxM Cumulative c) Inside (TxAdapton c) where
	cumulative = cumulativeInnerTxM	
instance (TxLayer Outside c,TxLayer Inside c) => CumulativeInput (TxM Cumulative c) Outside (TxAdapton c) where
	cumulative = cumulativeOuterTxM

cumulativeInnerTxM :: (TxLayer Outside c,IncK (TxAdapton c) a,TxLayer Inside c) => Resolve (TxAdapton c) a -> Inside (TxAdapton c) a -> Inside (TxAdapton c) (TxM Cumulative c Inside (TxAdapton c) a)
cumulativeInnerTxM resolve m = m >>= refInnerTxMWith resolve
cumulativeOuterTxM :: (TxLayer Inside c,IncK (TxAdapton c) a,TxLayer Outside c) => Resolve (TxAdapton c) a -> Outside (TxAdapton c) a -> Outside (TxAdapton c) (TxM Cumulative c Outside (TxAdapton c) a)
cumulativeOuterTxM resolve m = m >>= refOuterTxMWith resolve

modInnerTxM :: (IsolationKind i,TxLayer Outside c,IncK (TxAdapton c) a,TxLayer Inside c) => Inside (TxAdapton c) a -> Inside (TxAdapton c) (TxM i c Inside (TxAdapton c) a)
modInnerTxM m = m >>= refInnerTxM
modOuterTxM :: (IsolationKind i,IncK (TxAdapton c) a,TxLayer Inside c,TxLayer Outside c) => Outside (TxAdapton c) a -> Outside (TxAdapton c) (TxM i c Outside (TxAdapton c) a)
modOuterTxM m = m >>= refOuterTxM

refOuterTxM :: (IsolationKind i,IncK (TxAdapton c) a,TxLayer l c,TxLayer Outside c,TxLayer Inside c) => a -> Outside (TxAdapton c) (TxM i c l (TxAdapton c) a)
refOuterTxM = refOuterTxM' Proxy Proxy
refOuterTxM' :: (IsolationKind i,IncK (TxAdapton c) a,TxLayer l c,TxLayer Outside c,TxLayer Inside c) => Proxy i -> Proxy (TxAdapton c) -> a -> Outside (TxAdapton c) (TxM i c l (TxAdapton c) a)
refOuterTxM' i inc (v::a) = refOuterTxMWith (defaultTxResolve i inc (Proxy :: Proxy a)) v

refOuterTxMWith :: (IsolationKind i,IncK (TxAdapton c) a,TxLayer l c,TxLayer Inside c,TxLayer Outside c) => TxResolve i (TxAdapton c) a -> a -> Outside (TxAdapton c) (TxM i c l (TxAdapton c) a)
refOuterTxMWith (resolve :: TxResolve i (TxAdapton c) a) v = doBlockTx $ do
	let proxy = Proxy :: Proxy c
	m <- unsafeIOToInc $! do
		idU <- newUnique
		dta <- newIORef' v
		dependentsU <- WeakMap.new
		-- since the ref will never be reused, we don't need to worry about it's creator
		waitQ <- newQ
		lck <- Lock.new
		notifies <- newTxNotifies proxy
		content <- CMap.empty
		unmemo <- newMVar $ error "no unmemo"
		let meta = TxNodeMeta
			(   idU
			:!: dependentsU
			:!: (\isFuture rootThread thread txlog -> return ())
			:!: (\isFuture rootThread thread txlog -> return ())
			:!: (\isFuture rootThread thread txst txlogs -> bufferTxM isFuture rootThread thread m txst txlogs)
			:!: Nothing
			:!: waitQ
			:!: lck
			:!: notifies
			:!: content
			:!: unmemo :!: drawTxM m)
		    m = TxM (dta :!: meta :!: resolve) :: TxM i c l (TxAdapton c) a
		return m
	newTxMLog m
	return $! m

refInnerTxM :: (IsolationKind i,IncK (TxAdapton c) a,TxLayer l c,TxLayer Outside c,TxLayer Inside c) => a -> Inside (TxAdapton c) (TxM i c l (TxAdapton c) a)
refInnerTxM = refInnerTxM' Proxy Proxy
refInnerTxM' :: (IsolationKind i,IncK (TxAdapton c) a,TxLayer l c,TxLayer Outside c,TxLayer Inside c) => Proxy i -> Proxy (TxAdapton c) -> a -> Inside (TxAdapton c) (TxM i c l (TxAdapton c) a)
refInnerTxM' i inc (v::a) = refInnerTxMWith (defaultTxResolve i inc (Proxy :: Proxy a)) v

refInnerTxMWith :: (IsolationKind i,TxLayer Outside c,IncK (TxAdapton c) a,TxLayer l c,TxLayer Inside c) => TxResolve i (TxAdapton c) a -> a -> Inside (TxAdapton c) (TxM i c l (TxAdapton c) a)
refInnerTxMWith (resolve :: TxResolve i (TxAdapton c) a) v = doBlockTx $ do
	let proxy = Proxy :: Proxy c
	idU <- unsafeIOToInc newUnique
	creator <- mkRefCreatorTx idU
	m <- unsafeIOToInc $! do
		dta <- newIORef' v
		dependentsU <- WeakMap.new
		-- add a reference dependency (they are transitive up to the top-level calling thunk)
		waitQ <- newQ
		lck <- Lock.new
		notifies <- newTxNotifies proxy
		content <- CMap.empty
		unmemo <- newMVar $ error "no unmemo"
		let meta = TxNodeMeta
			(   idU
			:!: dependentsU
			:!: (\isFuture rootThread thread txlog -> return ())
			:!: (\isFuture rootThread thread txlog -> return ())
			:!: (\isFuture rootThread thread txst txlogs -> bufferTxM isFuture rootThread thread m txst txlogs)
			:!: creator
			:!: waitQ
			:!: lck
			:!: notifies
			:!: content
			:!: unmemo :!: drawTxM m)
		    m = TxM (dta :!: meta :!: resolve) :: TxM i c l (TxAdapton c) a
		return m
	newTxMLog m
	return $! m

{-# INLINE getInnerTxM #-}
getInnerTxM :: (IsolationKind i,TxLayer Outside c,IncK (TxAdapton c) a,TxLayer Inside c) => TxM i c Inside (TxAdapton c) a -> Inside (TxAdapton c) a
getInnerTxM = \t -> {-# SCC getInnerTxM #-} doBlockTx $ do
	(!value,!status) <- readTxMValue t -- read from the buffer
	addTxDependency (metaTxM t) (checkTxM t $! value) status -- updates dependencies of callers
--	str <- displayK value
--	debugTx2 $ "getInnerTxM " ++ show (idTxNM $ metaTxM t) ++ " " ++ str
	return value

{-# INLINE getOuterTxM #-}	
getOuterTxM :: (IsolationKind i,IncK (TxAdapton c) a,TxLayer l c,TxLayer Outside c) => TxM i c l (TxAdapton c) a -> Outside (TxAdapton c) a
getOuterTxM = \t -> {-# SCC getOuterTxM #-} liftM Prelude.fst $ readTxMValue t

{-# INLINE checkTxM #-}
checkTxM :: (IsolationKind i,TxLayer Outside c,IncK (TxAdapton c) a,TxLayer Inside c) => TxM i c Inside (TxAdapton c) a -> a -> Inside (TxAdapton c) (Bool,TxStatus)
checkTxM t oldv = do
	(!value,!status) <- readTxMValue t
	let !ok = oldv == value
	return $! (ok,status)

setInnerTxM :: (IsolationKind i,IncK (TxAdapton c) a,TxLayer Outside c,TxLayer Inside c) => TxM i c Inside (TxAdapton c) a -> a -> Outside (TxAdapton c) ()
setInnerTxM t v' = doBlockTx $ do
	(!v,_) <- readTxMValue t
	unless (v == v') $ do
		writeTxMValue t v'
		dirtyTx (metaTxM t)

setOuterTxM :: (IsolationKind i,IncK (TxAdapton c) a,TxLayer Outside c) => TxM i c Outside (TxAdapton c) a -> a -> Outside (TxAdapton c) ()
setOuterTxM t v' = doBlockTx $ do
	(!v,_) <- readTxMValue t
	writeTxMValue t v'
	return ()
		
-- ** lazy inputs @L@


-- ** lazy outputs @U@

instance (TxLayer Inside c,TxLayer Outside c) => Thunk (TxU c) Inside (TxAdapton c) where
	new = thunkTxU
	{-# INLINE new #-}
	newc = constTxU
	{-# INLINE newc #-}
	read = forceInnerTxU
	{-# INLINE read #-}

instance (TxLayer Outside c) => Thunk (TxU c) Outside (TxAdapton c) where
	new = thunkTxU
	{-# INLINE new #-}
	newc = constTxU
	{-# INLINE newc #-}
	read = forceOuterTxU
	{-# INLINE read #-}

-- no memoization at the outer layer
instance (TxLayer Outside c) => Output (TxU c) Outside (TxAdapton c) where
	thunk = thunkTxU
	{-# INLINE thunk #-}
	const = constTxU
	{-# INLINE const #-}
	force = forceOuterTxU
	{-# INLINE force #-}
	forceOutside = forceOuterTxU
	{-# INLINE forceOutside #-}

instance (TxLayer Outside c,TxLayer Inside c) => Output (TxU c) Inside (TxAdapton c) where
	thunk = thunkTxU
	{-# INLINE thunk #-}
	const = constTxU
	{-# INLINE const #-}
	force = forceInnerTxU
	{-# INLINE force #-}
	forceOutside = world . liftM Prelude.fst . doBlockTx . forceNoDependentsTxU
	{-# INLINE forceOutside #-}
	memo = memoTxU
	{-# INLINE memo #-}
	memoAs = memoTxUAs
	{-# INLINE memoAs #-}
--	gmemoQ = gmemoQTxU
--	{-# INLINE gmemoQ #-}

memoTxU :: (TxLayer Outside c,IncK (TxAdapton c) a,TxLayer Inside c,Memo arg) => ((arg -> Inside (TxAdapton c) (TxU c Inside (TxAdapton c) a)) -> arg -> Inside (TxAdapton c) a) -> (arg -> Inside (TxAdapton c) (TxU c Inside (TxAdapton c) a))
memoTxU f arg = do
	params <- readTxParams
	let memo_func = memoNonRecTxU (txAdaptonMemoSize params) (txAdaptonMemoPolicy params) (thunkTxU . f memo_func)
	memo_func arg

memoTxUAs :: (TxLayer Outside c,Memo name,IncK (TxAdapton c) a,TxLayer Inside c,Memo arg) => name -> ((arg -> Inside (TxAdapton c) (TxU c Inside (TxAdapton c) a)) -> arg -> Inside (TxAdapton c) a) -> (arg -> Inside (TxAdapton c) (TxU c Inside (TxAdapton c) a))
memoTxUAs name f arg = do
	params <- readTxParams
	let memo_func = memoNonRecTxUAs (txAdaptonMemoSize params) (txAdaptonMemoPolicy params) name (thunkTxU . f memo_func)
	memo_func arg

thunkTxU :: (TxLayer Outside c,IncK (TxAdapton c) a,TxLayer l c,TxLayer l1 c) => l1 (TxAdapton c) a -> l (TxAdapton c) (TxU c l1 (TxAdapton c) a)
thunkTxU = thunkTxU' Proxy
	where
	{-# INLINE thunkTxU' #-}
	thunkTxU' :: (TxLayer Outside c,IncK (TxAdapton c) a,TxLayer l c,TxLayer l1 c) => Proxy c -> l1 (TxAdapton c) a -> l (TxAdapton c) (TxU c l1 (TxAdapton c) a)
	thunkTxU' proxy c = doBlockTx $ do
		u <- unsafeIOToInc $! do
			idU <- newUnique
			dta <- newIORef' (TxThunk c)
			dependentsU <- WeakMap.new
			waitQ <- newQ
			lck <- Lock.new
			notifies <- newTxNotifies proxy
			content <- CMap.empty
			unmemo <- newMVar (return () :!: Map.empty)
			let meta = TxNodeMeta
				(   idU
				:!: dependentsU
				:!: (\isFuture rootThread thread txlog -> changeDirtyValueTx isFuture rootThread thread True (TxStatus (Write Nothing :!: False)) u txlog >> return ())
				:!: (\isFuture rootThread thread txlog -> forgetTxU isFuture rootThread thread u txlog >> return ())
				:!: (\isFuture rootThread thread txst txlogs -> bufferTxU isFuture rootThread thread u txst txlogs)
				:!: Nothing
				:!: waitQ
				:!: lck
				:!: notifies
				:!: content
				:!: unmemo :!: drawTxU u)
			    u = TxU (dta :!: meta)
			return u
		newTxULog u
		return $! u

constTxU :: (TxLayer Outside c,IncK (TxAdapton c) a,TxLayer l c,TxLayer l1 c) => a -> l (TxAdapton c) (TxU c l1 (TxAdapton c) a)
constTxU = constTxU' Proxy
	where
	{-# INLINE constTxU' #-}
	constTxU' :: (TxLayer Outside c,IncK (TxAdapton c) a,TxLayer l c,TxLayer l1 c) => Proxy c -> a -> l (TxAdapton c) (TxU c l1 (TxAdapton c) a)
	constTxU' proxy v = doBlockTx $ do
		u <- unsafeIOToInc $! do
			idU <- newUnique
			dta <- newIORef' (TxConst v)
			dependentsU <- WeakMap.new
			waitQ <- newQ
			lck <- Lock.new
			notifies <- newTxNotifies proxy
			content <- CMap.empty
			unmemo <- newMVar (return () :!: Map.empty)
			let meta = TxNodeMeta
				(   idU
				:!: dependentsU
				:!: (\isFuture rootThread thread txlog -> return ())
				:!: (\isFuture rootThread thread txlog -> forgetTxU isFuture rootThread thread u txlog >> return ())
				:!: (\isFuture rootThread thread txst txlogs -> bufferTxU isFuture rootThread thread u txst txlogs)
				:!: Nothing
				:!: waitQ
				:!: lck
				:!: notifies
				:!: content
				:!: unmemo :!: drawTxU u)
			    u = TxU (dta :!: meta)
			return u
		newTxULog u
		return $! u

drawTxM :: (IsolationKind i,TxLayer Inside c,TxLayer Outside c,IncK (TxAdapton c) a,TxLayer l c) => TxM i c l (TxAdapton c) a -> Outside (TxAdapton c) DrawDot
drawTxM (m :: TxM i c l (TxAdapton c) a) = do
	let i = Proxy :: Proxy i
	let l = Proxy :: Proxy l
	case (toIsolation i,toLayerKind l) of
		(Versioned,Inside) -> let m1 :: TxM Versioned c Inside (TxAdapton c) a = coerce m in draw Proxy m1
		(Forgetful,Inside) -> let m1 :: TxM Forgetful c Inside (TxAdapton c) a = coerce m in draw Proxy m1
		(Cumulative,Inside) -> let m1 :: TxM Cumulative c Inside (TxAdapton c) a = coerce m in draw Proxy m1
		(Versioned,Outside) -> let m1 :: TxM Versioned c Outside (TxAdapton c) a = coerce m in draw Proxy m1
		(Forgetful,Outside) -> let m1 :: TxM Forgetful c Outside (TxAdapton c) a = coerce m in draw Proxy m1
		(Cumulative,Outside) -> let m1 :: TxM Cumulative c Outside (TxAdapton c) a = coerce m in draw Proxy m1

drawTxU :: (TxLayer Outside c,IncK (TxAdapton c) a,TxLayer l c) => TxU c l (TxAdapton c) a -> Outside (TxAdapton c) DrawDot
drawTxU (u :: TxU c l (TxAdapton c) a) = do
	let l = Proxy :: Proxy l
	case toLayerKind l of
		Inside -> let u1 :: TxU c Inside (TxAdapton c) a = coerce u in draw Proxy u1
		Outside -> let u1 :: TxU c Outside (TxAdapton c) a = coerce u in draw Proxy u1

{-# INLINE forceOuterTxU #-}
forceOuterTxU :: (IncK (TxAdapton c) a,TxLayer Outside c) => TxU c Outside (TxAdapton c) a -> Outside (TxAdapton c) a
forceOuterTxU = \t -> doBlockTx $ do
	!(d,status') <- readTxUValue t $! TxStatus (Read Nothing :!: False)
	v <- case d of
		TxThunk force -> force
		TxConst value -> return value
		otherwise -> error "forceOuterTxU stores no IC data"
	return $! v

{-# INLINE forceInnerTxU #-}
forceInnerTxU :: (IncK (TxAdapton c) a,TxLayer Inside c,TxLayer Outside c) => TxU c Inside (TxAdapton c) a -> Inside (TxAdapton c) a
forceInnerTxU = \t -> {-# SCC forceInnerTxU #-} doBlockTx $ do
	!(value,status) <- forceNoDependentsTxU t
	addTxDependency (metaTxU t) (checkTxU t $! value) status
	return $! value

hasDependenciesTxU :: (TxLayer Outside c,IncK (TxAdapton c) a,TxLayer Inside c) => TxU c Inside (TxAdapton c) a -> Inside (TxAdapton c) Bool
hasDependenciesTxU t = doBlockTx $ do
	(!d,_) <- readTxUValue t $! TxStatus (Read Nothing :!: False)
	case d of
		TxValue _ value force dependencies -> liftM (not . List.null) $ unsafeIOToInc $ readIORef' dependencies
		TxThunk force -> error "cannot test dependencies of unevaluated thunk"
		TxConst value -> return False

-- in case we repair the thunks, we need to make sure that the cached value/dependencies match
{-# INLINE forceNoDependentsTxU #-}
forceNoDependentsTxU :: (IncK (TxAdapton c) a,TxLayer Inside c,TxLayer Outside c) => TxU c Inside (TxAdapton c) a -> Inside (TxAdapton c) (a,TxStatus)
forceNoDependentsTxU = \t -> forceNoDependentsTxU' (Read Nothing) t
  where
	{-# INLINE forceNoDependentsTxU' #-}
	forceNoDependentsTxU' :: (IncK (TxAdapton c) a,TxLayer Inside c,TxLayer Outside c) => TxStatusVar -> TxU c Inside (TxAdapton c) a -> Inside (TxAdapton c) (a,TxStatus)
	forceNoDependentsTxU' status t = do
		(!d,!status') <- readTxUValue t $! TxStatus (status :!: False)
		!r <- case d of
			TxValue 0# value force dependencies -> return $! (value,status')
			TxValue 1# value force dependencies -> if (status==Eval Nothing)
				then repairInnerTxU t value force dependencies
				else forceNoDependentsTxU' (Eval Nothing) t
			TxThunk force -> unsafeIOToInc (newIORef' []) >>= \deps -> evaluateInnerTxU t force deps status'
			TxConst value -> return $! (value,status')
		return $! r

-- in case we repair the thunks, we need to make sure that the cached value/dependencies match
checkTxU :: (IncK (TxAdapton c) a,TxLayer Inside c,TxLayer Outside c) => TxU c Inside (TxAdapton c) a -> a -> Inside (TxAdapton c) (Bool,TxStatus)
checkTxU t v = {-# SCC checkTxU #-} checkTxU' (Read Nothing) t v where
	{-# INLINE checkTxU' #-}
	checkTxU' :: (IncK (TxAdapton c) a,TxLayer Inside c,TxLayer Outside c) => TxStatusVar -> TxU c Inside (TxAdapton c) a -> a -> Inside (TxAdapton c) (Bool,TxStatus)
	checkTxU' status t oldv = do
		(!d,!status') <- readTxUValue t $! TxStatus (status :!: False)
		!r <- case d of
			TxValue 0# value force dependencies -> return (oldv==value,status') -- since the variable may have been dirtied and re-evaluated since the last time we looked at it
			TxValue 1# value force dependencies -> if (status== Eval Nothing)
				then do
					!(v,status'') <- repairInnerTxU t value force dependencies
					let !ok = oldv == v
					return $! (ok,status'') 
				else checkTxU' (Eval Nothing) t oldv
			TxThunk _ -> return $! (False,status')
			TxConst value -> return $! (False,status')
		return $! r

repairInnerTxU :: (TxLayer Outside c,IncK (TxAdapton c) a,TxLayer Inside c) => TxU c Inside (TxAdapton c) a -> a -> Inside (TxAdapton c) a -> TxDependencies c -> Inside (TxAdapton c) (a,TxStatus)
repairInnerTxU t value force txdependencies = do
		!txid <- readTxIdRef
		(isFuture,rootThread) <- readRootTx
		!thread <- unsafeIOToInc myThreadId
		debugTx2 $ "repairing thunk "++ show (hashUnique $ idTxNM $ metaTxU t)
		!tbl <- readTxLogs
		(v,status') <- unsafeIOToInc (readIORef' txdependencies) >>= List.foldr (repair' isFuture rootThread thread t force tbl txdependencies) (norepair' isFuture rootThread thread t value tbl) . List.reverse --we need to reverse the dependency list to respect evaluation order
--		str <- displayK v
--		debugTx2 $ "repaired thunk "++ show (hashUnique $ idTxNM $ metaTxU t) ++ " " ++ str
		return $! (v,status')
	where
	-- finishes by dirtying the node
	{-# INLINE norepair' #-}
	norepair' :: (TxLayer Outside c,IncK (TxAdapton c) a,TxLayer Inside c) => Bool -> ThreadId -> ThreadId -> TxU c Inside (TxAdapton c) a -> a -> TxLogs c -> Inside (TxAdapton c) (a,TxStatus)
	norepair' isFuture rootThread thread t value tbl = do
		liftM (value,) $ unsafeIOToInc $! changeDirtyValueTx isFuture rootThread thread False (TxStatus (Eval Nothing :!: False)) t tbl
	
	-- repairs a dependency
	{-# INLINE repair' #-}
	repair' :: (TxLayer Outside c,IncK (TxAdapton c) a,TxLayer Inside c) => Bool -> ThreadId -> ThreadId -> TxU c Inside (TxAdapton c) a -> Inside (TxAdapton c) a -> TxLogs c -> TxDependencies c -> (TxDependency c,Weak (TxDependency c)) -> Inside (TxAdapton c) (a,TxStatus) -> Inside (TxAdapton c) (a,TxStatus)
	repair' isFuture rootThread thread t force tbl txdependencies (d,w) m = do
		isDirty <- unsafeIOToInc $ readIORef' (dirtyTxW d)
		if isDirty
			then do
				txdependents <- unsafeIOToInc $ getBufferedTxDependents isFuture rootThread thread tbl (srcMetaTxW d) (TxStatus (Read Nothing :!: True)) -- we only modify the dependents
				unsafeIOToInc $ changeTxDependency txdependencies txdependents d False
				!(ok,src_status) <- checkTxW d
				if ok
					then liftM (\(v,s) -> (v,mappend src_status s)) m
					else unsafeIOToInc (newIORef' []) >>= \deps -> evaluateInnerTxU t force deps src_status
			else m

{-# INLINE evaluateInnerTxU #-}
evaluateInnerTxU :: (IncK (TxAdapton c) a,TxLayer Inside c,TxLayer Outside c) => TxU c Inside (TxAdapton c) a -> Inside (TxAdapton c) a -> TxDependencies c -> TxStatus -> Inside (TxAdapton c) (a,TxStatus)
evaluateInnerTxU t force txdependencies status = {-# SCC evaluateInnerTxU #-} do
	txstatus <- unsafeIOToInc $ newIORef' (status `mappend` (TxStatus (Eval Nothing :!: False)))
	pushTxStack (metaTxU t :!: SJust txdependencies :!: txstatus)
	value <- force
	-- update the status of the thunk with the children statuses (a thunk with written dependents is written)
	inner_status <- unsafeIOToInc $ readIORef' txstatus 
	let newstatus = changeStatus inner_status
	-- write the value
	status' <- writeTxUValue t (TxValue 0# value force txdependencies) newstatus -- forgets the original dependencies
	popTxStack
	return $! (value,status')

isDirtyUnevaluatedTxU :: (IncK (TxAdapton c) a,TxLayer l1 c,TxLayer l c,TxLayer Outside c) => TxU c l1 (TxAdapton c) a -> l (TxAdapton c) (Maybe Bool)
isDirtyUnevaluatedTxU t = doBlockTx $ do
	(d,_) <- readTxUValue t $! (TxStatus (Read Nothing :!: False))
	case d of
		TxThunk force -> return Nothing --unevaluated thunk
		TxConst value -> return $ Just False -- constant value
		TxValue 1# value force dependencies -> return $ Just True -- dirty
		TxValue 0# value force dependencies -> return $ Just False -- not dirty

isUnevaluatedTxU :: (IncK (TxAdapton c) a,TxLayer l1 c,TxLayer l c,TxLayer Outside c) => TxU c l1 (TxAdapton c) a -> l (TxAdapton c) Bool
isUnevaluatedTxU t = doBlockTx $ do
	(d,_) <- readTxUValue t $! (TxStatus (Read Nothing :!: False))
	case d of
		TxThunk force -> return True --unevaluated thunk
		otherwise -> return False

oldvalueTxU :: (IncK (TxAdapton c) a,TxLayer l c,TxLayer l1 c,TxLayer Outside c) => TxU c l1 (TxAdapton c) a -> l (TxAdapton c) a
oldvalueTxU t = doBlockTx $ do
	(d,_) <- readTxUValue t $! (TxStatus (Read Nothing :!: False))
	case d of
		TxValue dirty value force dependencies -> return value
		TxThunk force -> error "no old value available"
		TxConst value -> return value

-- ** auxiliary functions
	
-- makes the new node an eval or a write
changeDirtyValueTx :: (IncK (TxAdapton c) a,TxLayer l c,TxLayer Outside c) => Bool -> ThreadId -> ThreadId -> Bool -> TxStatus -> TxU c l (TxAdapton c) a -> TxLogs c -> IO TxStatus
changeDirtyValueTx isFuture rootThread thread dirty newstatus u txlog = do
	tvar <- changeTxU isFuture rootThread thread u (Just chgDirty) newstatus txlog
	readIORef' $ dynTxStatus tvar
  where
	chgDirty (TxValue _ value force dependencies , ori) = return $! (TxValue (if dirty then 1# else 0#) value force dependencies , ori)
	
forgetTxU :: (IncK (TxAdapton c) a,TxLayer l c,TxLayer Outside c) => Bool -> ThreadId -> ThreadId -> TxU c l (TxAdapton c) a -> TxLogs c -> IO TxStatus
forgetTxU isFuture rootThread thread u txlog = do
	tvar <- changeTxU isFuture rootThread thread u (Just forget) (TxStatus (Write Nothing :!: False)) txlog
	readIORef' $ dynTxStatus tvar
  where
	forget (TxValue _ _ force dependencies , ori) = do
		(unmemo_ori :!: unmemo_buffs) <- readMVar "forgetTxU" (unmemoTxNM $ metaTxU u)
		unmemo_ori >> Foldable.mapM_ id unmemo_buffs
		clearTxDependencies False dependencies
		ori' <- case ori of
			Left deps -> liftM Right $ mkWeakRefKey deps deps Nothing
			Right wdeps -> return $ Right wdeps
		return (TxThunk force , ori') -- forget original dependencies
	forget dta = return dta

{-# INLINE mkRefCreatorTx #-}
mkRefCreatorTx :: (TxLayer l c) => Unique -> l (TxAdapton c) (Maybe (TxCreator c))
mkRefCreatorTx = \idU -> do
	top <- topTxStack
	case top of
		Just (callermeta :!: SJust txcallerdependencies :!: _) -> do
			-- its ok to point to the buffered transaction dependencies reference, because the creator never changes
			weak <- unsafeIOToInc $ mkWeakRefKey txcallerdependencies callermeta Nothing
			return $ Just weak
		otherwise -> return Nothing

-- multiple dependencies on the same source node are combined into one
{-# INLINE addTxDependency #-}
addTxDependency :: (TxLayer Inside c) => TxNodeMeta c -> Inside (TxAdapton c) (Bool,TxStatus) -> TxStatus -> Inside (TxAdapton c) ()
addTxDependency !calleemeta !check !calleestatus = {-# SCC addTxDependency #-} do
	!txid <- readTxIdRef
	!top <- topThunkTxStack
	(isFuture,rootThread) <- readRootTx
	thread <- unsafeIOToInc myThreadId
	case top of
		Just (callermeta :!: SJust txcallerdependencies :!: callerstatus) -> do
			let !callerid = idTxNM callermeta
			!tbl <- readTxLogs
			unsafeIOToInc $! do
				!dirtyW <- newIORef' False 
				!originalW <- newIORef' False -- dependencies are created within a transaction
				let !dependencyW = TxDependency (calleemeta :!: dirtyW :!: check :!: callermeta :!: originalW :!: MkWeak (mkWeakRefKey $! txcallerdependencies))
				!txcalleedependents <- getBufferedTxDependents isFuture rootThread thread tbl calleemeta $! TxStatus (Read Nothing :!: True) -- only adding dependency
				let !purge = WeakMap.deleteFinalized (dependentsTxNM calleemeta) callerid >> WeakMap.deleteFinalized txcalleedependents callerid
				!weak <- mkWeakRefKey txcallerdependencies dependencyW $! Just purge
				modifyIORef' txcallerdependencies ((dependencyW,weak):) 
				WeakMap.insertWeak txcalleedependents (idTxNM callermeta) weak
				atomicModifyIORef' callerstatus (\x -> (mappend calleestatus x,())) -- join the status of the callee with the calller thunk
		otherwise -> return ()

{-# INLINE changeTxDependency #-}
changeTxDependency :: TxDependencies c -> TxDependents c -> TxDependency c -> Bool -> IO ()
changeTxDependency !txdependencies !txdependents !(TxDependency (srcMetaW :!: dirtyW :!: checkW :!: tgtMetaW :!: originalW :!: _)) !isDirty = {-# SCC changeTxDependency #-} do
	isOriginal <- readIORef' originalW
	if isOriginal
		then do -- when the dependency is not buffered, make a buffered non-dirty copy
			let !tgtid = idTxNM tgtMetaW
			!dirtyW' <- newIORef' isDirty
			!originalW' <- newIORef' False -- dependencies are created within a transaction
			let !dependencyW' = TxDependency (srcMetaW :!: dirtyW' :!: checkW :!: tgtMetaW :!: originalW' :!: MkWeak (mkWeakRefKey $! txdependencies))
			let !purge = WeakMap.deleteFinalized (dependentsTxNM srcMetaW) tgtid >> WeakMap.deleteFinalized txdependents tgtid
			!weak' <- mkWeakRefKey txdependencies dependencyW' (Just purge)
			updateTxDependency (idTxNM srcMetaW) (dependencyW',weak') txdependencies -- overrides the original dependency
			WeakMap.insertWeak txdependents (idTxNM tgtMetaW) weak' 
		else do
			writeIORef' dirtyW isDirty

{-# INLINE dirtyTx #-}
dirtyTx :: (TxLayer l c) => TxNodeMeta c -> l (TxAdapton c) ()
dirtyTx !umeta = {-# SCC dirtyTx #-} do
	!tbl <- readTxLogs
	!txid <- readTxIdRef
	(isFuture,rootThread) <- readRootTx
	thread <- unsafeIOToInc myThreadId
	unsafeIOToInc $ dirtyCreatorTx isFuture rootThread thread tbl (creatorTxNM umeta)
	unsafeIOToInc $ dirtyRecursivelyTx isFuture rootThread thread tbl umeta

dirtyTxWeak :: (TxLayer l c) => Weak (TxNodeMeta c) -> l (TxAdapton c) ()
dirtyTxWeak !wmeta = do
	mb <- unsafeIOToInc $ Weak.deRefWeak wmeta
	case mb of
		Nothing -> return ()
		Just meta -> dirtyTx meta

dirtyCreatorTx :: Bool -> ThreadId -> ThreadId -> TxLogs c -> Maybe (TxCreator c) -> IO ()
dirtyCreatorTx !isFuture rootThread thread tbl Nothing = return ()
dirtyCreatorTx !isFuture rootThread thread tbl (Just wcreator) = do
	mb <- deRefWeak wcreator
	case mb of
		Just creatorMeta -> do
			forgetTxNM creatorMeta isFuture rootThread thread tbl
			dirtyRecursivelyTx isFuture rootThread thread tbl creatorMeta
		Nothing -> return ()

dirtyRecursivelyTx :: Bool -> ThreadId -> ThreadId -> TxLogs c -> TxNodeMeta c -> IO ()
dirtyRecursivelyTx isFuture rootThread thread tbl !meta = do
	-- we need to get ALL the dependents (original + buffered) and dirty them
	!(txdependents,dependents) <- getTxDependents isFuture rootThread thread tbl meta $! TxStatus (Write Nothing :!: True) -- marks the buffered dependents as a write, we will also dirty its dependencies
	Foldable.mapM_ (dirtyTx' txdependents) dependents
  where
	{-# INLINE dirtyTx' #-}
	dirtyTx' txdependents = \d -> do
		isDirty <- readIORef' (dirtyTxW d)
		unless isDirty $ do -- stop if the dependency is already dirty
			mb_txdependencies <- getTxDependencies isFuture rootThread thread tbl (tgtMetaTxW d) $! TxStatus (Write Nothing :!: False) -- marks the buffered dependencies as a write, since dirtying results from a change
			case mb_txdependencies of
				Nothing -> return ()
				Just txdependencies -> do
					changeTxDependency txdependencies txdependents d True
					dirtyTxNM (tgtMetaTxW d) isFuture rootThread thread tbl -- dirty the thunk itself
					dirtyRecursivelyTx isFuture rootThread thread tbl (tgtMetaTxW d)

{-# INLINE updateTxDependency #-}	
updateTxDependency :: Unique -> (TxDependency c,Weak (TxDependency c)) -> TxDependencies c -> IO ()
updateTxDependency !did !d !rds = modifyIORef' rds updateTxDependency' where
	{-# INLINE updateTxDependency' #-}
	updateTxDependency' [] = [] -- if the dependency is not found, we don't change it
	updateTxDependency' (x:xs) = if did == xid then d : xs else x : updateTxDependency' xs
		where xid = idTxNM $ srcMetaTxW $ Prelude.fst x

{-# INLINE insertTxDependent #-}
insertTxDependent :: Unique -> Weak (TxDependent c) -> TxDependents c -> IO ()
insertTxDependent !did !d !deps = WeakMap.insertWeak deps did d

-- ** Transactional support

-- | commits local buffered changes to the original thunk, and returns a log of writes (ignores reads, evals and news)
-- when @doWrites@ is turned off we simply mark new dependencies as original
-- the second returned action wakes up sleeping txs that are listening to changed modifiables
-- we can commit buffered dependents of Writes even when doEvals is False because: if X=Write and X -buffered->Y then Y=Write
commitDynTxVar :: (TxLayer Outside c) => DynTxVar c -> Bool -> Bool -> IO (RepairDynTxVar c,Wakes)
commitDynTxVar (DynTxU (BuffTxU buff_dta) txdeps u txstat :: DynTxVar c) doWrites doEvals = do
	let !proxy = Proxy :: Proxy c
	stat <- readIORef' txstat
	case stat of
		TxStatus (Read Nothing :!: b) -> do
			let idu = (idTxNM $ metaTxU u)
			-- add buffered dependents on top of persistent dependents
			when (doEvals && b) $ -- we only commit dependents if we have an Eval lock
				WeakMap.unionWithKey' (dependentsTxNM $ metaTxU u) txdeps
			checks <- if (doEvals && b)
				then checkTxNotifies proxy False (metaTxU u) (notifiesTxNM $ metaTxU u)
				else return mempty
			return (checks,Map.empty)
		TxStatus (Eval Nothing :!: b) -> do
			-- commit the buffered data to the original thunk
			-- for dependencies we change the reference itself
			when (doEvals && b) $ -- we only commit dependents if we have an Eval lock
				WeakMap.unionWithKey' (dependentsTxNM $ metaTxU u) txdeps
			let idu = (idTxNM $ metaTxU u)
			let commit = do
				(dta,ori_dependencies) <- readIORef' buff_dta
				case dta of
					TxValue dirty value force txrdependencies -> do
						markOriginalDependenciesTx txrdependencies
						commitDependenciesTx txrdependencies ori_dependencies
					otherwise -> return ()
				writeIORef (dataTxU u) $! dta
				return ()
			when doEvals commit
			checks <- if doEvals && b
				then checkTxNotifies proxy False (metaTxU u) (notifiesTxNM $ metaTxU u)
				else return mempty
			return (checks,Map.empty)
		TxStatus (Write Nothing :!: b) -> do
			when (doWrites && b) $ -- all buffered dependents of a write must be writes
				WeakMap.unionWithKey' (dependentsTxNM $ metaTxU u) txdeps
			-- commit the buffered data to the original thunk
			-- for dependencies we change the reference itself
			let commit = do
				(dta,ori_dependencies) <- readIORef' buff_dta
				case dta of
					TxValue dirty value force txrdependencies -> do
						markOriginalDependenciesTx txrdependencies
						commitDependenciesTx txrdependencies ori_dependencies
					otherwise -> return ()
				writeIORef (dataTxU u) $! dta
				
			let idu = (idTxNM $ metaTxU u)
			if doWrites
				then do
					commit
					wakes <- wakeUpWaits (metaTxU u)
					checks <- checkTxNotifies proxy True (metaTxU u) (notifiesTxNM $ metaTxU u)
					return (checks,wakes)
				else return mempty
		TxStatus (New isNewWrite :!: b) -> do
			let idu = (idTxNM $ metaTxU u)
			let mark = readIORef' (dataTxU u) >>= \dta -> case dta of
				(TxValue _ _ force dependencies) -> markOriginalDependenciesTx dependencies
				otherwise -> return ()
			-- no need to unmemoize, because we only commit buffered memo tables if evals and writes succeed
			let forget = modifyIORefM_' (dataTxU u) $ \dta -> case dta of
				(TxValue _ _ force dependencies) -> clearTxDependencies True dependencies >> return (TxThunk force)
				otherwise -> return dta
			if isNewWrite
				then do
					-- if the new thunk depends on a non-committed write, we forget its value
					if doWrites then mark else forget 
					-- we commits its dependents anyway
					checks <- if doWrites
						then checkTxNotifies proxy True (metaTxU u) (notifiesTxNM $ metaTxU u)
						else return mempty
					return (checks,Map.empty) -- there are no wakeups, since no previous tx that reads this variable may have committed
				else do
					-- we need to check for evals because a @New@ thunk may depend on evaluated dependencies
					if doEvals then mark else forget
					checks <- if (doEvals && b)
						then checkTxNotifies proxy False (metaTxU u) (notifiesTxNM $ metaTxU u)
						else return mempty
					return (checks,Map.empty)
		st -> error $ "commitDynTxU " ++ show st
commitDynTxVar (DynTxM (BuffTxM value) txdeps m txstat :: DynTxVar c) doWrites doEvals = do
	let !proxy = Proxy :: Proxy c
	stat <- readIORef' txstat
	case stat of
		TxStatus (Read Nothing :!: b) -> do
			-- add buffered dependents on top of persistent dependents
			when (doEvals && b) $
				WeakMap.unionWithKey' (dependentsTxNM $ metaTxM m) txdeps
			let idm = idTxNM $ metaTxM m
			checks <- if doEvals && b
				then checkTxNotifies proxy False (metaTxM m) (notifiesTxNM $ metaTxM m)
				else return mempty
			return (checks,Map.empty)
		TxStatus (Eval Nothing :!: b) -> do
			when (doEvals && b) $ -- we only commit dependents if we have an Eval lock
				WeakMap.unionWithKey' (dependentsTxNM $ metaTxM m) txdeps

			let idm = idTxNM $ metaTxM m
			let commit = readIORef' value >>= writeIORef' (dataTxM m)
			when doEvals commit
			checks <- if doEvals && b
				then checkTxNotifies proxy False (metaTxM m) (notifiesTxNM $ metaTxM m)
				else return mempty
			return (checks,Map.empty)
		TxStatus (Write Nothing :!: b) -> do
			when (doWrites && b) $ -- all buffered dependents of a write must be writes
				WeakMap.unionWithKey' (dependentsTxNM $ metaTxM m) txdeps
			let commit = readIORef' value >>= writeIORef' (dataTxM m)
			let idm = idTxNM $ metaTxM m
			if doWrites
				then do
					commit
					wakes <- wakeUpWaits (metaTxM m)
					wmeta <- mkWeakRefKey (dataTxM m) (metaTxM m) Nothing
					checks <- checkTxNotifies proxy True (metaTxM m) (notifiesTxNM $ metaTxM m)
					return (checks,wakes)
			else return mempty
		TxStatus (New isNewWrite :!: b) -> do
			let idm = (idTxNM $ metaTxM m)
			if isNewWrite
				then do
					-- we commits its dependents anyway
					checks <- if doWrites
						then checkTxNotifies proxy True (metaTxM m) (notifiesTxNM $ metaTxM m)
						else return mempty
					return (checks,Map.empty) -- there are no wakeups, since no previous tx that reads this variable may have committed
				else do
					checks <- if (doEvals && b)
						then checkTxNotifies proxy False (metaTxM m) (notifiesTxNM $ metaTxM m)
						else return mempty
					return (checks,Map.empty)
		st -> error $ "commitDynTxM " ++ show st

-- applies a buffered log to the global state
-- note that we report changes as a whole, since dependent output thunks don't report modifications on their own
commitTxLog :: (TxLayer Outside c) => TxLog c -> TxId c -> Bool -> Bool -> IO ((TxUnmemo c,RepairDynTxVar c),Wakes)
commitTxLog (txlog :: TxLog c) txid doWrites doEvals = {-# SCC commitTxLog #-} do
	let !proxy = Proxy :: Proxy c
	!thread <- myThreadId
	txblock <- flattenTxLogBlocks txlog
	
	-- commits buffered modifiable/thunk data
	let add !xs !(uid,tvar) = do
		-- remove this thread (always a root thread) from the notify list
		unTxNotifies proxy thread uid $! dynTxNotifies tvar
		-- commit changes
		ys <- commitDynTxVar tvar doWrites doEvals
		-- remove buffered memotables from the variable's memo list
		when (isDynTxU tvar) $ modifyMVarMasked_' "commitTxLog" (unmemoTxNM $! dynTxMeta tvar) $ \(ori :!: buffs) -> return (ori :!: Map.delete thread buffs)
		-- delete content cache for this thread
		CMap.delete thread (contentTxNM $ dynTxMeta tvar)
		return $! mappend xs ys
	(writes,wakes) <- MemoTable.foldM add mempty txblock
	
	-- commits transaction-local memo tables
	txunmemo <- commitTxLogMemoTables (doWrites && doEvals) thread txlog
	-- finalize the whole buffered table
	finalizeTxLog txlog
	return ((txunmemo,writes),wakes)
	
-- | registers a thunk to be woken up by modifications on the variables that it reads
-- we need to delete writes on retry, otherwise the effects of a failed tx may become visible or lead to inconsistencies, e.g., if the flow of the program changed
-- note that we nevertheless wait on writes, since they could have read the variable before writing to it (we don't distinguish these two cases)
retryDynTxVar :: (TxLayer Outside c) => TxLog c -> Lock -> Unique -> DynTxVar c -> IO ()
retryDynTxVar txlog lck uid tvar = do
	stat <- readIORef' $ dynTxStatus tvar
	when (not $ isNew stat) $ enqueueWait lck (dynTxMeta tvar)

retryTxLog :: (TxLayer Outside c) => Lock -> TxLog c -> IO ()
retryTxLog lck txlog = flattenTxLogBlocks txlog >>= MemoTable.mapM_ (\(uid,dyntxvar) -> retryDynTxVar txlog lck uid dyntxvar)

-- lifts all writes to the innermost nested txlog
liftTxLogsWrites :: (TxLayer l c,TxLayer Outside c) => ThreadId -> TxLogs c -> l (TxAdapton c) ()
liftTxLogsWrites thread (SCons txlog txlogs) = do
	b <- isInside
	unless b $ unsafeIOToInc $ do
		txblock <- flattenTxLogBlocks txlog
		liftTxLogsWrites' txblock txlogs
		
  where
--	liftTxLogsWrites' :: MonadBlock c IO => TxLog c -> TxLogs c -> IO ()
	liftTxLogsWrites' txblock SNil = return ()
	liftTxLogsWrites' txblock (SCons txlog1 txlogs1) = do
		xs <- readIORef (txLogBuff txlog1)
		Foldable.mapM_ (MemoTable.mapM_ (liftWrite txblock)) xs
		liftTxLogsWrites' txblock txlogs1
--	liftWrite :: MonadBlock c IO => TxLog c -> (Unique,DynTxVar c) -> IO ()
	liftWrite txblock (uid,tvar) = do
		stat <- readIORef' $ dynTxStatus tvar
		when (isWriteOrNewTrue stat) $ do
		mb <- MemoTable.lookup txblock uid
		case mb of
			Nothing -> addTxLogEntryUp thread (SCons txlog SNil) uid tvar
			-- don't overlap
			Just _ -> return ()

-- extends a base txlog with all its enclosing txlogs, ignoring writes in all of them
{-# INLINE flattenTxLogs_ #-}
flattenTxLogs_ txlogs = readRootTx >>= \(isFuture,rootThread) -> unsafeIOToInc myThreadId >>= \thread -> flattenTxLogs isFuture rootThread thread txlogs >> return ()
flattenTxLogs :: (TxLayer Inside c,TxLayer Outside c,TxLayer l c) => Bool -> ThreadId -> ThreadId -> TxLogs c -> l (TxAdapton c) (TxLog c)
flattenTxLogs isFuture rootThread thread txlogs@(SCons toptxlog SNil) = {-# SCC flattenTxLogs #-} unbufferTopTxLog isFuture thread txlogs True >> return toptxlog
flattenTxLogs isFuture rootThread thread (SCons txlog txlogs) = {-# SCC flattenTxLogs #-} do
	unsafeIOToInc $ commitNestedTx isFuture rootThread thread False txlog txlogs
	flattenTxLogs isFuture rootThread thread txlogs

throwTx :: (TxLayer Outside c,Exception e) => e -> Outside (TxAdapton c) a
throwTx = txLayer Proxy . Catch.throwM

throwInvalidTx :: (TxLayer l c) => InvalidTxRepair c -> l (TxAdapton c) a
throwInvalidTx = throwInvalidTxProxy Proxy
	where
	throwInvalidTxProxy :: (TxLayer l c) => Proxy c -> InvalidTxRepair c -> l (TxAdapton c) a
	throwInvalidTxProxy (c :: Proxy c) repair = txLayer Proxy $ Catch.throwM $ (InvalidTx repair :: InvalidTx c)

-- kills all futures spawned by this thread
killChildrenTx :: TxLayer l c => l (TxAdapton c) ()
killChildrenTx = do
	fsref <- readFuturesTx
	unsafeIOToInc $ do
		fs <- readIORef' fsref
		Map.foldrWithKey (\k v m -> killThread k >> m) (return ()) fs
		writeIORef' fsref Map.empty

waitForChildrenTx :: (TxLayer Inside c,TxLayer Outside c,TxLayer l c) => l (TxAdapton c) ()
waitForChildrenTx = do
	fsref <- readFuturesTx
	fs <- unsafeIOToInc $ readIORef' fsref
	Foldable.mapM_ waitForChild fs
  where
	waitForChild :: (TxLayer Inside c,TxLayer Outside c,TxLayer l c) => DynTxFuture c -> l (TxAdapton c) ()
	waitForChild (DynTxFuture f) = joinTx f >> return ()

catchTx :: (TxLayer Inside c,TxLayer Outside c,Exception e) => Outside (TxAdapton c) a -> (e -> Outside (TxAdapton c) a) -> Outside (TxAdapton c) a
catchTx (stm :: Outside (TxAdapton c) a) (h :: e -> Outside (TxAdapton c) a) = txLayer proxyOutside $ (unTxLayer proxyOutside stm) `Catch.catches` [unmaskedHandler catchInvalid,unmaskedHandler catchRetry,unmaskedHandler catchExcpt] where
	catchInvalid (e::InvalidTx c) = Catch.throwM e
	catchRetry (e::BlockedOnTxRetry) = Catch.throwM e
	catchExcpt (e::e) = unTxLayer proxyOutside $ do
		validateCatchTx "catchTx"
		h e

unmaskedHandler :: (Exception e,MonadIO m) => (e -> m a) -> Catch.Handler m a
unmaskedHandler f = Catch.Handler $ \e -> liftIO allowInterrupt >> f e
{-# INLINE unmaskedHandler #-}

atomicallyTx :: (TxLayer Inside c,TxLayer Outside c,TxLayer l c) => IncParams (TxAdapton c) -> String -> l (TxAdapton c) a -> IO a
atomicallyTx (params :: IncParams (TxAdapton c)) msg (stm :: l (TxAdapton c) a) = initializeTx params try where
	l = Proxy :: Proxy l
	try = txLayer l $ flip Catch.catches [unmaskedHandler catchInvalid,unmaskedHandler catchRetry,unmaskedHandler catchSome] $ unTxLayer l $ do
		debugTx $ "started tx " ++ msg
		-- run the tx
		x <- stm
		-- tries to commit the current tx, otherwise repairs it incrementally
		!mbsuccess <- validateAndCommitTopTx msg True
		case mbsuccess of
			Nothing -> do
				debugTx $ "finished tx " ++ msg
				exitTx -- prepares to leave the tx's environment (no exceptions are caught outside @atomicallyTx@)
				return x
			Just repair -> throwInvalidTx repair
	catchInvalid (InvalidTx repair :: InvalidTx c) = flip Catch.catches [unmaskedHandler catchInvalid] $ unTxLayer l $ do -- concurrent invalidation exceptions
		killChildrenTx
		starttime <- readTxId
		let withRepair = if (txAdaptonRepair params) then Just repair else Nothing
		let msg = "caught InvalidTx: retrying tx previously known as " ++ show starttime
		restartTxWithRepair withRepair msg starttime try
	catchRetry BlockedOnTxRetry = flip Catch.catches [unmaskedHandler catchInvalid] $ unTxLayer l $ do -- concurrent invalidation exceptions
		killChildrenTx
		debugTx "caught BlockedOnTxRetry"
		-- if the retry was invoked on an inconsistent state, we incrementally repair and run again, otherwise we place the tx in the waiting queue
		mbsuccess <- validateAndRetryTopTx msg
		case mbsuccess of
			Left lck -> do -- retried txs are always in a consistent state, because we apply all affecting updates before releasing the lock
				debugTx "put tx to sleep"
				-- wait for the lock to be released (whenever some variables that it depends on are changed)
				-- we don't consume the contents of the mvar to avoid further puts to succeeed; a new MVar is created for each retry
				unsafeIOToInc $ Lock.acquire "catchRetry" lck
				debugTx $ "woke up tx"
				starttime <- readTxId
				restartTxWithRepair Nothing ("try: BlockedOnTxRetry retrying invalid tx previously known as " ++ show starttime) starttime try
			Right repair -> do
				starttime <- readTxId
				let withRepair = if (txAdaptonRepair params) then Just repair else Nothing
				let msg = "caughtRetry InvalidTx: retrying tx previously known as " ++ show starttime
				restartTxWithRepair withRepair msg starttime try
	catchSome (e::SomeException) = flip Catch.catches [unmaskedHandler catchInvalid] $ unTxLayer l $ do -- concurrent invalidation exceptions
		killChildrenTx
		-- handle ThreadKilled differently
		case fromException e :: Maybe AsyncException of
			Just ThreadKilled -> txLayer l $ Catch.throwM e -- just die, no validation
			otherwise -> do
				debugTx $ "caught SomeException " ++ show e
				-- we still need to validate on exceptions, otherwise repair incrementally; transaction-local allocations still get committed
				mbsuccess <- validateAndCommitTopTx msg False
				case mbsuccess of
					Nothing -> do
						debugTx $ "finished exceptional tx " ++ msg
						txLayer Proxy $ Catch.throwM e
					Just repair -> do
						starttime <- readTxId
						let withRepair = if (txAdaptonRepair params) then Just repair else Nothing
						let msg = "try: SomeException retrying invalid tx previously known as " ++ show starttime
						restartTxWithRepair withRepair msg starttime try

retryTx :: TxLayer Outside c => Outside (TxAdapton c) a
retryTx = unsafeIOToInc $ throwIO BlockedOnTxRetry

-- if an alternative retries, its non-write effects are merged with the parent tx log, to allow IC reuse; when the alternative is retried, the parent log will already contain its previous data.
-- if both alternatives retry, then both their logs will be merged with the parent, as with STM
orElseTx :: (TxLayer Outside c,TxLayer Inside c) => Outside (TxAdapton c) a -> Outside (TxAdapton c) a -> Outside (TxAdapton c) a
orElseTx (stm1 :: Outside (TxAdapton c) a) stm2 = do1 where
	try1 = do { x <- stm1; validateAndCommitNestedTx "orElse1" Nothing; return x }
	try2 = do { x <- stm2; validateAndCommitNestedTx "orElse2" Nothing; return x }
	do1 = startNestedTx $ txLayer proxyOutside $ (unTxLayer proxyOutside try1) `Catch.catches` [unmaskedHandler catchRetry1,unmaskedHandler catchInvalid,unmaskedHandler catchSome]
	do2 = startNestedTx $ txLayer proxyOutside $ (unTxLayer proxyOutside try2) `Catch.catches` [unmaskedHandler catchRetry2,unmaskedHandler catchInvalid,unmaskedHandler catchSome]
	catchRetry1 BlockedOnTxRetry = unTxLayer proxyOutside $ validateAndRetryNestedTx "orElseRetry1" >> do2
	catchRetry2 BlockedOnTxRetry = unTxLayer proxyOutside (validateAndRetryNestedTx "orElseRetry2") >> Catch.throwM BlockedOnTxRetry
	catchInvalid (e::InvalidTx c) = Catch.throwM e
	catchSome (e::SomeException) = unTxLayer proxyOutside (validateAndCommitNestedTx "orElseSome" (Just e)) >> Catch.throwM e

-- appends a freshly created txlog for the inner tx
startNestedTx :: TxLayer Outside c => Outside (TxAdapton c) a -> Outside (TxAdapton c) a
startNestedTx m = do
	(params :!: root :!: fs :!: starttime :!: stack :!: txlogs) <- Reader.ask
	unsafeIOToInc $ do
		txlog <- emptyTxLog starttime $ txAdaptonMemoSize params
		Reader.runReaderT (runTxOuter m) (params :!: root :!: fs :!: starttime :!: stack :!: SCons txlog txlogs)

-- spawns a nested transaction in a different thread
futureTx :: (TxLayer Inside c,TxLayer Outside c) => Outside (TxAdapton c) a -> Outside (TxAdapton c) (TxFuture c a)
futureTx stm = do
	(_,rootThread) <- readRootTx
	result <- unsafeIOToInc newEmptyMVar
	let !f = TxFuture result
	forkNestedTx rootThread f (try result)
	return f
  where
	try result = do
		x <- stm
		mb <- validateFutureTx "futureTx" Nothing
		txenv <- Reader.ask
		thread <- unsafeIOToInc myThreadId
		case mb of
			Nothing -> unsafeIOToInc $ putMVar "futureTx" result (Right x :!: thread :!: txenv)
			Just repair -> unsafeIOToInc $ putMVar "futureTx" result (Left (toException $ InvalidTx repair) :!: thread :!: txenv)

joinTx :: (TxLayer Inside c,TxLayer Outside c,TxLayer l c) => TxFuture c a -> l (TxAdapton c) a
joinTx f@(TxFuture result) = do
	e <- unsafeIOToInc $ readMVar "joinTx" result
	case e of
		(Left ex :!: child_thread :!: child_env) -> do
			case fromException ex :: Maybe TxException of
				Just _ -> return ()
				otherwise -> commitFutureTx "joinTx" False child_thread child_env
			unsafeIOToInc $ Control.Exception.throw ex
		(Right v :!: child_thread :!: child_env) -> do
			commitFutureTx "joinTx" True child_thread child_env
			return v
  

-- appends a freshly created txlog for the inner tx
forkNestedTx :: (TxLayer Inside c,TxLayer Outside c) => ThreadId -> TxFuture c a -> Outside (TxAdapton c) () -> Outside (TxAdapton c) ()
forkNestedTx rootThread (f :: TxFuture c a) m = do
	parent_thread <- unsafeIOToInc myThreadId
	(params :!: root :!: fs :!: starttime :!: stack :!: txlogs) <- Reader.ask
	
	let child = (runTxOuter m) `Catch.catches` [unmaskedHandler catchRetry,unmaskedHandler catchInvalid,unmaskedHandler catchSome]
	
	unsafeIOToInc $ do
		mkSnap <- snapshotTxLogs (txAdaptonMemoSize params) parent_thread txlogs
		forkIO $ do
			thread <- myThreadId
			snap <- mkSnap thread
			fs' <- newIORef' Map.empty
			txlog <- emptyTxLog starttime $ txAdaptonMemoSize params
			Reader.runReaderT child (params :!: Just rootThread :!: fs' :!: starttime :!: stack :!: SCons txlog snap)
		return ()
  where
	catchRetry BlockedOnTxRetry = do
		thread <- lift myThreadId
		unTxLayer proxyOutside (validateFutureTx "futureRetry" Nothing)
		exceptionalFutureTx rootThread thread f BlockedOnTxRetry
	catchInvalid (e::InvalidTx c) = do
		thread <- lift myThreadId
		exceptionalFutureTx rootThread thread f e
	catchSome (e::SomeException) = do
		case fromException e :: Maybe AsyncException of
			Just ThreadKilled -> return () -- die gracefully if killed by parent
			otherwise -> do -- don't throw the exception to the parent yet because it requires committing
				thread <- lift myThreadId
				unTxLayer proxyOutside (validateFutureTx "futureSome" (Just e))
				txenv <- Reader.ask
				lift $ putMVar "forkNestedTx" (unTxFuture f) (Left e :!: thread :!: txenv)

-- validates a nested tx and its enclosing txs up the tx tree
-- the repairing action unmemoizes possible conflicting buffered memo entries and repairs variables that were conflicting
validateTxsCommit :: Bool -> TxId CommitConflict -> TxLogs CommitConflict -> IO (Maybe (InvalidTxRepair CommitConflict))
validateTxsCommit isFuture starttime txlogs = do
	!thread <- myThreadId
	-- gets the transactions that committed after the current transaction's start time
	txs <- readMVar "validateTxsCommit" doneTxs
	let finished = Map.toAscList $ Map.filterWithKey (\k v -> k > starttime) txs
	let finishtime = if List.null finished then starttime else Prelude.fst (List.last finished)
	mbrepairs <- validateTxsCommit' isFuture thread starttime txlogs finished
	case mbrepairs of
		Nothing -> return Nothing
		Just repairs -> return $ Just (finishtime :!: repairs)

validateTxsCommit' :: Bool -> ThreadId -> UTCTime -> TxLogs CommitConflict -> [(UTCTime,(TxUnmemo CommitConflict,RepairDynTxVar CommitConflict))] -> IO (Maybe (TxRepair CommitConflict))
validateTxsCommit' isFuture thread starttime txlogs finished = do
	let (txtimes,txunmemos,txwrites) = compactFinished finished
	debugTx' $ "[" ++ show starttime ++ "] validating against " ++ show txtimes {- ++ ""  ++ show txwrites-}
	checkTxsCommit isFuture thread txlogs (txunmemos,txwrites)

compactFinished :: [(UTCTime,(TxUnmemo CommitConflict,RepairDynTxVar CommitConflict))] -> ([UTCTime],TxUnmemo CommitConflict,RepairDynTxVar CommitConflict)
compactFinished [] = ([],mempty,mempty)
compactFinished ((t1,(u1,w1)):xs) = let (t2,u2,w2) = compactFinished xs in (t1 : t2,u1 `mappend` u2,mappend w1 w2)

commitTopTx :: (TxLayer Outside c) => TxLog c -> TxId c -> Bool -> Bool -> IO ()
commitTopTx txlog txid doWrites doEvals = do
	-- commits the log and gets a sequence of performed writes
	(writes,wakeups) <- commitTxLog txlog txid doWrites doEvals
	-- finishes the current tx and deletes txs that finished before the start of the earliest running tx
	-- we don't need to log transactions with empty commits (no @Eval@s or @Write@s)
	-- deletes this transaction from the running list and gets the earliest running tx 
	finishTx txid writes
	-- wakes up the transactions after updating their buffered content
	Foldable.mapM_ tryRelease wakeups

-- makes the parent log sensitive to the variables used in the nested branch
commitNestedTx :: (TxLayer Inside c,TxLayer Outside c) => Bool -> ThreadId -> ThreadId -> Bool -> TxLog c -> TxLogs c -> IO ()
commitNestedTx isFuture rootThread thread doWrites txlog_child txlogs_parent = {-# SCC flattenTxLogs #-} do
	
	-- delayed child finalization (since we re-used the weak pointers on merge, and finalizing the child txlog now would kill them as well)
	let !txlog_parent = Strict.head txlogs_parent
	mkWeakKey txlog_parent txlog_parent $! Just $! finalizeTxLog txlog_child
	
	-- merge the modifications with the parent log
	mergeTxLog doWrites isFuture thread txlog_child txlogs_parent
	-- merges the buffered memo table entries for a txlog with its parent
	mergeTxLogMemos thread txlog_child txlogs_parent
	return ()

commitFutureTx :: (TxLayer Inside c,TxLayer Outside c,TxLayer l c) => String -> Bool -> ThreadId -> TxEnv c -> l (TxAdapton c) ()
commitFutureTx msg doWrites child_thread child_env@(txEnvLogs -> SCons child_txlog _) = do
	waitForChildrenTx
	parent_txlogs <- readTxLogs
	parent_thread <- unsafeIOToInc myThreadId
	(parent_isFuture,_) <- readRootTx
	-- commit child txlog
	dirties <- unsafeIOToInc $ mergeFutureTxLog doWrites parent_isFuture parent_thread child_thread parent_txlogs child_env
	
	-- merge memos
	unsafeIOToInc $ mergeFutureTxLogMemos child_thread parent_thread child_txlog parent_txlogs

-- returns a bool stating whether the transaction was committed or needs to be incrementally repaired
-- no exceptions should be raised inside this block
validateAndCommitTopTx :: (TxLayer Inside c,TxLayer Outside c,TxLayer l c) => String -> Bool -> l (TxAdapton c) (Maybe (InvalidTxRepair c))
validateAndCommitTopTx msg doWrites = waitForChildrenTx >> atomicTx ("validateAndCommitTopTx "++msg) (\doEvals -> do
	!txenv@(_ :!: timeref :!: callstack :!: txlogs@(SCons txlog SNil)) <- Reader.ask
	starttime <- unsafeIOToInc $ readIORef' timeref
	(isFuture,_) <- readRootTx
	mbsuccess <- unsafeIOToInc $ validateTxs isFuture starttime txlogs
	case mbsuccess of
		Nothing -> do
			unsafeIOToInc $ commitTopTx txlog starttime doWrites doEvals
			return Nothing
		Just conflicts -> 
			return $! Just $ TxRepair (flattenTxLogs_ txlogs) `appendInvalidTxRepair` conflicts)

validateAndCommitNestedTx :: (TxLayer Inside c,TxLayer Outside c) => String -> Maybe SomeException -> Outside (TxAdapton c) ()
validateAndCommitNestedTx msg mbException = do
	!txenv@(_ :!: timeref :!: callstack :!: txlogs@(SCons txlog1 txlogs1)) <- Reader.ask
	starttime <- unsafeIOToInc $ readIORef' timeref
	(isFuture,rootThread) <- readRootTx
	case mbException of
		Just e -> do -- throwing an exception exits the chain of txs one by one
			doBlockTx $ unsafeIOToInc $ myThreadId >>= \thread -> commitNestedTx isFuture rootThread thread False txlog1 txlogs1 -- does not perform @Write@s
		Nothing -> do
			-- validates the current and enclosing txs up the tx tree
			mbsuccess <- unsafeIOToInc $ validateTxs isFuture starttime txlogs
			case mbsuccess of
				Nothing -> doBlockTx $ unsafeIOToInc $ myThreadId >>= \thread -> commitNestedTx isFuture rootThread thread True txlog1 txlogs1 -- performs @Write@s
				Just conflicts -> 
					-- re-start from the top
					throwInvalidTx $ TxRepair (flattenTxLogs_ txlogs) `appendInvalidTxRepair` conflicts

validateFutureTx :: (TxLayer Inside c,TxLayer Outside c) => String -> Maybe SomeException -> Outside (TxAdapton c) (Maybe (InvalidTxRepair c))
validateFutureTx msg mbException = do
	!txenv@(_ :!: timeref :!: callstack :!: txlogs@(SCons txlog1 txlogs1)) <- Reader.ask
	starttime <- unsafeIOToInc $ readIORef' timeref
	(isFuture,rootThread) <- readRootTx
	case mbException of
		Just e -> do -- throwing an exception exits the chain of txs one by one
			return Nothing
		Nothing -> do
			-- validates the current and enclosing txs up the tx tree
			unsafeIOToInc $ validateTxs isFuture starttime txlogs

validateCatchTx :: (TxLayer Inside c,TxLayer Outside c) => String -> Outside (TxAdapton c) ()
validateCatchTx msg = do
	txlogs <- readTxLogs
	starttime <- readTxId
	(isFuture,_) <- readRootTx
	mbsuccess <- unsafeIOToInc $ validateTxs isFuture starttime txlogs
	case mbsuccess of
		Nothing -> do
			-- in case the computation raises an exception, discard all its visible (write) effects
			-- unbuffer all writes at the innermost log
			thread <- unsafeIOToInc myThreadId
			doBlockTx $ liftTxLogsWrites thread txlogs >> unbufferTopTxLog isFuture thread txlogs True
		Just conflicts -> 
			throwInvalidTx $ TxRepair (flattenTxLogs_ txlogs) `appendInvalidTxRepair` conflicts

-- validates a transaction and places it into the waiting queue for retrying
validateAndRetryTopTx :: (TxLayer Inside c,TxLayer Outside c,TxLayer l c) => String -> l (TxAdapton c) (Either Lock (InvalidTxRepair c))
validateAndRetryTopTx msg = atomicTx ("validateAndRetryTopTx "++msg) $ \doEvals -> do
	!txenv@(_ :!: timeref :!: callstack :!: txlogs@(SCons txlog SNil)) <- Reader.ask
	starttime <- unsafeIOToInc $ readIORef' timeref
	(isFuture,_) <- readRootTx
	-- validates the current and enclosing txs up the tx tree
	mbsuccess <- unsafeIOToInc $ validateTxs isFuture starttime txlogs
	case mbsuccess of
		Nothing -> do
			lck <- unsafeIOToInc $ Lock.newAcquired -- sets the tx lock as acquired; the tx will be resumed when the lock is released
			unsafeIOToInc $ commitTopTx txlog starttime False doEvals -- commit @Eval@ and @New@ computations
			unsafeIOToInc $ retryTxLog lck txlog -- wait on changes to retry (only registers waits, does not actually wait)
			return $ Left lck
		Just conflicts -> 
			return $ Right $ TxRepair (flattenTxLogs_ txlogs) `appendInvalidTxRepair` conflicts

-- validates a nested transaction and merges its log with its parent
-- note that retrying discards the tx's writes
validateAndRetryNestedTx :: (TxLayer Inside c,TxLayer Outside c) => String -> Outside (TxAdapton c) ()
validateAndRetryNestedTx msg = do
	!txenv@(_ :!: timeref :!: callstack :!: txlogs@(SCons txlog1 txlogs1)) <- Reader.ask
	starttime <- unsafeIOToInc $ readIORef' timeref
	(isFuture,_) <- readRootTx
	mbsuccess <- unsafeIOToInc $ validateTxs isFuture starttime txlogs
	(isFuture,rootThread) <- readRootTx
	case mbsuccess of
		Nothing -> doBlockTx $ unsafeIOToInc $ myThreadId >>= \thread -> commitNestedTx isFuture rootThread thread False txlog1 txlogs1 -- does not perform @Write@s on @retry@
		Just conflicts ->
			-- discards writes and applies the conflicting changes to locally repair the current tx
			throwInvalidTx $ TxRepair (flattenTxLogs_ txlogs) `appendInvalidTxRepair` conflicts 

-- like STM, our txs are:
-- same as transaction repair: within transactions, we use no locks; we use locks for commit
-- 1) disjoint-access parallel: non-overlapping writes done in parallel
-- 2) read-parallel: reads done in parallel
-- 3) eval-semi-parallel: evals done in parallel, with the updates of the latest eval being discarded; both evals succeed, but their commits are not parallel though (one commits first and the other discards its evaluated data, and commits only dependents)
-- runs transaction-specific code atomically in respect to a global state
atomicTx :: (TxLayer l c,TxLayer Inside c,TxLayer Outside c) => String -> (Bool -> l (TxAdapton c) a) -> l (TxAdapton c) a
atomicTx msg m = do
	debugTx $ "waiting " ++ msg
	withTxLocks $ \doEvals -> do
		debugTx $ "locked " ++ show doEvals ++ " " ++ msg 
		x <- m doEvals
		debugTx $ "unlocked " ++ show doEvals ++ " " ++ msg
		return x
	
--	SCons txlog SNil <- readTxLogs
--	(read_lcks,eval_lcks,write_lcks) <- unsafeIOToInc $ txLocks txlog
--	
--	debugTx $ "waiting " ++ msg ++ " " -- ++ show reads ++ " " ++ show evals ++ " " ++ show writes
--	withLocksTx read_lcks eval_lcks write_lcks $ \doEval -> do
--		debugTx $ "locked " ++ show doEval ++ " " -- ++ msg ++ if doEval then show evals else "" ++ " " ++ show writes
--		x <- m doEval
--		debugTx $ "unlocked " ++ msg -- ++ if doEval then show evals else "" ++ " " ++ show writes
--		return x

{-# INLINE initializeTx #-}
initializeTx :: TxLayer l c => IncParams (TxAdapton c) -> l (TxAdapton c) a -> IO a
initializeTx (params :: IncParams (TxAdapton c)) m = do
	starttime <- startTx (Proxy :: Proxy c) >>= newIORef'
	stack <- newIORef' SNil
	fs <- newIORef' Map.empty
	tbl <- emptyTxLog starttime $ txAdaptonMemoSize params
	Reader.runReaderT (runTxOuter $ outside m) (params :!: Nothing :!: fs :!: starttime :!: stack :!: SCons tbl SNil)

-- checks if the current txlog is consistent with a sequence of concurrent modifications
-- Nothing = success
-- Just = some conflicting changes happened simultaneously to the current tx
-- if the current tx uses a variable that has been written to before, there is a conflict
-- note that we also consider write-write conflicts, since we don't log written but not read variables differently from read-then-written ones
checkTxsCommit :: Bool -> ThreadId -> TxLogs CommitConflict -> (TxUnmemo CommitConflict,RepairDynTxVar CommitConflict) -> IO (Maybe (TxRepair CommitConflict))
checkTxsCommit isFuture thread txlogs (unmemo,RepairDynTxVarC writtenIDs) = do
	ok <- Map.foldrWithKey (\meta st m2 -> checkDynTxVar txlogs meta st >>= \mb1 -> m2 >>= \mb2 -> return $ mb1 `mappend` mb2) (return Nothing) writtenIDs
	case ok of
		Nothing -> return Nothing
		Just repair -> return $ Just $ mappend repair unmemo
  where
	checkDynTxVar :: TxLogs CommitConflict -> TxNodeMeta CommitConflict -> Bool -> IO (Maybe (TxRepair CommitConflict))
	checkDynTxVar txlogs meta True = do -- concurrent write
		mb <- findTxContentEntry isFuture thread txlogs meta
		case mb of
			Nothing -> return Nothing
			Just (!tvar,!isTop) -> do
				-- Write-XXX are always conflicts
				-- if there was a previous @Eval@ or @Write@ on a buffered variable we discard buffered content but keep buffered dependents
				debugTx2' $ "write conflict " ++ show (idTxNM meta)
				return $ Just $ TxRepair $ readTxLogs >>= \txlogs -> readRootTx >>= \(isFuture,_) -> unsafeIOToInc $ myThreadId >>= \thread -> unbufferTxVar isFuture thread False txlogs (dynTxMeta tvar) -- a conflict, unbuffer only later when tx is retried with dirtying
	checkDynTxVar txlogs meta False = do -- concurrent dependent write
		mb <- findTxContentEntry isFuture thread txlogs meta
		case mb of
			Nothing -> return Nothing
			Just (!tvar,!isTop) -> do
				stat <- readIORef' $ dynTxStatus tvar
				if isWriteOrNewTrue stat
					then do
						-- if a dependent was added concurrently while this tx dirtied a variable
						debugTx2' $ "write dependents conflict " ++ show (idTxNM meta)
						return $ Just $ TxRepair $ readTxLogs >>= \txlogs -> readRootTx >>= \(isFuture,_) -> unsafeIOToInc $ myThreadId >>= \thread -> unbufferTxVar isFuture thread False txlogs (dynTxMeta tvar) -- a conflict, unbuffer only later when tx is retried with dirtying
					else return Nothing

-- True = write, False = dependent write
checkTxNotifiesE :: (TxLayer Outside EarlyConflict) => Bool -> TxNodeMeta EarlyConflict -> TxNotifies EarlyConflict -> IO (RepairDynTxVar EarlyConflict)
checkTxNotifiesE True meta xs = readMVar "checkTxNotifiesE" xs >>= Map.foldrWithKey checkThread (return mempty) where -- concurrent writes
	checkThread thread _ m = liftM (RepairDynTxVarE . Map.insert thread (TxRepair $ readTxLogs >>= \txlogs -> readRootTx >>= \(isFuture,_) -> unsafeIOToInc $ unbufferTxVar isFuture thread False txlogs meta) . unRepairDynTxVarE) m
checkTxNotifiesE False meta xs = readMVar "checkTxNotifiesE" xs >>= Map.foldrWithKey checkThread (return mempty) where -- concurrent dependent writes (new dependents added to this node)
	checkThread thread isWriteOrNewTrue m = do
		if isWriteOrNewTrue -- unbuffer only if the current thread wrote to the dependents
			then liftM (RepairDynTxVarE . Map.insert thread (TxRepair $ readTxLogs >>= \txlogs -> readRootTx >>= \(isFuture,_) -> unsafeIOToInc $ unbufferTxVar isFuture thread False txlogs meta) . unRepairDynTxVarE) m
			else m	

-- restarts a tx with a new starting time
-- note that the time has already been added to the @runningTxs@
{-# INLINE restartTx #-}
restartTx :: TxLayer l c => TxId c -> l (TxAdapton c) a -> l (TxAdapton c) a
restartTx newtime m = do
 	!(_ :!: rootThread :!: fs :!: timeref :!: stack :!: logs) <- Reader.ask
	unsafeIOToInc $! writeIORef' timeref newtime
	m

{-# INLINE resetTx #-}
resetTx :: TxLayer l c => l (TxAdapton c) a -> l (TxAdapton c) a
resetTx (m :: l (TxAdapton c) a) = do
	!thread <- unsafeIOToInc myThreadId
	-- unmemoize all old buffered memotables
	readTxLogs >>= unsafeIOToInc . unbufferTxLogMemos thread
	!now <- unsafeIOToInc $! startTx (Proxy :: Proxy c) >>= newIORef'
	!stack <- unsafeIOToInc $! newIORef' SNil
	!params <- readTxParams
	!fs <- unsafeIOToInc $ newIORef' Map.empty
	!tbl <- unsafeIOToInc $ emptyTxLog now $ txAdaptonMemoSize params
	Reader.local (\_ -> (params :!: Nothing :!: fs :!: now :!: stack :!: SCons tbl SNil)) m

instance TxConflictClass EarlyConflict where
	
	addTxNotifies = addTxNotifiesE
	{-# INLINE addTxNotifies #-}
	newTxNotifies _ = newMVar Map.empty
	{-# INLINE newTxNotifies #-}
	unTxNotifies _ = unTxNotifiesE
	{-# INLINE unTxNotifies #-}
	checkTxNotifies _ = checkTxNotifiesE
	{-# INLINE checkTxNotifies #-}
	
	startTx _ = do
		t <- myThreadId
		addLiveTx t
		return t
	{-# INLINE startTx #-}
	
	finishTx txid (TxRepair txunmemo,RepairDynTxVarE notifies) = do
		debugTx' $ "FINISHED"
    	
		-- invalidates and repairs concurrent transactions, by sending an exception with the repair computation
		let invalidate thread (TxRepair repair) m = do
			mb <- findLiveTx thread
			case mb of
				Nothing -> do
					debugTx' $ "can't throw InvalidTx to non-transactional" ++ show thread
					return ()
				Just lck -> do
					Lock.with "finishTx" lck $ do
						debugTx' $ "throwing InvalidTx to " ++ show thread
						throwTo thread $ ((InvalidTx (txid :!: (TxRepair $ readTxLogs >>= flattenTxLogs_ >> repair >> txunmemo))) :: InvalidTx EarlyConflict)
			m
		Map.foldrWithKey invalidate (return ()) notifies
		debugTx' $ "thrown all"
	{-# INLINE finishTx #-}
		
	exitTx = readTxId >>= unsafeIOToInc . deleteLiveTx -- wait until it is safe to exit the current transaction (i.e., when others transactions will no longer throw asynchronous exceptions to the current thread)
	{-# INLINE exitTx #-}
		
	restartTxWithRepair (Just (from :!: TxRepair repair)) msg threadid m = restartTx threadid $ debugTx msg >> repair >> m
	restartTxWithRepair Nothing msg threadid m = resetTx $ debugTx msg >> m
	{-# INLINE restartTxWithRepair #-}

	validateTxs _ _ _ = return Nothing -- no intermediate validation, only when invalidation exceptions are received
	{-# INLINE validateTxs #-}
		
	appendInvalidTxRepair r1 (t :!: r2) = (t :!: mappend r1 r2)
	{-# INLINE appendInvalidTxRepair #-}
	extendInvalidTxRepair (t :!: r1) r2 = (t :!: mappend r1 r2)
	{-# INLINE extendInvalidTxRepair #-}

	exceptionalFutureTx rootThread thread (TxFuture result) e = lift $ throwTo rootThread e

{-# INLINE addTxNotifiesE #-}
-- always receives a root thread
addTxNotifiesE :: ThreadId -> Maybe TxStatus -> TxStatus -> TxLogs EarlyConflict -> Unique -> TxNotifies EarlyConflict -> IO ()
addTxNotifiesE rootThread old new txlogs uid notifies = {-# SCC addTxNotifiesE #-} do
	case diffTxStatus old new of
		Nothing -> return ()
		Just isWriteOrNewTrue -> modifyMVarMasked_' "addTxNotifiesE" notifies (return . Map.insertWith (||) rootThread isWriteOrNewTrue)
  where
	{-# INLINE diffTxStatus #-}
	diffTxStatus Nothing st2 = Just (isWriteOrNewTrue st2)
	diffTxStatus (Just st1) st2 = if (isWriteOrNewTrue st1 == isWriteOrNewTrue st2) then Nothing else Just (isWriteOrNewTrue st2)
	
{-# INLINE unTxNotifiesE #-}
unTxNotifiesE :: TxId EarlyConflict -> Unique -> TxNotifies EarlyConflict -> IO ()
unTxNotifiesE tid uid notifies = modifyMVarMasked_' "unTxNotifiesE" notifies (return . Map.delete tid)
	
instance TxConflictClass CommitConflict where
	
	addTxNotifies txid old new txlogs uid meta = return ()
	{-# INLINE addTxNotifies #-}
	newTxNotifies _ = return ()
	{-# INLINE newTxNotifies #-}
	unTxNotifies _ tid uid notifies = return ()
	{-# INLINE unTxNotifies #-}
	
	checkTxNotifies _ b meta _ = return $ RepairDynTxVarC $ Map.singleton meta b
	{-# INLINE checkTxNotifies #-}
	
	startTx _ = do
		t <- getCurrentTime
		addRunningTx t
		return t	
	{-# INLINE startTx #-}
	
	finishTx starttime writes@(txunmemo,RepairDynTxVarC txwrites) = do
		mbearliestTx <- modifyMVarMasked' "finishTx" runningTxs (\xs -> let xs' = List.delete starttime xs in return (xs',lastMay xs'))
		let addDone time m = if Map.null txwrites then m else Map.insert time writes m
		now <- case mbearliestTx of
			Just earliestTx -> modifyMVarMasked' "finishTx" doneTxs (\m -> getCurrentTime >>= \now -> let m' = Map.filterWithKey (\t _ -> t > earliestTx) (addDone now m) in m' `seq` return (m',now))
			Nothing -> modifyMVarMasked' "finishTx" doneTxs (\m -> getCurrentTime >>= \now -> let m' = addDone now m in m' `seq` return (m',now))
		debugTx' $ "["++show starttime ++ "] FINISHED as " ++ show now ++ " in " ++ show (diffUTCTime now starttime)  {- ++ show txvars -}
	{-# INLINE finishTx #-}
		
	exitTx = return () -- nothing to be done
	{-# INLINE exitTx #-}
		
	restartTxWithRepair (Just (newtime :!: TxRepair repair)) msg starttime m = do
		unsafeIOToInc $ updateRunningTx starttime newtime
		restartTx newtime $ debugTx msg >> repair >> m
	restartTxWithRepair Nothing msg starttime m = do
		unsafeIOToInc $ deleteRunningTx starttime
		resetTx $ debugTx msg >> m
	{-# INLINE restartTxWithRepair #-}
	
	validateTxs = validateTxsCommit
	{-# INLINE validateTxs #-}
	
	appendInvalidTxRepair r1 (t :!: r2) = (t :!: mappend r1 r2)
	{-# INLINE appendInvalidTxRepair #-}
	extendInvalidTxRepair (t :!: r1) r2 = (t :!: mappend r1 r2)
	{-# INLINE extendInvalidTxRepair #-}
	
	exceptionalFutureTx rootThread thread (TxFuture result) e = do
		txenv <- Reader.ask
		lift $ putMVar "exceptionalFutureTx" result (Left (toException e) :!: thread :!: txenv)
	

instance (TxLayer Inside c,TxLayer Outside c,Incremental (TxAdapton c)) => Transactional (TxAdapton c) where
	readAtomicallyWithParams params = atomicallyTx params ""
	retry = retryTx
	orElse = orElseTx

instance (TxLayer Inside c,TxLayer Outside c,Incremental (TxAdapton c)) => MonadThrow (Outside (TxAdapton c)) where 
	throwM = throwTx

instance (TxLayer Inside c,TxLayer Outside c,Incremental (TxAdapton c)) => MonadCatch (Outside (TxAdapton c)) where 
	catch = catchTx
	
instance (TxLayer Inside c,TxLayer Outside c) => Promising (Outside (TxAdapton c)) where
	
	type Promise (Outside (TxAdapton c)) = STxMVar (TxM Versioned c) (TxAdapton c)
	type PromiseK (Outside (TxAdapton c)) a = STxMVarK (TxM Versioned c) (TxAdapton c) a
	
	promise = newEmptySTxMVar
	deliver p v = putSTxMVar p v
	claim p = readSTxMVar p

instance (TxLayer Inside c,TxLayer Outside c) => Futuristic (Outside (TxAdapton c)) where
	
	type Future (Outside (TxAdapton c)) = TxFuture c
	type FutureK (Outside (TxAdapton c)) a = ()
	
	future = futureTx
	join = joinTx

	

