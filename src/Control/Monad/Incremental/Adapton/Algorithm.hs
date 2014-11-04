{-# LANGUAGE ImpredicativeTypes, ConstraintKinds, GADTs, MagicHash, RecursiveDo, BangPatterns, UndecidableInstances, FlexibleInstances, ScopedTypeVariables, FlexibleContexts, KindSignatures, MultiParamTypeClasses #-}

-- | Adapton-style (http://dl.acm.org/citation.cfm?id=2594324) incremental computations.
-- Adapton currently requires the monad to support IO, for efficiency but mostly because our implementation relies heavily on @Unique@ identifiers and @Weak@ references.
-- This exposes more than would be desirable to end-users, but it does not seem easy to overcome this limitation.

module Control.Monad.Incremental.Adapton.Algorithm where

import Control.Monad.Incremental
import Control.Monad.Incremental.Adapton.Layers
import Control.Monad.Incremental.Adapton.Types
import Control.Monad.Incremental.Adapton.Memo
import Control.Monad
import Data.Strict.Maybe as Strict
import Debug
import Control.Monad.Ref
import Data.Unique
import System.Mem.Weak
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import System.Mem.MemoTable
import System.Mem.WeakRef
import Data.IORef
import qualified System.Mem.WeakSet as WeakSet
import Data.Strict.Tuple (Pair(..))
import Data.Strict.List (SList(..))
import qualified Data.Strict.List as SList
import Data.Proxy
import Data.WithClass.MData
import Prelude hiding (mod,const,read)

-- * Strict modifiables

instance (MonadIO m,Layer Inside inc r m) => Thunk M Inside inc r m where
	new = modInnerM
	{-# INLINE new #-}
	newc = refInnerM
	{-# INLINE newc #-}
	read = getInnerM
	{-# INLINE read #-}

instance (MonadIO m,Layer Outside inc r m) => Thunk M Outside inc r m where
	new = modOuterM
	{-# INLINE new #-}
	newc = refOuterM
	{-# INLINE newc #-}
	read = getOuterM
	{-# INLINE read #-}

instance (MonadIO m,Layer Inside inc r m) => Input M Inside inc r m where
	ref = refInnerM
	{-# INLINE ref #-}
	get = getInnerM
	{-# INLINE get #-}
	set = setM
	{-# INLINE set #-}
	getOutside = getOuterM
	{-# INLINE getOutside #-}
	refOutside = refOuterM
	{-# INLINE refOutside #-}
	modOutside = \c -> outside c >>= refOutside
	{-# INLINE modOutside #-}

instance (MonadIO m,Layer Outside inc r m) => Input M Outside inc r m where
	ref = refOuterM
	{-# INLINE ref #-}
	get = getOuterM
	{-# INLINE get #-}
	set = setM
	{-# INLINE set #-}
	refOutside = refOuterM
	{-# INLINE refOutside #-}
	modOutside = \c -> c >>= refOutside
	{-# INLINE modOutside #-}

modInnerM :: (Eq a,MonadIO m,Layer Inside inc r m) => Inside inc r m a -> Inside inc r m (M Inside inc r m a)
modInnerM m = m >>= refInnerM

modOuterM :: (Eq a,Layer Outside inc r m,MonadIO m) => Outside inc r m a -> Outside inc r m (M Outside inc r m a)
modOuterM m = m >>= refOuterM

refOuterM :: (Eq a,Layer l inc r m,MonadIO m,Layer Outside inc r m) => a -> Outside inc r m (M l inc r m a)
refOuterM v = inL $ do
	idU <- liftIO newUnique
	dta <- newRef v
	dependentsU <- liftIO $ WeakSet.new
	-- since the ref will never be reused, we don't need to worry about it's creator
	return $ M (dta,(NodeMeta (idU,dependentsU,error "nodirty",return (),Nothing,mkEmptyUDataOp)))

refInnerM :: (Eq a,Layer l inc r m,MonadIO m,Layer Inside inc r m) => a -> Inside inc r m (M l inc r m a)
refInnerM v = inL $ do
	idU <- liftIO newUnique
	dta <- newRef v
	dependentsU <- liftIO $ WeakSet.new
	-- add a reference dependency (they are transitive up to the top-level calling thunk)
	creator <- mkRefCreator idU
	return $ M (dta,(NodeMeta (idU,dependentsU,error "nodirty",return (),creator,mkEmptyUDataOp)))

-- forces a lazy modifiable (encoded as a plain thunk)
-- the layer is just for uniformity, but it does not matter for @M@
{-# INLINE getInnerM #-}
getInnerM :: (MonadIO m,Eq a,Layer Inside inc r m) => M Inside inc r m a -> Inside inc r m a
getInnerM = \t -> {-debug ("getInnerM " ++ show (hashUnique $ idNM $ metaM t)) $ -} inL $  do
	value <- readRef (dataM t)
	addDependency (metaM t) (inL $ checkM t $! value) -- updates dependencies of callers
	return value

-- forces a lazy modifiable (encoded as a plain thunk)
-- the layer is just for uniformity, but it does not matter for @M@
-- if running at the outer layer, we don't need to add dependencies since thunks are always re-evaluated
{-# INLINE getOuterM #-}	
getOuterM :: (MonadIO m,Layer l inc r m,InLayer Outside inc r m) => M l inc r m a -> Outside inc r m a
getOuterM = \t -> inL $ readRef (dataM t)

-- force that does not return the value nor adds dependencies, but instead checks whether the value has not changed
{-# INLINE checkM #-}
checkM :: (Eq a,Layer Inside inc r m,MonadIO m) => M Inside inc r m a -> a -> m Bool
checkM = \t oldv -> do
	v <- readRef (dataM t)
	return (oldv == v)

setM :: (Eq a,Layer Outside inc r m,MonadIO m,Layer l inc r m) => M l inc r m a -> a -> Outside inc r m ()
setM t v' = debug ("changed " ++ show (hashUnique $ idNM $ metaM t)) $ inL $ do
	v <- readRef (dataM t)
	unless (v == v') $ do
		writeRef (dataM t) $ v'
		dirty (metaM t) -- dirties only dependents; dirty also parent dependencies (to avoid reuse for thunks that created this reference)

-- * Lazy modifiables

instance (Layer Inside inc r m,MonadIO m) => Thunk L Inside inc r m where
	new = modL
	{-# INLINE new #-}
	newc = refL
	{-# INLINE newc #-}
	read = getInnerL
	{-# INLINE read #-}

instance (Layer Outside inc r m,MonadIO m) => Thunk L Outside inc r m where
	new = modL
	{-# INLINE new #-}
	newc = refL
	{-# INLINE newc #-}
	read = getOuterL
	{-# INLINE read #-}

instance (MonadIO m,Layer Inside inc r m,InLayer Outside inc r m) => Input L Inside inc r m where
	ref = refL
	{-# INLINE ref #-}
	mod = modL
	{-# INLINE mod #-}
	get = getInnerL
	{-# INLINE get #-}
	set = setL
	{-# INLINE set #-}
	overwrite = overwriteL
	{-# INLINE overwrite #-}
	modify = modifyL
	{-# INLINE modify #-}
	getOutside = inside . getNoDependentsInnerL
	{-# INLINE getOutside #-}

instance (MonadIO m,Layer Outside inc r m) => Input L Outside inc r m where
	ref = refL
	{-# INLINE ref #-}
	mod = modL
	{-# INLINE mod #-}
	get = getOuterL
	{-# INLINE get #-}
	set = setL
	{-# INLINE set #-}
	overwrite = overwriteL
	{-# INLINE overwrite #-}
	modify = modifyL
	{-# INLINE modify #-}

refL :: (Layer l inc r m,MonadIO m) => a -> l inc r m (L l inc r m a)
--refL :: (Layer l inc r m,MonadIO m) => a -> Outer r m (L l inc r m a)
refL v = inL $ do
	idU <- liftIO newUnique
	dta <- newRef (LConst 0# v) 
	dependentsU <- liftIO $ WeakSet.new 
	creator <- mkRefCreator idU
	return $ L (dta,(NodeMeta (idU,dependentsU,error "nodirty",return (),creator,mkEmptyUDataOp)))

modL :: (Layer l inc r m,MonadIO m) => l inc r m a -> l inc r m (L l inc r m a)
--modL :: (Layer l inc r m,MonadIO m) => l inc r m a -> Outer r m (L l inc r m a)
modL m = inL $ do
	idU <- liftIO newUnique
	dta <- newRef (LThunk 0# m)
	dependentsU <- liftIO $ WeakSet.new
	creator <- mkRefCreator idU
	return $ L (dta,(NodeMeta (idU,dependentsU,error "nodirty",return (),creator,mkEmptyUDataOp)))

-- forces a lazy modifiable (encoded as a plain thunk)
{-# INLINE getInnerL #-}
getInnerL :: (MonadIO m,Eq a,Layer Inside inc r m) => L Inside inc r m a -> Inside inc r m a
getInnerL = \t -> {-debug ("getInnerL " ++ show (hashUnique $ idNM $ metaL t)) $ -} do
	value <- getNoDependentsInnerL t
	inL $ addDependency (metaL t) (inL $ checkL t $! value) -- updates dependencies of callers
	return value

-- forces a lazy modifiable (encoded as a plain thunk)
{-# INLINE getOuterL #-}
getOuterL :: (MonadIO m,Eq a,Layer Outside inc r m) => L Outside inc r m a -> Outside inc r m a
getOuterL = error "getting a lazy modifiable outside" --getNoDependentsOuterL

-- force that does not return the value nor adds dependencies, but instead checks whether the value has not changed
{-# INLINE checkL #-}
checkL :: (Eq a,Layer Inside inc r m,MonadIO m) => L Inside inc r m a -> a -> m Bool
checkL = \t oldv -> do
	d <- readRef (dataL t)
	case d of
		LThunk _ _ -> return False
		LConst _ value -> return (oldv == value)

{-# INLINE getNoDependentsInnerL #-}
getNoDependentsInnerL :: (MonadIO m,Layer Inside inc r m) => L Inside inc r m a -> Inside inc r m a
getNoDependentsInnerL = \t -> {-debug ("getNoDependentsInnerL " ++ show (hashUnique $ idNM $ metaL t)) $ -} do
	d <- inL $ readRef (dataL t)
	case d of
		LThunk _ force -> evaluateInnerL t force --unevaluated thunk
		LConst _ value -> return value -- constant value

{-# INLINE getNoDependentsOuterL #-}
getNoDependentsOuterL :: (MonadIO m,Layer Outside inc r m) => L Outside inc r m a -> Outside inc r m a
getNoDependentsOuterL = \t -> do
	d <- inL $ readRef (dataL t)
	case d of
		LThunk _ force -> evaluateOuterL t force --unevaluated thunk
		LConst _ value -> return value -- constant value

-- does not add the modifiable to the stack, as we do with thunks
{-# INLINE evaluateInnerL #-}
evaluateInnerL :: (MonadIO m,Layer Inside inc r m) => L Inside inc r m a -> Inside inc r m a -> Inside inc r m a
evaluateInnerL t force = debug ("re-evaluatingInnerL " ++ show (hashUnique $ idNM $ metaL t)) $ do
	inL $ liftIO $ pushStack (metaL t :!: SNothing)
	value <- force
	inL $ writeRef (dataL t) $ LConst 1# value
	inL $ liftIO popStack
	return value

-- does not add the modifiable to the stack, as we do with thunks
-- does not store the result in the modifiable
{-# INLINE evaluateOuterL #-}
evaluateOuterL :: (MonadIO m,Layer Outside inc r m) => L Outside inc r m a -> Outside inc r m a -> Outside inc r m a
evaluateOuterL t force = debug ("re-evaluatingOuterL " ++ show (hashUnique $ idNM $ metaL t)) $ do
	inL $ liftIO $ pushStack (metaL t :!: SNothing)
	value <- force
	markForcedL t
	inL $ liftIO popStack
	return value

markForcedL :: (MonadIO m,Layer l inc r m) => L l inc r m a -> l inc r m ()
markForcedL t = inL $ do
	d <- readRef (dataL t)
	writeRef (dataL t) $ case d of
		LThunk _ force -> LThunk 1# force
		LConst _ value -> LConst 1# value

setL :: (Eq a,Layer Outside inc r m,MonadIO m,Layer l inc r m) => L l inc r m a -> a -> Outside inc r m ()
setL l v' = debug ("changed " ++ show (hashUnique $ idNM $ metaL l)) $ inL $ do
	d <- readRef (dataL l)
	let value_changed isForced = do
		writeRef (dataL l) $ LConst isForced v'
		dirty (metaL l) -- dirties only dependents
	case d of
		LThunk isForced m -> value_changed isForced
		LConst isForced v -> if v==v'
			then return ()
			else value_changed isForced

-- changes the value lazily, so it cannot perform equality
overwriteL :: (MonadIO m,Layer l inc r m,InLayer Outside inc r m) => L l inc r m a -> l inc r m a -> Outside inc r m ()
overwriteL t m = inL $ do
	d <- readRef (dataL t)
	writeRef (dataL t) $ case d of
		LThunk isForced _ -> LThunk isForced m
		LConst isForced _ -> LThunk isForced m
	dirty (metaL t) -- dirties only dependents

-- appends a change to the chain of pending changes
modifyL :: (MonadIO m,Layer l inc r m,InLayer Outside inc r m) => L l inc r m a -> (a -> l inc r m a) -> Outside inc r m ()
modifyL t f = inL $ do
	d <- readRef (dataL t)
	writeRef (dataL t) $ case d of
		LThunk isForced force -> LThunk isForced $ force >>= f -- if it has never been evaluated, it remains so
		LConst isForced value -> LThunk isForced $ f value
	dirty (metaL t) -- dirties only dependents

-- | Tests if a lazy modifiable has been evaluated
isUnevaluatedL :: (Layer l inc r m) => L l1 inc r m a -> l inc r m Bool
isUnevaluatedL t = do
	d <- inL $ readRef (dataL t)
	case d of
		LThunk _ force -> return True --unevaluated thunk
		LConst _ value -> return False -- constant value

-- | Tests if a lazy modifiable has ever been forced
-- If it has never been forced, then we know that its dependents do not need to be repaired
isUnforcedL :: (Layer l inc r m) => L l1 inc r m a -> l inc r m Bool
isUnforcedL t = do
	d <- inL $ readRef (dataL t)
	case d of
		LThunk 0# force -> return True
		LThunk 1# force -> return False
		LConst 0# value -> return True
		LConst 1# value -> return False

-- * Thunks

instance (MonadIO m,Layer Inside inc r m) => Thunk U Inside inc r m where
	new = thunkU
	{-# INLINE new #-}
	newc = constU
	{-# INLINE newc #-}
	read = forceInnerU
	{-# INLINE read #-}

instance (MonadIO m,Layer Outside inc r m) => Thunk U Outside inc r m where
	new = thunkU
	{-# INLINE new #-}
	newc = constU
	{-# INLINE newc #-}
	read = forceOuterU
	{-# INLINE read #-}

-- no memoization at the outer layer
instance (Layer Outside inc r m,MonadRef r m,WeakRef r,MonadIO m) => Output U Outside inc r m where
	thunk = thunkU
	{-# INLINE thunk #-}
	const = constU
	{-# INLINE const #-}
	force = forceOuterU
	{-# INLINE force #-}
	forceOutside = forceOuterU
	{-# INLINE forceOutside #-}

instance (Layer Inside inc r m,MonadRef r m,WeakRef r,MonadIO m) => Output U Inside inc r m where
	thunk = thunkU
	{-# INLINE thunk #-}
	const = constU
	{-# INLINE const #-}
	force = forceInnerU
	{-# INLINE force #-}
	forceOutside = world . forceNoDependentsU
	{-# INLINE forceOutside #-}
	memo = memoU
	{-# INLINE memo #-}
	gmemoQ = gmemoQU
	{-# INLINE gmemoQ #-}

-- | Creates a new thunk
thunkU :: (MonadIO m,Layer l inc r m,Layer l1 inc r m) => l1 inc r m a -> l inc r m (U l1 inc r m a)
thunkU c = inL $ do
	idU <- liftIO newUnique
	dta <- newRef (Thunk c)
	dependentsU <- liftIO $ WeakSet.new
	wdta <- liftIO $ mkWeakWithRefKey dta dta Nothing -- we use a weak pointer to avoid keeping the thunk alive due to its metadata
	debug ("thunkU "++show idU) $ return $ U (dta,(NodeMeta (idU,dependentsU,dirtyValue wdta,forgetUData wdta,Nothing,mkUDataOpWeak wdta)))

constU :: (MonadIO m,Layer l inc r m,Layer l1 inc r m) => a -> l inc r m (U l1 inc r m a)
constU v = inL $ do
	idU <- liftIO newUnique
	dta <- newRef (Const v)
	wdta <- liftIO $ mkWeakWithRefKey dta dta Nothing -- we use a weak pointer to avoid keeping the thunk alive due to its metadata
	--debug ("constU "++show idU) $
	return $ U (dta,(NodeMeta (idU,error "no dependents",error "no dirty",forgetUData wdta,Nothing,mkUDataOpWeak wdta)))

-- | Force a computation without change propagation
forceOuterU :: (MonadIO m,Eq a,Layer Outside inc r m) => U Outside inc r m a -> Outside inc r m a
forceOuterU = error "forceOuter"

-- | Force a computation with change propagation
-- NOTE: if we allow @U@ thunks to be modified, then this constant optimization is unsound!
forceInnerU :: (MonadIO m,Eq a,Layer Inside inc r m) => U Inside inc r m a -> Inside inc r m a
forceInnerU = \t -> {-debug ("forceInnerU " ++ show (hashUnique $ idU t)) $ -} do
	value <- forceNoDependentsU t
	has <- hasDependenciesU t -- for the case when a thunk is actually a constant computation (what arises frequently in generic code...), we don't need to record dependencies
	if has
		then inL $ addDependency (metaU t) (checkU t $! value) -- updates dependencies of callers
		else inL $ writeRef (dataU t) $ Const value -- make it an actual constant
	return value

-- tests if a thunk has no dependencies
hasDependenciesU :: Layer Inside inc r m => U Inside inc r m a -> Inside inc r m Bool
hasDependenciesU t = do
	d <- inL $ readRef (dataU t)
	case d of
		Value _ value force dependencies -> liftM (not . null) $ inL $ readRef dependencies
		Thunk force -> error "cannot test dependencies of unevaluated thunk"
		Const value -> return False -- constant value

oldvalueU :: (Layer l inc r m,Layer l1 inc r m) => U l1 inc r m a -> l inc r m a
oldvalueU t = do
	d <- inL $ readRef (dataU t)
	case d of
		Value dirty value force dependencies -> return value
		Thunk force -> error "no old value available"
		Const value -> return value

-- force that does not record any dependency on other thunks, e.g., (internally) when only displaying the contents or when only repairing
{-# INLINE forceNoDependentsU #-}
forceNoDependentsU :: (MonadIO m,Eq a,Layer Inside inc r m) => U Inside inc r m a -> Inside inc r m a
forceNoDependentsU = \t -> {-debug ("forceNoDependentsU "++show (idNM $ metaU t)) $ -} do
	d <- inL $ readRef (dataU t)
	case d of
		Value 0# value force dependencies -> return value -- is not dirty
		Value 1# value force dependencies -> repairInnerU t value force dependencies -- is dirty
		Thunk force -> inL (newRef []) >>= evaluateInnerU t force --unevaluated thunk
		Const value -> return value -- constant value

-- force that does not return the value nor adds dependencies, but instead checks whether the value has not changed
checkU :: (MonadIO m,Eq a,Layer Inside inc r m) => U Inside inc r m a -> a -> Inside inc r m Bool
checkU t oldv = do
	d <- inL $ readRef (dataU t)
	case d of
		Value 0# value force dependencies -> return (oldv==value)
		Value 1# value force dependencies -> liftM (oldv ==) (repairInnerU t value force dependencies)
		Thunk _ -> return False -- if the thunk has never been computed
		Const value -> return False --return (oldv == value) -- assuming that @U@ thunks cannot be mutated, the value for constants cannot change

-- used to avoid forcing the current node if none of the dependencies changes
repairInnerU :: (MonadIO m,Layer Inside inc r m) => U Inside inc r m a -> a -> Inside inc r m a -> r (Dependencies inc r m) -> Inside inc r m a
repairInnerU t value force dependencies = debug ("repairing thunk "++ show (hashUnique $ idNM $ metaU t)) $
		inL (readRef dependencies) >>= foldr repair' norepair' . reverse --we need to reverse the dependency list to respect evaluation order
	where
	{-# INLINE norepair' #-}
--	norepair' :: (MonadIO m,Layer Inside inc r m) => Inside inc r m a
	norepair' = inL (writeDirtyValue (dataU t) 0#) >> return value -- if no dependency is dirty, simply return its value
	{-# INLINE repair' #-}
--	repair' :: (MonadIO m,Layer Inside inc r m) => Dependency inc r m -> Inside inc r m a -> Inside inc r m a
	repair' (Dependency (srcMetaW,dirtyW,checkW,tgtMetaW),_) m = do
		isDirty <- inL $ readRef dirtyW
		if isDirty
			then do
				inL $ writeRef dirtyW False -- undirty the dependency
				ok <- checkW -- checks if the dependency does not need to be re-evaluated (not dirty or the new value is the same as the old one)
				if ok
					then debug ("dependency has not changed "++show (idNM $ srcMetaW) ++" "++show (idNM $ tgtMetaW)) m
					else debug ("dependency has changed"++show (idNM $ srcMetaW) ++" "++show (idNM $ tgtMetaW)) $ inL (clearDependencies dependencies >> newRef []) >>= evaluateInnerU t force -- we create a new dependencies reference to free all the old data that depends on the it
			else m

-- recomputes a node
-- does not clear the dependencies on its own
{-# INLINE evaluateInnerU #-}
evaluateInnerU :: (MonadIO m,Layer Inside inc r m) => U Inside inc r m a -> Inside inc r m a -> r (Dependencies inc r m) -> Inside inc r m a
evaluateInnerU t force dependencies = debug ("re-evaluatingInnerU " ++ show (hashUnique $ idU t)) $ do
	inL $ liftIO $ pushStack (metaU t :!: SJust dependencies)
	value <- force
	inL $ writeRef (dataU t) $ Value 0# value force dependencies
	inL $ liftIO popStack
	return value

-- Nothing = unevaluated, Just True = dirty, Just False = not dirty
isDirtyUnevaluatedU :: (Layer l inc r m) => U l1 inc r m a -> l inc r m (Maybe Bool)
isDirtyUnevaluatedU t = do
	d <- inL $ readRef (dataU t)
	case d of
		Thunk force -> return Nothing --unevaluated thunk
		Const value -> return $ Just False -- constant value
		Value 1# value force dependencies -> return $ Just True -- dirty
		Value 0# value force dependencies -> return $ Just False -- dirty

isUnevaluatedU :: (Layer l inc r m) => U l1 inc r m a -> l inc r m Bool
isUnevaluatedU t = do
	d <- inL $ readRef (dataU t)
	case d of
		Thunk force -> return True --unevaluated thunk
		otherwise -> return False

-- | Explicit memoization for recursive functions, fixpoint
-- Essentially, it is a fixed-point operation returning thunks but in the process of tying the knot it adds memoization.
memoU :: (MonadIO m,Eq a,Output U Inside inc r m,Memo arg) => ((arg -> Inside inc r m (U Inside inc r m a)) -> arg -> Inside inc r m a) -> (arg -> Inside inc r m (U Inside inc r m a))
memoU f = let memo_func = memoNonRecU (thunk . f memo_func) in memo_func

gmemoQU :: (Eq b,Output U Inside inc r m,MonadIO m) => Proxy ctx -> (GenericQMemoU ctx Inside inc r m b -> GenericQMemoU ctx Inside inc r m b) -> GenericQMemoU ctx Inside inc r m b
gmemoQU ctx (f :: (GenericQMemoU ctx Inside inc r m b -> GenericQMemoU ctx Inside inc r m b)) =
	let memo_func :: GenericQMemoU ctx Inside inc r m b
	    memo_func = gmemoNonRecU ctx (f memo_func)
	in memo_func

-- * Auxiliary functions

{-# INLINE addDependency #-}
-- adds a bidirectional dependency on a thunk
addDependency :: (MonadIO m,MonadRef r m,WeakRef r,Layer Inside inc r m) => NodeMeta inc r m -> Inside inc r m Bool -> m ()
addDependency calleemeta check = do
	top <- liftIO topThunkStack
	case top of
		Just (callermeta :!: SJust callerdependencies) -> debug ("added BX dependency: "++show (hashUnique $ idNM calleemeta) ++ " -> " ++ show (hashUnique $ idNM callermeta)) $ do
			dirtyW <- newRef False 
			let dependencyW = Dependency (calleemeta,dirtyW,check,callermeta)
			let weakset = dependentsNM calleemeta
			weak <- liftIO $ mkWeakWithRefKey callerdependencies dependencyW (Just $ WeakSet.purge weakset) -- the dependency lives as long as the dependencies reference lives, that in turn lives as long as the caller thunk itself lives
			mapRef ((dependencyW,finalize weak):) callerdependencies
			liftIO $ WeakSet.insertWeak weakset weak
		otherwise -> debug ("nostack "++show (hashUnique $ idNM calleemeta)) $ return ()

-- | deletes all dependencies, by running their finalizers; this will also kill the "inverse" dependents
{-# INLINE clearDependencies #-}
clearDependencies :: (MonadRef r m,MonadIO m) => r (Dependencies inc r m) -> m ()
clearDependencies = \r -> readRef r >>= mapM_ (liftIO . snd)

{-# INLINE writeDirtyValue #-}
writeDirtyValue :: MonadRef r m => r (UData l inc r m a) -> UBool -> m ()
writeDirtyValue = \dta dirty -> mapRef (\(Value _ value force dependencies) -> Value dirty value force dependencies) dta

{-# INLINE dirtyValue #-}
dirtyValue :: (MonadRef r m,MonadIO m) => Weak (r (UData l inc r m a)) -> m ()
dirtyValue = \wdta -> do
	mb <- liftIO $ deRefWeak wdta
	case mb of
		Just dta -> dirtyValue' dta
		Nothing -> return ()

dirtyValue' :: (MonadRef r m,MonadIO m) => r (UData l inc r m a) -> m ()
dirtyValue' dta = mapRef (\(Value _ value force dependencies) -> Value 1# value force dependencies) dta

-- consider the example: do { t <- thunk (ref e); r <- force t; v <- get r; set r v'; r <- force t; v <- get r }
--we need to forget the previous execution of the thunk (and therefore ignore the set over the inner reference) so that execution is consistent with re-evaluating from scratch
-- XXX: we could refine the type system to prevent the modification of references created inside thunks, but that seems overcomplicated just to cut an edge case.
-- dirtying purges dead weak pointers
{-# INLINE dirty #-}
dirty :: (MonadIO m,MonadRef r m) => NodeMeta inc r m -> m ()
dirty = \umeta -> do
	dirtyCreator (creatorNM umeta) -- if we change a reference that was created inside some memoized thunk, we have to forget all the memoized data for the parent thunk
	dirtyRecursively (dependentsNM umeta)

-- does not remove dead dependencies
dirtyRecursively :: (MonadIO m,MonadRef r m) => Dependents inc r m -> m ()
dirtyRecursively deps = do
	WeakSet.mapPurgeM_ dirty' deps -- take the chance to purge eventually dead pointers from dependents to dependencies (since there is no ultimate guarantee that finalizers run)
	where
	{-# INLINE dirty' #-}
	dirty' :: (MonadIO m,MonadRef r m) => Dependent inc r m -> m ()
	dirty' = \(Dependency (srcMeta,dirty,check,tgtMeta)) -> do
		isDirty <- readRef dirty
		unless isDirty $ do
			writeRef dirty True -- dirty the dependency
			dirtyUM tgtMeta -- dirty the caller thunk
			dirtyRecursively (dependentsNM tgtMeta) -- dirty recursively

-- whenever a reference is changed, remove all the cached data of the parent thunk that created the reference, and dirty recursive thunks that depend on the parent thunk
dirtyCreator :: (MonadIO m,MonadRef r m) => Maybe (Creator inc r m) -> m ()
dirtyCreator Nothing = return ()
dirtyCreator (Just wcreator) = do
	mb <- liftIO $ deRefWeak wcreator
	case mb of
		Just creatorMeta -> do
			forgetUM creatorMeta
			dirtyRecursively (dependentsNM creatorMeta)
		Nothing -> return ()

-- forgets all the old cached data in a thunk
forgetUData :: (MonadRef r m,MonadIO m) => Weak (r (UData l inc r m a)) -> m ()
forgetUData wdta = do
	mb <- liftIO $ deRefWeak wdta
	case mb of
		Just dta -> forgetUData' dta
		Nothing -> return ()

forgetUData' :: (MonadRef r m,MonadIO m) => r (UData l inc r m a) -> m ()
forgetUData' dta = do
	d <- readRef dta
	case d of
		Value dirty value force dependencies -> clearDependencies dependencies >> writeRef dta (Thunk force)
		otherwise -> return ()

{-# INLINE mkRefCreator #-}
-- if the parent is a reference, we don't need to remember it because no dirtying will be necessary
mkRefCreator :: (WeakRef r,MonadIO m) => Unique -> m (Maybe (Creator inc r m))
mkRefCreator = \idU -> liftIO $ do
	top <- topStack
	case top of
		Just (callermeta :!: SJust callerdependencies) -> do
			weak <- mkWeakWithRefKey callerdependencies callermeta Nothing -- the parent reference should live as long as the creator's dependencies
			{-debug (show (hashUnique idU) ++ "refparent " ++ show (hashUnique $ idNM callermeta)) $ -}
			return $ Just weak
		otherwise -> {-debug (show (hashUnique idU) ++ "refparent NONE") $ -}return Nothing

instance (Eq a,Layer l inc r m,Thunk M l inc r m,MData ctx (l inc r m) a
		, Sat (ctx (M l inc r m a)),DeepTypeable (M l inc r m a)
		) => MData ctx (l inc r m) (M l inc r m a) where
	gfoldl ctx k z t = z new >>= flip k (read t)
	gunfold ctx k z c = z new >>= k
	toConstr ctx m = dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Control.Monad.Adapton.M" [mkConstr ty "M" [] Prefix]

instance (Eq a,Layer l inc r m,Thunk L l inc r m,MData ctx (l inc r m) a
		, Sat (ctx (L l inc r m a)),DeepTypeable (L l inc r m a)
		) => MData ctx (l inc r m) (L l inc r m a) where
	gfoldl ctx k z t = z new >>= flip k (read t)
	gunfold ctx k z c = z new >>= k
	toConstr ctx m = dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Control.Monad.Adapton.L" [mkConstr ty "L" [] Prefix]

instance (Eq a,Layer l inc r m,Thunk U l inc r m,MData ctx (l inc r m) a
		, Sat (ctx (U l inc r m a)),DeepTypeable (U l inc r m a)
		) => MData ctx (l inc r m) (U l inc r m a) where
	gfoldl ctx k z t = z new >>= flip k (read t)
	gunfold ctx k z c = z new >>= k
	toConstr ctx m = dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Control.Monad.Adapton.U" [mkConstr ty "U" [] Prefix]
