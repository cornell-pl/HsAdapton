{-# LANGUAGE ImpredicativeTypes, ConstraintKinds, GADTs, MagicHash, RecursiveDo, BangPatterns, UndecidableInstances, FlexibleInstances, ScopedTypeVariables, FlexibleContexts, KindSignatures, MultiParamTypeClasses #-}

-- | Adapton-style (http://dl.acm.org/citation.cfm?id=2594324) incremental computations.
-- Adapton currently requires the monad to support IO, for efficiency but mostly because our implementation relies heavily on @Unique@ identifiers and @Weak@ references.
-- This exposes more than would be desirable to end-users, but it does not seem easy to overcome this limitation.

module Control.Monad.Incremental.Internal.Adapton.Algorithm where

import Control.Monad.Incremental
import Control.Monad.Incremental.Internal.Adapton.Layers
import Control.Monad.Incremental.Internal.Adapton.Types
import Control.Monad.Incremental.Internal.Adapton.Memo
import Control.Monad
import qualified Data.Strict.Maybe as Strict
import Debug
import Data.Unique
import System.Mem.Weak.Exts
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified System.Mem.MemoTable as MemoTable

import Data.IORef.Exts
import Data.IORef
import Data.Strict.Tuple (Pair(..))
import qualified Data.Strict.List as SList
import Data.Proxy
import Data.WithClass.MData
import Prelude hiding (mod,const,read)
import System.Mem.WeakMap (WeakMap(..))
import qualified System.Mem.WeakMap as WeakMap

-- * Strict modifiables

instance (Layer Inside Adapton) => Thunk M Inside Adapton where
	new = modInnerM
	{-# INLINE new #-}
	newc = refInnerM
	{-# INLINE newc #-}
	read = getInnerM
	{-# INLINE read #-}

instance (Layer Outside Adapton) => Thunk M Outside Adapton where
	new = modOuterM
	{-# INLINE new #-}
	newc = refOuterM
	{-# INLINE newc #-}
	read = getOuterM
	{-# INLINE read #-}

instance (Layer Inside Adapton) => Input M Inside Adapton where
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

instance (Layer Outside Adapton) => Input M Outside Adapton where
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

modInnerM :: (AdaptonImpl inc,IncK inc a,Layer Inside inc) => Inside inc a -> Inside inc (M Inside inc a)
modInnerM m = m >>= refInnerM

modOuterM :: (AdaptonImpl inc,IncK inc a,Layer Outside inc) => Outside inc a -> Outside inc (M Outside inc a)
modOuterM m = m >>= refOuterM

refOuterM :: (IncK inc a,Layer l inc,Layer Outside inc) => a -> Outside inc (M l inc a)
refOuterM v = unsafeIOToInc $ do
	idU <- newUnique
	dta <- newIORef v
	dependentsU <- WeakMap.new
	-- since the ref will never be reused, we don't need to worry about it's creator
	return $ M (dta,(NodeMeta (idU,dependentsU,error "nodirty",return (),Nothing)))

refInnerM :: (AdaptonImpl inc,IncK inc a,Layer l inc,Layer Inside inc) => a -> Inside inc (M l inc a)
refInnerM v = unsafeIOToInc $ do
	idU <- newUnique
	dta <- newIORef v
	dependentsU <- WeakMap.new
	-- add a reference dependency (they are transitive up to the top-level calling thunk)
	creator <- mkRefCreator idU
	return $ M (dta,(NodeMeta (idU,dependentsU,error "nodirty",return (),creator)))

-- forces a lazy modifiable (encoded as a plain thunk)
-- the layer is just for uniformity, but it does not matter for @M@
{-# INLINE getInnerM #-}
getInnerM :: (AdaptonImpl inc,Eq a,IncK inc a,Layer Inside inc) => M Inside inc a -> Inside inc a
getInnerM = \t -> {-debug ("getInnerM " ++ show (hashUnique $ idNM $ metaM t)) $ -} unsafeIOToInc $  do
	value <- readIORef (dataM t)
	addDependency (metaM t) (unsafeIOToInc $ checkM t $! value) -- updates dependencies of callers
	return value

-- forces a lazy modifiable (encoded as a plain thunk)
-- the layer is just for uniformity, but it does not matter for @M@
-- if running at the outer layer, we don't need to add dependencies since thunks are always re-evaluated
{-# INLINE getOuterM #-}	
getOuterM :: (Layer l inc) => M l inc a -> Outside inc a
getOuterM = \t -> unsafeIOToInc $ readIORef (dataM t)

-- force that does not return the value nor adds dependencies, but instead checks whether the value has not changed
{-# INLINE checkM #-}
checkM :: (Eq a,IncK inc a,Layer Inside inc) => M Inside inc a -> a -> IO Bool
checkM = \t oldv -> do
	v <- readIORef' (dataM t)
	return (oldv == v)

setM  :: (Eq a,IncK inc a,Layer Outside inc,Layer l inc) => M l inc a -> a -> Outside inc ()
setM t v' = debug ("changed " ++ show (hashUnique $ idNM $ metaM t)) $ unsafeIOToInc $ do
	v <- readIORef' (dataM t)
	unless (v == v') $ do
		writeIORef' (dataM t) v'
		dirty (metaM t) -- dirties only dependents; dirty also parent dependencies (to avoid reuse for thunks that created this reference)

-- * Lazy modifiables

instance (Layer Inside Adapton) => Thunk L Inside Adapton where
	new = modL
	{-# INLINE new #-}
	newc = refL
	{-# INLINE newc #-}
	read = getInnerL
	{-# INLINE read #-}

instance (Layer Outside Adapton) => Thunk L Outside Adapton where
	new = modL
	{-# INLINE new #-}
	newc = refL
	{-# INLINE newc #-}
	read = getOuterL
	{-# INLINE read #-}

instance (Layer Inside Adapton) => Input L Inside Adapton where
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

instance (Layer Outside Adapton) => Input L Outside Adapton where
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

refL :: (AdaptonImpl inc,Layer l inc) => a -> l inc (L l inc a)
refL v = unsafeIOToInc $ do
	idU <- newUnique
	dta <- newIORef (LConst v) 
	dependentsU <- WeakMap.new 
	creator <- mkRefCreator idU
	return $ L (dta,(NodeMeta (idU,dependentsU,error "nodirty",return (),creator)))

modL :: (AdaptonImpl inc,Layer l inc) => l inc a -> l inc (L l inc a)
modL m = unsafeIOToInc $ do
	idU <- newUnique
	dta <- newIORef (LThunk m)
	dependentsU <- WeakMap.new
	creator <- mkRefCreator idU
	return $ L (dta,(NodeMeta (idU,dependentsU,error "nodirty",return (),creator)))

-- forces a lazy modifiable (encoded as a plain thunk)
{-# INLINE getInnerL #-}
getInnerL :: (AdaptonImpl inc,Eq a,IncK inc a,Layer Inside inc) => L Inside inc a -> Inside inc a
getInnerL = \t -> {-debug ("getInnerL " ++ show (hashUnique $ idNM $ metaL t)) $ -} do
	value <- getNoDependentsInnerL t
	unsafeIOToInc $ addDependency (metaL t) (unsafeIOToInc $ checkL t $! value) -- updates dependencies of callers
	return value

-- forces a lazy modifiable (encoded as a plain thunk)
{-# INLINE getOuterL #-}
getOuterL :: (AdaptonImpl inc,IncK inc a,Layer Outside inc) => L Outside inc a -> Outside inc a
getOuterL = error "getting a lazy modifiable outside" --getNoDependentsOuterL

-- force that does not return the value nor adds dependencies, but instead checks whether the value has not changed
{-# INLINE checkL #-}
checkL :: (AdaptonImpl inc,Eq a,IncK inc a,Layer Inside inc) => L Inside inc a -> a -> IO Bool
checkL = \t oldv -> do
	d <- readIORef (dataL t)
	case d of
		LThunk _ -> return False
		LConst value -> return (oldv == value)

{-# INLINE getNoDependentsInnerL #-}
getNoDependentsInnerL :: (AdaptonImpl inc,Layer Inside inc) => L Inside inc a -> Inside inc a
getNoDependentsInnerL = \t -> {-debug ("getNoDependentsInnerL " ++ show (hashUnique $ idNM $ metaL t)) $ -} do
	d <- unsafeIOToInc $ readIORef (dataL t)
	case d of
		LThunk force -> evaluateInnerL t force --unevaluated thunk
		LConst value -> return value -- constant value

{-# INLINE getNoDependentsOuterL #-}
getNoDependentsOuterL :: (AdaptonImpl inc,Eq a,Layer Outside inc) => L Outside inc a -> Outside inc a
getNoDependentsOuterL = \t -> do
	d <- unsafeIOToInc $ readIORef (dataL t)
	case d of
		LThunk force -> evaluateOuterL t force --unevaluated thunk
		LConst value -> return value -- constant value

-- does not add the modifiable to the stack, as we do with thunks
{-# INLINE evaluateInnerL #-}
evaluateInnerL :: (AdaptonImpl inc,Layer Inside inc) => L Inside inc a -> Inside inc a -> Inside inc a
evaluateInnerL t force = debug ("re-evaluatingInnerL " ++ show (hashUnique $ idNM $ metaL t)) $ do
	unsafeIOToInc $ pushStack (metaL t :!: Strict.Nothing)
	value <- force
	unsafeIOToInc $ writeIORef (dataL t) $! LConst value
	popStack'
	return value

-- does not add the modifiable to the stack, as we do with thunks
-- does not store the result in the modifiable
{-# INLINE evaluateOuterL #-}
evaluateOuterL :: (AdaptonImpl inc,Eq a,Layer Outside inc) => L Outside inc a -> Outside inc a -> Outside inc a
evaluateOuterL t force = debug ("re-evaluatingOuterL " ++ show (hashUnique $ idNM $ metaL t)) $ do
	unsafeIOToInc $ pushStack (metaL t :!: Strict.Nothing)
	value <- force
	popStack'
	return value

setL :: (Eq a,IncK inc a,Layer Outside inc,Layer l inc) => L l inc a -> a -> Outside inc ()
setL l v' = debug ("changed " ++ show (hashUnique $ idNM $ metaL l)) $ unsafeIOToInc $ do
	d <- readIORef (dataL l)
	let value_changed = do
		writeIORef (dataL l) $! LConst v'
		dirty (metaL l) -- dirties only dependents
	case d of
		LThunk m -> value_changed
		LConst v -> if v==v'
			then return ()
			else value_changed

-- changes the value lazily, so it cannot perform equality
overwriteL :: (Layer l inc) => L l inc a -> l inc a -> Outside inc ()
overwriteL t m = unsafeIOToInc $ do
	d <- readIORef (dataL t)
	writeIORef (dataL t) $! case d of
		LThunk _ -> LThunk m
		LConst _ -> LThunk m
	dirty (metaL t) -- dirties only dependents

-- appends a change to the chain of pending changes
modifyL :: (Layer l inc) => L l inc a -> (a -> l inc a) -> Outside inc ()
modifyL t f = unsafeIOToInc $ do
	d <- readIORef (dataL t)
	writeIORef (dataL t) $! case d of
		LThunk force -> LThunk $ force >>= f -- if it has never been evaluated, it remains so
		LConst value -> LThunk $ f value
	dirty (metaL t) -- dirties only dependents

-- | Tests if a lazy modifiable has been evaluated
isUnevaluatedL :: (Layer l inc) => L l1 inc a -> l inc Bool
isUnevaluatedL t = do
	d <- unsafeIOToInc $ readIORef (dataL t)
	case d of
		LThunk force -> return True --unevaluated thunk
		LConst value -> return False -- constant value

-- * Thunks

instance (Layer Inside Adapton) => Thunk U Inside Adapton where
	new = thunkU
	{-# INLINE new #-}
	newc = constU
	{-# INLINE newc #-}
	read = forceInnerU
	{-# INLINE read #-}

instance (Layer Outside Adapton) => Thunk U Outside Adapton where
	new = thunkU
	{-# INLINE new #-}
	newc = constU
	{-# INLINE newc #-}
	read = forceOuterU
	{-# INLINE read #-}

-- no memoization at the outer layer
instance (Layer Outside Adapton) => Output U Outside Adapton where
	thunk = thunkU
	{-# INLINE thunk #-}
	const = constU
	{-# INLINE const #-}
	force = forceOuterU
	{-# INLINE force #-}
	forceOutside = forceOuterU
	{-# INLINE forceOutside #-}

instance (Layer Inside Adapton) => Output U Inside Adapton where
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
	memoAs = memoUAs
	{-# INLINE memoAs #-}
	gmemoQ = gmemoQU
	{-# INLINE gmemoQ #-}
	gmemoQAs = gmemoQUAs
	{-# INLINE gmemoQAs #-}

-- | Creates a new thunk
thunkU :: (Layer l inc,Layer l1 inc) => l1 inc a -> l inc (U l1 inc a)
thunkU c = unsafeIOToInc $ do
	idU <- newUnique
	dta <- newIORef (Thunk c)
	dependentsU <- WeakMap.new
--	wdta <- mkWeakRefKey dta dta Nothing -- we use a weak pointer to avoid keeping the thunk alive due to its metadata
	debug ("thunkU "++show idU) $ return $ U (dta,(NodeMeta (idU,dependentsU,dirtyValue' dta,forgetUData' dta,Nothing)))

constU :: (Layer l inc,Layer l1 inc) => a -> l inc (U l1 inc a)
constU v = unsafeIOToInc $ do
	idU <- newUnique
	dta <- newIORef (Const v)
--	wdta <- mkWeakRefKey dta dta Nothing -- we use a weak pointer to avoid keeping the thunk alive due to its metadata
	--debug ("constU "++show idU) $
	return $ U (dta,(NodeMeta (idU,error "no dependents",error "no dirty",forgetUData' dta,Nothing)))

-- | Force a computation without change propagation
forceOuterU :: (AdaptonImpl inc,IncK inc a,Layer Outside inc) => U Outside inc a -> Outside inc a
forceOuterU = \t -> do
	d <- unsafeIOToInc $ readIORef (dataU t)
	case d of
		Thunk force -> force
		Const value -> return value
		otherwise -> error "forceOuterU stores no IC data"

-- | Force a computation with change propagation
-- NOTE: if we allow @U@ thunks to be modified, then this constant optimization is unsound!
forceInnerU :: (AdaptonImpl inc,Eq a,IncK inc a,Layer Inside inc) => U Inside inc a -> Inside inc a
forceInnerU = \t -> {-debug ("forceInnerU " ++ show (hashUnique $ idU t)) $ -} do
	value <- forceNoDependentsU t
	has <- hasDependenciesU t -- for the case when a thunk is actually a constant computation (what arises frequently in generic code...), we don't need to record dependencies
	if has
		then unsafeIOToInc $ addDependency (metaU t) (checkU t $! value) -- updates dependencies of callers
		else unsafeIOToInc $ writeIORef (dataU t) $! Const value -- make it an actual constant
	return value

-- tests if a thunk has no dependencies
hasDependenciesU :: Layer Inside inc => U Inside inc a -> Inside inc Bool
hasDependenciesU t = do
	d <- unsafeIOToInc $ readIORef (dataU t)
	case d of
		Value _ value force dependencies -> liftM (not . null) $ unsafeIOToInc $ readIORef dependencies
		Thunk force -> error "cannot test dependencies of unevaluated thunk"
		Const value -> return False -- constant value

oldvalueU :: (Layer l inc,Layer l1 inc) => U l1 inc a -> l inc a
oldvalueU t = do
	d <- unsafeIOToInc $ readIORef (dataU t)
	case d of
		Value dirty value force dependencies -> return value
		Thunk force -> error "no old value available"
		Const value -> return value

-- force that does not record any dependency on other thunks, e.g., (internally) when only displaying the contents or when only repairing
{-# INLINE forceNoDependentsU #-}
forceNoDependentsU :: (AdaptonImpl inc,IncK inc a,Layer Inside inc) => U Inside inc a -> Inside inc a
forceNoDependentsU = \t -> {-debug ("forceNoDependentsU "++show (idNM $ metaU t)) $ -} do
	d <- unsafeIOToInc $ readIORef (dataU t)
	case d of
		Value 0# value force dependencies -> return value -- is not dirty
		Value 1# value force dependencies -> repairInnerU t value force dependencies -- is dirty
		Thunk force -> unsafeIOToInc (newIORef []) >>= evaluateInnerU t force --unevaluated thunk
		Const value -> return value -- constant value

-- force that does not return the value nor adds dependencies, but instead checks whether the value has not changed
checkU :: (AdaptonImpl inc,Eq a,IncK inc a,Layer Inside inc) => U Inside inc a -> a -> Inside inc Bool
checkU t oldv = do
	d <- unsafeIOToInc $ readIORef (dataU t)
	case d of
		Value 0# value force dependencies -> return (oldv==value)
		Value 1# value force dependencies -> liftM (oldv ==) (repairInnerU t value force dependencies)
		Thunk _ -> return False -- if the thunk has never been computed
		Const value -> return False -- given that @U@ thunks cannot be mutated, the value for constants cannot change

-- used to avoid forcing the current node if none of the dependencies changes
repairInnerU :: (AdaptonImpl inc,Layer Inside inc) => U Inside inc a -> a -> Inside inc a -> IORef (Dependencies inc) -> Inside inc a
repairInnerU t value force dependencies = {-# SCC repairInnerU #-} debug ("repairing thunk "++ show (hashUnique $ idNM $ metaU t)) $
		unsafeIOToInc (readIORef dependencies) >>= foldr repair' norepair' . reverse --we need to reverse the dependency list to respect evaluation order
	where
	{-# INLINE norepair' #-}
	norepair' = unsafeIOToInc (writeDirtyValue (dataU t) 0#) >> return value -- if no dependency is dirty, simply return its value
	{-# INLINE repair' #-}
	repair' (Dependency (srcMetaW,dirtyW,checkW,tgtMetaW),_) m = do
		isDirty <- unsafeIOToInc $ readIORef dirtyW
		if isDirty
			then do
				unsafeIOToInc $ writeIORef' dirtyW False -- undirty the dependency
				ok <- checkW -- checks if the dependency does not need to be re-evaluated (dependent not dirty or the new value is the same as the old one)
				if ok
					then debug ("dependency has not changed "++show (idNM $ srcMetaW) ++" "++show (idNM $ tgtMetaW)) m
					else debug ("dependency has changed"++show (idNM $ srcMetaW) ++" "++show (idNM $ tgtMetaW)) $ unsafeIOToInc (clearDependencies dependencies >> newIORef []) >>= evaluateInnerU t force -- we create a new dependencies reference to free all the old data that depends on the it
			else m

-- recomputes a node
-- does not clear the dependencies on its own
{-# INLINE evaluateInnerU #-}
evaluateInnerU :: (AdaptonImpl inc,Typeable inc,Layer Inside inc) => U Inside inc a -> Inside inc a -> IORef (Dependencies inc) -> Inside inc a
evaluateInnerU t force dependencies = {-# SCC evaluateInnerU #-} debug ("re-evaluatingInnerU " ++ show (hashUnique $ idU t)) $ do
	unsafeIOToInc $ pushStack (metaU t :!: Strict.Just dependencies)
	value <- force
	unsafeIOToInc $ writeIORef (dataU t) $! Value 0# value force dependencies
	popStack'
	return value

-- Nothing = unevaluated, Just True = dirty, Just False = not dirty
isDirtyUnevaluatedU :: (Layer l inc) => U l1 inc a -> l inc (Maybe Bool)
isDirtyUnevaluatedU t = do
	d <- unsafeIOToInc $ readIORef (dataU t)
	case d of
		Thunk force -> return Nothing --unevaluated thunk
		Const value -> return $ Just False -- constant value
		Value 1# value force dependencies -> return $ Just True -- dirty
		Value 0# value force dependencies -> return $ Just False -- dirty

isUnevaluatedU :: (Layer l inc) => U l1 inc a -> l inc Bool
isUnevaluatedU t = do
	d <- unsafeIOToInc $ readIORef (dataU t)
	case d of
		Thunk force -> return True --unevaluated thunk
		otherwise -> return False

-- | Explicit memoization for recursive functions, fixpoint
-- A fixed-point operation returning thunks that in the process of tying the knot adds memoization.
memoU :: (Typeable inc,Typeable a,Typeable arg,IncK inc a,Layer Outside inc,Memo arg) => ((arg -> Inside inc (U Inside inc a)) -> arg -> Inside inc a) -> (arg -> Inside inc (U Inside inc a))
memoU f = let memo_func = memoNonRecU (thunkU . f memo_func) in memo_func

memoUAs :: (Typeable inc,Memo name,Typeable a,Typeable arg,IncK inc a,Layer Outside inc,Memo arg) => name -> ((arg -> Inside inc (U Inside inc a)) -> arg -> Inside inc a) -> (arg -> Inside inc (U Inside inc a))
memoUAs name f = let memo_func = memoNonRecUAs name (thunkU . f memo_func) in memo_func

gmemoQU :: (Typeable inc,Typeable b,Typeable ctx,IncK inc b,Output U Inside inc) => Proxy ctx -> (GenericQMemoU ctx Inside inc b -> GenericQMemoU ctx Inside inc b) -> GenericQMemoU ctx Inside inc b
gmemoQU ctx (f :: (GenericQMemoU ctx Inside inc b -> GenericQMemoU ctx Inside inc b)) =
	let memo_func :: GenericQMemoU ctx Inside inc b
	    memo_func = gmemoNonRecU ctx (f memo_func)
	in memo_func

gmemoQUAs :: (Typeable inc,Memo name,Typeable b,Typeable ctx,IncK inc b,Output U Inside inc) => Proxy ctx -> name -> (GenericQMemoU ctx Inside inc b -> GenericQMemoU ctx Inside inc b) ->  GenericQMemoU ctx Inside inc b
gmemoQUAs ctx name (f :: (GenericQMemoU ctx Inside inc b -> GenericQMemoU ctx Inside inc b)) =
	let memo_func :: GenericQMemoU ctx Inside inc b
	    memo_func = gmemoNonRecUAs ctx name (f memo_func)
	in memo_func

-- * Auxiliary functions

{-# INLINE addDependency #-}
-- adds a bidirectional dependency on a thunk
addDependency :: (AdaptonImpl inc,Layer Inside inc) => NodeMeta inc -> Inside inc Bool -> IO ()
addDependency calleemeta check = do
	!top <- topThunkStack
	case top of
		Just (callermeta :!: Strict.Just callerdependencies) -> debug ("added BX dependency: "++show (hashUnique $ idNM calleemeta) ++ " -> " ++ show (hashUnique $ idNM callermeta)) $ do
			dirtyW <- newIORef False 
			let dependencyW = Dependency (calleemeta,dirtyW,check,callermeta)
			let weakset = dependentsNM calleemeta
			let callerid = idNM callermeta
			weak <- liftIO $ mkWeakRefKey callerdependencies dependencyW (Just $ WeakMap.deleteFinalized weakset callerid) -- the dependency lives as long as the dependencies reference lives, that in turn lives as long as the caller thunk itself lives
			modifyIORef' callerdependencies ((dependencyW,finalize weak):) 
			liftIO $ WeakMap.insertWeak weakset callerid weak
			
		otherwise -> debug ("nostack "++show (hashUnique $ idNM calleemeta)) $ return ()

-- | deletes all dependencies, by running their finalizers; this will also kill the "inverse" dependents
{-# INLINE clearDependencies #-}
clearDependencies :: IORef (Dependencies inc) -> IO ()
clearDependencies = \r -> readIORef r >>= mapM_ (liftIO . snd)

{-# INLINE writeDirtyValue #-}
writeDirtyValue :: IORef (UData l inc a) -> UBool -> IO ()
writeDirtyValue = \dta dirty -> modifyIORef' dta (\(Value _ value force dependencies) -> Value dirty value force dependencies)

{-# INLINE dirtyValue #-}
dirtyValue :: Weak (IORef (UData l inc a)) -> IO ()
dirtyValue = \wdta -> do
	mb <- liftIO $ deRefWeak wdta
	case mb of
		Just dta -> dirtyValue' dta
		Nothing -> return ()

dirtyValue' :: IORef (UData l inc a) -> IO ()
dirtyValue' dta = modifyIORef' dta (\(Value _ value force dependencies) -> Value 1# value force dependencies)

-- consider the example: do { t <- thunk (ref e); r <- force t; v <- get r; set r v'; r <- force t; v <- get r }
--we need to forget the previous execution of the thunk (and therefore ignore the set over the inner reference) so that execution is consistent with re-evaluating from scratch
-- XXX: we could refine the type system to prevent the modification of references created inside thunks, but that seems overcomplicated just to cut an edge case.
-- dirtying purges dead weak pointers
{-# INLINE dirty #-}
dirty :: NodeMeta inc -> IO ()
dirty = \umeta -> do
	dirtyCreator (creatorNM umeta) -- if we change a reference that was created inside some memoized thunk, we have to forget all the memoized data for the parent thunk
	dirtyRecursively (dependentsNM umeta)

-- does not remove dead dependencies
dirtyRecursively :: Dependents inc -> IO ()
dirtyRecursively deps = do
	WeakMap.mapM_ dirty' deps -- take the chance to purge eventually dead pointers from dependents to dependencies (since there is no ultimate guarantee that finalizers run)
	where
	{-# INLINE dirty' #-}
	dirty' :: (Unique,Dependent inc) -> IO ()
	dirty' = \(_,Dependency (srcMeta,dirty,check,tgtMeta)) -> do
		isDirty <- readIORef dirty
		unless isDirty $ do
			writeIORef dirty $! True -- dirty the dependency
			dirtyUM tgtMeta -- dirty the caller thunk
			dirtyRecursively (dependentsNM tgtMeta) -- dirty recursively

-- whenever a reference is changed, remove all the cached data of the parent thunk that created the reference, and dirty recursive thunks that depend on the parent thunk
dirtyCreator :: Maybe (Creator inc) -> IO ()
dirtyCreator Nothing = return ()
dirtyCreator (Just wcreator) = do
	mb <- liftIO $ deRefWeak wcreator
	case mb of
		Just creatorMeta -> do
			forgetUM creatorMeta
			dirtyRecursively (dependentsNM creatorMeta)
		Nothing -> return ()

-- forgets all the old cached data in a thunk
forgetUData :: Weak (IORef (UData l inc a)) -> IO ()
forgetUData wdta = do
	mb <- liftIO $ deRefWeak wdta
	case mb of
		Just dta -> forgetUData' dta
		Nothing -> return ()

forgetUData' :: IORef (UData l inc a) -> IO ()
forgetUData' dta = do
	d <- readIORef dta
	case d of
		Value dirty value force dependencies -> clearDependencies dependencies >> (writeIORef dta $! Thunk force)
		otherwise -> return ()

{-# INLINE mkRefCreator #-}
-- if the parent is a reference, we don't need to remember it because no dirtying will be necessary
mkRefCreator :: AdaptonImpl inc => Unique -> IO (Maybe (Creator inc))
mkRefCreator = \idU -> liftIO $ do
	top <- topStack
	case top of
		Just (callermeta :!: Strict.Just callerdependencies) -> do
			weak <- mkWeakRefKey callerdependencies callermeta Nothing -- the parent reference should live as long as the creator's dependencies
			{-debug (show (hashUnique idU) ++ "refparent " ++ show (hashUnique $ idNM callermeta)) $ -}
			return $ Just weak
		otherwise -> {-debug (show (hashUnique idU) ++ "refparent NONE") $ -}return Nothing

instance (IncK inc a,Layer l inc,Thunk M l inc,MData ctx (l inc) a
		, Sat (ctx (M l inc a)),DeepTypeable (M l inc a)
		) => MData ctx (l inc) (M l inc a) where
	gfoldl ctx k z t = z new >>= flip k (read t)
	gunfold ctx k z c = z new >>= k
	toConstr ctx m = dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Control.Monad.Adapton.M" [mkConstr ty "M" [] Prefix]

instance (IncK inc a,Layer l inc,Thunk L l inc,MData ctx (l inc) a
		, Sat (ctx (L l inc a)),DeepTypeable (L l inc a)
		) => MData ctx (l inc) (L l inc a) where
	gfoldl ctx k z t = z new >>= flip k (read t)
	gunfold ctx k z c = z new >>= k
	toConstr ctx m = dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Control.Monad.Adapton.L" [mkConstr ty "L" [] Prefix]

instance (IncK inc a,Layer l inc,Thunk U l inc,MData ctx (l inc) a
		, Sat (ctx (U l inc a)),DeepTypeable (U l inc a)
		) => MData ctx (l inc) (U l inc a) where
	gfoldl ctx k z t = z new >>= flip k (read t)
	gunfold ctx k z c = z new >>= k
	toConstr ctx m = dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Control.Monad.Adapton.U" [mkConstr ty "U" [] Prefix]
