{-# LANGUAGE RankNTypes, TypeOperators, TypeFamilies, FlexibleContexts, UndecidableInstances, ScopedTypeVariables, DeriveDataTypeable, KindSignatures, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, GADTs #-}

module Control.Monad.BiAdapton where

import Control.Monad.BiAdapton.DataTypes
import Data.Typeable
import System.Mem.MemoTable
import Data.Maybe
import Control.Monad.Ref
import Data.IntMap (IntMap(..))
import qualified Data.IntMap as IntMap
import Data.Map (Map(..))
import qualified Data.Map as Map
import Control.Monad.State as State
import Data.Traversable as Traversable

import Debug.Trace

-- | Run a bidirectional Adapton computation
run :: Monad m => Outer m r a -> m a
run (Outer comp) = do
	(x,_,_) <- comp 0 Nothing
	return x

class (UniqueRef (l m r) (IdRef r),Typeable m,Typeable l,Typeable r,InL l,Ref (l m r) r,Ref m r) => Layer l m r where
	inner :: Inner m r a -> l m r a
	-- | outer is only to be used internally
	outer :: Outer m r a -> l m r a
	force :: (Typeable a,Memo a) => Direction -> U m r a -> l m r a
	thunk :: (Typeable a,Memo a) => Direction -> Inner m r a -> l m r (U m r a)
	thunk = thunk'
	-- | Bidirectional primitive
	rethunk :: (Typeable a,Memo a) => Direction -> U m r a -> Inner m r a -> l m r (U m r a)
	rethunk = rethunk'

instance (Typeable r,Typeable m,Ref m r,Ref (Inner m r) r) => Layer Inner m r where
	inner = id
	outer (Outer m) = Inner m
	force = forceInner

instance (Typeable r,Typeable m,Ref m r,Ref (Outer m r) r) => Layer Outer m r where
	inner (Inner m) = Outer m
	outer = id
	force = forceOuter

memo :: (Typeable a,Memo a,Layer Inner m r,Memo arg) => Direction -> ((arg -> Inner m r (U m r a)) -> arg -> Inner m r a) -> (arg -> Inner m r (U m r a))
memo dir f arg = let memo_func = memoLazyM (thunk dir . f memo_func) in memo_func arg -- it is important to apply the function to the argument here, otherwise new memoized functions are created for new runs

-- | change at the outer layer
set :: (Layer Inner m r,Layer Outer m r,Typeable a,Memo a) => U m r a -> a -> Outer m r ()
set t v' = do
	mbdir <- undefinedDirection t
	olddir <- readRef (directionU t)
	let dir = maybe (opposite olddir) id mbdir
--	inner $ forceOpposite (opposite dir) t
	inner $ force (opposite dir) t -- this value will be ovewritten anyway, but we can't discard its computation
--	writeRef (valueU t) (return v')
	writeRef (forceU t dir) (return v')
--	writeRef (changedU t) True
	trace ("written " ++ show (idU t) ++ " in direction " ++ show dir) $ inner $ dirty dir t
	writeRef (dirtyU t dir) True
	writeRef (directionU t) dir
	writeRef (memoU t dir) Empty
	return ()

get :: (Memo a,Typeable a,Layer Inner m r,Layer l m r) => U m r a -> l m r a
get t = readRef (directionU t) >>= \dir -> outer (forceOuter dir t)

ref :: (Typeable a,Memo a,Layer l m r) => a -> l m r (U m r a)
ref v = thunk Forward (return v)

thunk' :: (Typeable a,Memo a,Layer l m r) => Direction -> Inner m r a -> l m r (U m r a)
thunk' dir c = do
	valueU <- newUniqueRef Nothing
	directionU <- newRef dir
	let idU = refId valueU
	forceUFwd <- newRef $ error "no computation"
	forceUBwd <- newRef $ error "no computation"
	dependentsUFwd <- newRef IntMap.empty
	dependentsUBwd <- newRef IntMap.empty
	dependenciesUFwd <- newRef []
	dependenciesUBwd <- newRef []
	dirtyUFwd <- newRef True
	dirtyUBwd <- newRef True
	memoUFwd <- newRef Empty
	memoUBwd <- newRef Empty
	traceUFwd <- newRef []
	traceUBwd <- newRef []
	let t = U idU valueU directionU (bx forceUFwd forceUBwd) (bx dependentsUFwd dependentsUBwd) (bx dependenciesUFwd dependenciesUBwd) (bx dirtyUFwd dirtyUBwd) (bx memoUFwd memoUBwd) (bx traceUFwd traceUBwd)
	let changeSide = do -- if the forced direction is undefined, then force the opposite direction
			oldstack <- getCallStack
			putCallStack $ Nothing -- we only change sides at outer-thunks, that have no dependents
			value <- forceInner dir t
			writeRef (dirtyU t dir) False
			putCallStack oldstack
			return value
	writeRef (forceU t (opposite dir)) changeSide
	rethunk dir t c


rethunk' :: (Typeable a,Memo a,Layer l m r) => Direction -> U m r a -> Inner m r a -> l m r (U m r a)
rethunk' dir t c = do
	mapRefM (return . const True) (dirtyU t dir) -- we need to ensure that the new dependencies get computed
--	inL $ dirty (opposite dir) (dependentsU t (opposite dir))
	let final_comp = do
		inL $ mapRefM (\x -> return []) (traceU t dir)
		result <- c
		inL $ mapRefM (\memo -> readRef (traceU t dir) >>= \trace -> insert_trace (reverse trace) memo (SomeValue result)) (memoU t dir) -- when the result is computed, add it to the memo tree
		return result
	inL $ writeRef (forceU t dir) $ final_comp
	writeRef (directionU t) dir
	--do
	--	mb <- readRef (forceU t dir)
	--	case mb of
	--		Nothing -> writeRef (forceU t dir) $ Just final_comp
	--		Just c -> error $ "trying to overwrite a thunk computation at " ++ show dir ++ " " ++ show (idU t)
	return t

-- with change propagation
forceMemo :: (Memo a,Typeable a,Layer Inner m r) => Bool -> Direction -> U m r a -> Inner m r a
forceMemo withMemo dir t = trace ("forcing " ++ show dir ++ " " ++ show (idU t)) $ do
	dirty <- readRef $ dirtyU t dir
	result <- if (not dirty)
		then trace ("non-dirty " ++ show dir ++ " " ++ show (idU t)) $ readRef (valueU t) >>= return . fromJust
		else do
			mbv <- search_memo dir (SomeU t)
			if (withMemo && isJust mbv)
				then trace "memo hit" $ castAsThunkValue t $ fromJust mbv -- we cannot search the memotree if what changed the current thunk was a set
				else do
					trace ("forced at the inner layer " ++ show dir ++ " " ++ show (idU t)) $ do
					oldstack <- getCallStack
					inL $ clearDependenciesDependents dir (idU t) (dependenciesU t dir)
					putCallStack (Just $ SomeU t)
					comp <- readRef (forceU t dir)
					value <- comp
					writeRef (valueU t) (return value)
					writeRef (dirtyU t dir) False
--					undirtyUndefined (opposite dir) t
					putCallStack oldstack
					trace ("computed " ++ show (idU t)) $ return $ value
	callstack <- getCallStack
	case callstack of
		Just (SomeU caller) -> trace ("added dependency " ++ show (idU t) ++ show dir ++ show (idU caller)) $ do
			mapRefM (\tbl -> return $ tbl ++ [(idU t,SomeU t)]) (dependenciesU caller dir) -- insert at the end?
			mapRefM (return . IntMap.insert (idU caller) (SomeU caller)) (dependentsU t dir)
			mapRefM (\trace -> return $ (SomeU t,SomeValue result) : trace) (traceU caller dir)
			return ()
		otherwise -> return () -- if there is no caller, we don't add any dependency (e.g., when displaying)
	if dirty then forceOpposite (opposite dir) t else return () -- this should not change the result, and is only necessary to repair bidirectional dependencies; we need to close the cycle
	return result

forceInner :: (Layer Outer m r,Typeable a,Memo a) => Direction -> (U m r a) -> Inner m r a
forceInner = forceMemo True

forceOuter :: (Layer Outer m r,Typeable a,Memo a) => Direction -> (U m r a) -> Outer m r a
forceOuter dir t = inner $ forceMemo False dir t

-- * Auxiliary functions

noDependentsDirection :: Layer l m r => U m r a -> l m r (Maybe Direction)
noDependentsDirection t = do
	fwd <- readRef (dependentsU t Forward)
	bwd <- readRef (dependentsU t Backward)
	if IntMap.null fwd
		then if IntMap.null bwd then return Nothing else return (Just Forward)
		else if IntMap.null bwd then return (Just Backward) else error "both directions can't have dependents"

undefinedDirection :: Layer l m r => U m r a -> l m r (Maybe Direction)
undefinedDirection t = liftM (fmap opposite) $ noDependentsDirection t 

-- internal, used at set
dirty :: Layer l m r => Direction -> U m r a -> l m r ()
dirty dir t = do
	deps <- readRef (dependentsU t dir)
	if IntMap.null deps
		then do dirtyInverse (opposite dir) t
		else do
			deps' <- Traversable.mapM dirty' deps
			writeRef (dependentsU t dir) deps'
  where
	dirty' :: Layer l m r => SomeU m r -> l m r (SomeU m r)
	dirty' (SomeU u) = do
			b <- readRef (dirtyU u dir)
			if b
				then return $ SomeU u
				else do
					writeRef (dirtyU u dir) True
					writeRef (directionU u) dir
					dirty dir u
					return $ SomeU u

dirtyInverse :: Layer l m r => Direction -> U m r a -> l m r ()
dirtyInverse dir t = mapRefM (Traversable.mapM dirty') (dependentsU t dir) >> return ()
  where
	dirty' :: Layer l m r => SomeU m r -> l m r (SomeU m r)
	dirty' (SomeU u) = do
			b <- readRef (dirtyU u dir)
			if b
				then return $ SomeU u
				else do
					writeRef (dirtyU u dir) True
					writeRef (directionU u) dir
					dirtyInverse dir u
					return $ SomeU u

search_memo :: Layer l m r => Direction -> SomeU m r -> l m r (Maybe SomeValue)
search_memo dir (SomeU t) = readRef (memoU t dir) >>= search_memo_helper where
	search_memo_helper (Leaf value) = return $ Just value
	search_memo_helper Empty = return Nothing
	search_memo_helper (Node (SomeU t) tbl) = do
		k <- force dir t >>= return . memoKey
		case Map.lookup k tbl of
			Just tree -> search_memo_helper tree
			Nothing -> return Nothing

treeify :: Monad m => Trace m r -> SomeValue -> m (MemoTree m r)
treeify [] result = return $ Leaf result
treeify ((comp,SomeValue value):trace') result = do
	let id = memoKey value
	tree' <- treeify trace' result
	return $ Node comp $ Map.singleton id tree'

insert_trace :: Monad m => Trace m r -> MemoTree m r -> SomeValue -> m (MemoTree m r)
insert_trace [] tree result = return $ Leaf result
insert_trace trace (Leaf _) result = error "inconsistent computation"
insert_trace ((comp,SomeValue value):trace') (Node comp' tbl) result = do
	let id = memoKey value
	case Map.lookup id tbl of
		Just mem -> do
			tree <- insert_trace trace' mem result
			return $ Node comp' $ Map.insert id tree tbl
		Nothing -> do
			tree <- treeify trace' result
			return $ Node comp' $ Map.insert id tree tbl
insert_trace trace Empty result = treeify trace result

forceOpposite :: (Typeable l,Memo a,Typeable a,Layer l m r) => Direction -> U m r a -> l m r ()
forceOpposite dir t = trace ("opposites " ++ show dir ++ " " ++ show (idU t)) $ do
	deps <- readRef (dependentsU t dir)
	if IntMap.null deps then return () else forEachDependent dir (\t -> force dir t >> return ()) t

clearDependenciesDependents :: Ref m r => Direction -> Id -> r (Dependencies m r) -> m (r (Dependencies m r))
clearDependenciesDependents dir id r = mapRefM (\deps -> mapM_ clearDependency deps >> return []) r where
	clearDependency (id',SomeU t') = mapRefM (return . IntMap.delete id) (dependentsU t' dir)

forEachDependent :: (Typeable a,Memo a,Layer l m r) => Direction -> (forall b . (Typeable b,Memo b) => U m r b -> l m r ()) -> U m r a -> l m r ()
forEachDependent dir run (t :: U m r a) = do
	deps <- readRef $ dependentsU t dir
	if IntMap.null deps then run t >> return () else intMapM_ forceSomeU deps
  where
	forceSomeU (SomeU u) = forEachDependent dir run u

castAsThunkValue :: (Typeable a,Layer l m r) => U m r a -> SomeValue -> l m r a
castAsThunkValue (t :: U m r a) v = castAsValue (undefined :: a) v

castAsValue :: (Typeable a,Layer l m r) => a -> SomeValue -> l m r a
castAsValue (_ :: a) (SomeValue (b :: b)) = case cast b :: Maybe a of
	Just b' -> return b'
	Nothing -> error "types do not match"

printDependentsU :: (MonadIO m,Layer Inner m r) => Direction -> String -> U m r a -> m ()
printDependentsU dir pad t = do
	let dirStr = if dir == Forward then "-->" else "<--"
	liftIO $ putStrLn $ pad ++ dirStr ++ "U" ++ show (idU t)
	deps <- readRef (dependentsU t dir)
	intMapM_ (\some -> case some of { (SomeU t') -> printDependentsU dir (pad++" ") t' }) deps




