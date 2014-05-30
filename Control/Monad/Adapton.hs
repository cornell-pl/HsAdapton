{-# LANGUAGE UndecidableInstances, FlexibleInstances, ScopedTypeVariables, FlexibleContexts, KindSignatures, MultiParamTypeClasses #-}

module Control.Monad.Adapton where

import Control.Monad.Adapton.DataTypes
import Control.Monad.Ref
import Data.IntMap (IntMap(..))
import qualified Data.IntMap as IntMap
import Data.Map (Map(..))
import qualified Data.Map as Map
import Control.Monad.Trans
import System.Mem.MemoTable
import Data.Typeable
import Data.Traversable as Traversable
import Control.Monad hiding (mapM)
import Control.Monad.State (MonadState)
import qualified Control.Monad.State as State
import Data.Maybe

import Debug.Trace

-- | Run an Adapton computation
run :: Monad m => Outer m r a -> m a
run (Outer comp) = do
	(x,_,_) <- comp 0 Nothing
	return x

-- * Classes and instances

class (UniqueRef (l m r) (IdRef r),MonadIO m,MonadIO (l m r),Ref (Inner m r) r,Ref (l m r) r,Typeable r,Typeable m,MonadState (Id, CallStack m r) (l m r),InL l,Ref m r) => Layer l m r where
	inner :: Inner m r a -> l m r a
	inOuter :: l m r a -> Outer m r a
	-- | outer is only to be used internally!
	outer :: Outer m r a -> l m r a
	force :: (Typeable a,Memo a) => U m r a -> l m r a
	thunk :: (Typeable a,Memo a) => Inner m r a -> l m r (U m r a)
	thunk = thunk'

instance (MonadIO m,Ref (Inner m r) r,Typeable m,Typeable r,Ref m r) => Layer Inner m r where
	inner = id
	inOuter (Inner m) = Outer m
	outer (Outer m) = Inner m
	force = forceInner

instance (MonadIO m,Ref (Outer m r) r,Ref (Inner m r) r,Typeable m,Typeable r,Ref m r) => Layer Outer m r where
	inner (Inner m) = Outer m
	inOuter = id
	outer = id
	force = forceOuter

-- * Main functions

memo :: (Typeable a,Memo a,Layer Inner m r,Memo arg) => ((arg -> Inner m r (U m r a)) -> arg -> Inner m r a) -> (arg -> Inner m r (U m r a))
memo f arg = let memo_func = memoLazyM (thunk . f memo_func) in memo_func arg -- it is important to apply the function to the argument here, otherwise new memoized functions are created for new runs

-- | Force a computation with change propagation
forceInner :: (Memo a,Typeable a,Layer Inner m r) => U m r a -> Inner m r a
forceInner = forceMemo True

-- | Force a computation without change propagation
forceOuter :: (Layer Outer m r,Typeable a,Memo a) => U m r a -> Outer m r a
forceOuter = inner . forceMemo False

forceMemo :: (Memo a,Typeable a,Layer Inner m r) => Bool -> U m r a -> Inner m r a
forceMemo withMemo t = do
	dirty <- readRef $ dirtyU t
	result <- if (not dirty)
		then readRef $ valueU t
		else do
			oldstack <- getCallStack
			mbv <- search_memo (SomeU t)
			if (withMemo && isJust mbv)
				then castAsThunkValue t $ fromJust mbv
				else do
					trace ("forced at the inner layer " ++ show (idU t)) $ do
					inL $ clearDependenciesDependents (idU t) (dependenciesU t)
					putCallStack (Just $ SomeU t)
					value <- readRef (forceU t) >>= \c -> c
					writeRef (valueU t) value
					writeRef (dirtyU t) False
					putCallStack oldstack
					return value
	addToStack t result
	return result

thunk' :: (Typeable a,Memo a,Layer l m r) => Inner m r a -> l m r (U m r a)
thunk' c = do
	valueU <- newUniqueRef $ error "no old value in thunk"
	let idU = refId valueU
	forceU <- newRef $ return $ error "no old value in thunk"
	dependentsU <- newRef IntMap.empty
	dependenciesU <- newRef []
	dirtyU <- newRef True
	memoU <- newRef Empty
	traceU <- newRef []
	let t = U idU valueU forceU dependentsU dependenciesU dirtyU memoU traceU
	let final_comp = do
		inL $ mapRefM (\x -> return []) traceU
		result <- c
		inL $ mapRefM (\memo -> readRef traceU >>= \trace -> insert_trace (reverse trace) memo (SomeValue result)) memoU
		return result
	inL $ writeRef forceU final_comp
	return t

-- | change at the outer layer
set :: Layer Outer m r => M m r a -> a -> Outer m r ()
set (M t) v' = do
	writeRef (valueU t) v'
	dirty t
	writeRef (memoU t) Empty

-- | Corresponds to forcing a thunk at the outer layer
get :: (Typeable a,Memo a,Layer l m r) => M m r a -> l m r a
get (M t) = outer $ forceOuter t

-- | We could define a specialized version that does not require @Memo a@
ref :: (Typeable a,Memo a,Layer Outer m r) => a -> Outer m r (M m r a)
ref v = liftM M $ thunk (return v)

-- * Auxiliary functions

insert_trace :: Ref m r => Trace m r -> MemoTree m r -> SomeValue -> m (MemoTree m r)
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

treeify :: Ref m r => Trace m r -> SomeValue -> m (MemoTree m r)
treeify [] result = return $ Leaf result
treeify ((comp,SomeValue value):trace') result = do
	let id = memoKey value
	tree' <- treeify trace' result
	return $ Node comp $ Map.singleton id tree'

search_memo :: Layer l m r => SomeU m r -> l m r (Maybe SomeValue)
search_memo (SomeU t) = readRef (memoU t) >>= search_memo_helper where
	search_memo_helper (Leaf value) = return $ Just value
	search_memo_helper Empty = return Nothing
	search_memo_helper (Node (SomeU t) tbl) = do
		k <- force t >>= return . memoKey
		case Map.lookup k tbl of
			Just tree -> search_memo_helper tree
			Nothing -> return Nothing

addToStack :: (Memo a,Typeable a,Layer l m r) => U m r a -> a -> l m r ()
addToStack t result = do
	callstack <- getCallStack
	case callstack of
		Just (SomeU caller) -> do
			mapRefM (\tbl -> return $ tbl ++ [(idU t,SomeU t)]) (dependenciesU caller) -- insert at the end?
			mapRefM (return . IntMap.insert (idU caller) (SomeU caller)) (dependentsU t)
			mapRefM (\trace -> return $ (SomeU t,SomeValue result) : trace) (traceU caller)
			return ()
		Nothing -> return ()

clearDependenciesDependents :: Ref m r => Id -> r (Dependencies m r) -> m (r (Dependencies m r))
clearDependenciesDependents id r = mapRefM (\deps -> mapM_ clearDependency deps >> return []) r where
	clearDependency (id',SomeU t') = mapRefM (return . IntMap.delete id) (dependentsU t')

castAsThunkValue :: (Typeable a,Layer l m r) => U m r a -> SomeValue -> l m r a
castAsThunkValue (t :: U m r a) v = castAsValue (undefined :: a) v

castAsValue :: (Typeable a,Layer l m r) => a -> SomeValue -> l m r a
castAsValue (_ :: a) (SomeValue (b :: b)) = case cast b :: Maybe a of
	Just b' -> return b'
	Nothing -> error "types do not match"

dirty :: Layer l m r => U m r a -> l m r ()
dirty t = mapRefM (Traversable.mapM dirty') (dependentsU t) >> return ()
  where
	dirty' :: Layer l m r => SomeU m r -> l m r (SomeU m r)
	dirty' (SomeU u) = do
			b <- inL $ readRef (dirtyU u)
			if b
				then return $ SomeU u
				else do
					inL $ writeRef (dirtyU u) True
					dirty u
					return $ SomeU u

noDependents :: Layer l m r => U m r a -> l m r Bool
noDependents t = liftM (IntMap.null) $ readRef (dependentsU t)



