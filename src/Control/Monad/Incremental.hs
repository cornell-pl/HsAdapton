{-# LANGUAGE OverlappingInstances, UndecidableInstances, DeriveDataTypeable, StandaloneDeriving, ConstraintKinds, DataKinds, PolyKinds, ScopedTypeVariables, GADTs, FlexibleContexts, Rank2Types, TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

module Control.Monad.Incremental where

import Control.Monad.Ref
import Control.Monad.Trans.Class
import Data.Typeable
import Data.WithClass.MGenerics.Text
import Data.WithClass.MData
import Prelude hiding (mod,const,read)
import System.Mem.WeakKey
import System.Mem.MemoTable (Memo(..))
import System.Mem.Weak as Weak
import Data.Hashable
import System.Mem.WeakTable hiding (new)
import System.Mem.MemoTable hiding (memo)
import Data.WithClass.MGenerics.Aliases
import Language.Haskell.TH.Syntax
import Control.Monad
import Data.IORef
import qualified Data.Strict.Tuple as Strict
import Control.Monad.IO.Class
import Control.Concurrent
import Debug
import GHC.Exts

-- | General class for incremental computation libraries
class (MonadRef r m,WeakRef r
	,  Monad (Outside inc r m), InLayer Outside inc r m
	,  Monad (Inside inc r m), InLayer Inside inc r m
	) => Incremental (inc :: *) (r :: * -> *) (m :: * -> *) where
	
	data Outside inc (r :: * -> *) (m :: * -> *) a :: *
	data Inside inc (r :: * -> *) (m :: * -> *) a :: *
	
	-- lifts a computation at the inner layer to one at the outer layer
	world :: Inside inc r m a -> Outside inc r m a
	-- unlifts a computation at the outer layer to one at the inner layer. This function is not to be used normally, as it is unsafe for many IC idioms.
	unsafeWorld :: Outside inc r m a -> Inside inc r m a
	
	runIncremental :: Outside inc r m a -> m a

class (Typeable l,Typeable inc,Typeable r,Typeable m) => InLayer l inc r m where
	inL :: m a -> l inc r m a
	
-- | A class to facilitate the combination of different incremental computation styles
class LiftInc (l :: * -> (* -> *) -> (* -> *) -> * -> *) inc1 inc2 r m where
	liftInc :: l inc1 r m a -> l inc2 r m a

class Layers l1 l2 where
	liftLayer :: (Layer l1 inc r m,Layer l2 inc r m) => l1 inc r m a -> l2 inc r m a
instance Layers Inside Outside where
	liftLayer = world
instance Layers Inside Inside where
	liftLayer = id
instance Layers Outside Outside where
	liftLayer = id

-- | Incremental computation layers
class (InLayer l inc r m,Monad (l inc r m),Incremental inc r m) => Layer l inc r m where
	inside :: Inside inc r m a -> l inc r m a
	outside :: l inc r m a -> Outside inc r m a

instance (InLayer Outside inc r m,Incremental inc r m) => Layer Outside inc r m where
	inside = world
	{-# INLINE inside #-}
	outside = id
	{-# INLINE outside #-}

instance (InLayer Inside inc r m,Incremental inc r m) => Layer Inside inc r m where
	inside = id
	{-# INLINE inside #-}
	outside = world
	{-# INLINE outside #-}

deriving instance Typeable Outside
deriving instance Typeable Inside
--deriving instance Typeable IncrementalArgs

type family IncK inc a :: Constraint

-- | A general class for thunks with no assumptions of incrementality. The only expectation is that it supports sharing (if we read the thunk twice the computataion is only performed once)
class (Typeable mod,Layer l inc r m) => Thunk mod l inc r m where
	new :: (IncK inc a) => l inc r m a -> l inc r m (mod l inc r m a)
	newc :: (IncK inc a) => a -> l inc r m (mod l inc r m a)
	newc = Control.Monad.Incremental.new . return
	read :: (IncK inc a) => mod l inc r m a -> l inc r m a

--instance Output mod l inc r m => Thunk mod l inc r m where
--	new = mod
--	newc = ref
--	read = get
--
--instance Input mod l inc r m => Thunk mod l inc r m where
--	new = thunk
--	newc = const
--	read = force

-- | Output modifiable references (can NOT be directly mutated; are updated for changes on other modifiables)
class (Thunk mod l inc r m,Layer l inc r m) => Output mod l inc r m where
	
	thunk :: (IncK inc a) => l inc r m a -> l inc r m (mod l inc r m a)
	
	const :: (IncK inc a) => a -> l inc r m (mod l inc r m a)
	const = thunk . return
	{-# INLINE const #-}
	
	force :: (IncK inc a) => mod l inc r m a -> l inc r m a
	
	{-# INLINE forceOutside #-}
	forceOutside :: (IncK inc a) => mod l inc r m a -> Outside inc r m a
	forceOutside = outside . force
	
	-- * these memoization functions are specific to the Adapton approach, otherwise they are no-ops
	memo :: (IncK inc a,Memo arg) => ((arg -> l inc r m (mod l inc r m a)) -> arg -> l inc r m a) -> (arg -> l inc r m (mod l inc r m a))
	memo f arg = thunk $ f (memo f) arg
	
	memo2 :: (IncK inc a,Memo arg1,Memo arg2) => ((arg1 -> arg2 -> l inc r m (mod l inc r m a)) -> arg1 -> arg2 -> l inc r m a) -> (arg1 -> arg2 -> l inc r m (mod l inc r m a))
	memo2 f = curry (memo (uncurry . f . curry))
	{-# INLINE memo2 #-}
	
	memo3 :: (IncK inc a,Memo arg1,Memo arg2,Memo arg3) => ((arg1 -> arg2 -> arg3 -> l inc r m (mod l inc r m a)) -> arg1 -> arg2 -> arg3 -> l inc r m a) -> (arg1 -> arg2 -> arg3 -> l inc r m (mod l inc r m a))
	memo3 f = curry3 (memo (uncurry3 . f . curry3))
		where
		curry3 f x y z = f (x,y,z)
		uncurry3 f (x,y,z) = f x y z
	{-# INLINE memo3 #-}
	
	-- | fix-point memoization for incremental generic queries
	gmemoQ :: (Typeable ctx,IncK inc b) => Proxy ctx -> (GenericQMemo ctx mod l inc r m b -> GenericQMemo ctx mod l inc r m b) -> GenericQMemo ctx mod l inc r m b
	gmemoQ ctx (f :: GenericQMemo ctx mod l inc r m b -> GenericQMemo ctx mod l inc r m b) =
		let memo_func :: GenericQMemo ctx mod l inc r m b
		    memo_func = f memo_func
		in memo_func
	{-# INLINE gmemoQ #-}

-- | Input modifiable references (can be directly mutated; are NOT updated for changes on other thunks/modifiables)
-- inputs that support delayed changes are parameterized with the layer at which that computation should run
-- Minimal definition: ref, get, set
class (Thunk mod l inc r m,Layer l inc r m) => Input mod l inc r m where
	
	-- | strict modifiable
	ref :: (IncK inc a) => a -> l inc r m (mod l inc r m a)
	-- | lazy modifiable
	
	{-# INLINE mod #-}
	mod :: (IncK inc a) => l inc r m a -> l inc r m (mod l inc r m a)
	mod c = c >>= ref
	
	-- | reads the value of a modifiable
	get :: (IncK inc a) => mod l inc r m a -> l inc r m a
	
	-- | strictly changes the value of a modifiable
	set :: (IncK inc a,Layer Outside inc r m) => mod l inc r m a -> a -> Outside inc r m ()
	
	-- | lazily changes the value of a modifiable
	{-# INLINE overwrite #-}
	overwrite :: (IncK inc a,Layer Outside inc r m) => mod l inc r m a -> l inc r m a -> Outside inc r m ()
	overwrite = \m c -> outside c >>= set m
	
	-- | lazily appends a new change to the pending change sequence of a modifiable
	{-# INLINE modify #-}
	modify :: (IncK inc a,Layer Outside inc r m) => mod l inc r m a -> (a -> l inc r m a) -> Outside inc r m ()
	modify = \m f -> getOutside m >>= overwrite m . f

	{-# INLINE refOutside #-}
	refOutside :: IncK inc a => a -> Outside inc r m (mod l inc r m a)
	refOutside = outside . ref

	{-# INLINE modOutside #-}
	modOutside :: IncK inc a => l inc r m a -> Outside inc r m (mod l inc r m a)
	modOutside = outside . mod
	
	{-# INLINE getOutside #-}
	getOutside :: IncK inc a => mod l inc r m a -> Outside inc r m a
	getOutside = outside . get

overwriteAndReturn :: (IncK inc a,Input mod l inc r m,Layer Outside inc r m) => mod l inc r m a -> (l inc r m a) -> Outside inc r m (mod l inc r m a)
overwriteAndReturn t m = overwrite t m >> return t

modifyAndReturn :: (IncK inc a,Input mod l inc r m,Layer Outside inc r m) => mod l inc r m a -> (a -> l inc r m a) -> Outside inc r m (mod l inc r m a)
modifyAndReturn t f = modify t f >> return t

-- * Generics

instance (DeepTypeable inc,MData ctx (Inside inc r m) a,Typeable a,DeepTypeable m,DeepTypeable r,Sat (ctx (Inside inc r m a)),Monad m) => MData ctx (Inside inc r m) (Inside inc r m a) where
	gfoldl ctx k z m = z return >>= flip k m
	gunfold ctx k z c = z return >>= k
	toConstr ctx m = dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Control.Monad.Adapton.Inside" [mkConstr ty "return" [] Prefix]
	
instance (Layer Inside inc r m,DeepTypeable inc,MData ctx (Outside inc r m) a,Typeable a,DeepTypeable m,DeepTypeable r,Sat (ctx (Inside inc r m a)),Monad m) => MData ctx (Outside inc r m) (Inside inc r m a) where
	gfoldl ctx k z m = inside m >>= \x -> z (liftM return) >>= flip k (return x)
	gunfold ctx k z c = z (liftM return) >>= k
	toConstr ctx m = dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Control.Monad.Adapton.Inside" [mkConstr ty "return" [] Prefix]

instance (Layer Outside inc r m,DeepTypeable inc,MData ctx (Outside inc r m) a,Typeable a,DeepTypeable m,DeepTypeable r,Sat (ctx (Outside inc r m a)),Monad m) => MData ctx (Outside inc r m) (Outside inc r m a) where
	gfoldl ctx k z m = z return >>= flip k m
	gunfold ctx k z c = z return >>= k
	toConstr ctx m = dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Control.Monad.Adapton.Outside" [mkConstr ty "return" [] Prefix]

instance (Typeable a,IncK inc a,Sat (ctx (mod l1 inc r m a)),Eq a,MData ctx (l2 inc r m) (l1 inc r m a),Layers l1 l2,Layer l1 inc r m,Layer l2 inc r m,Thunk mod l1 inc r m,DeepTypeable inc,DeepTypeable m,DeepTypeable r,DeepTypeable (mod l1 inc r m a)
		) => MData ctx (l2 inc r m) (mod l1 inc r m a) where
	gfoldl ctx k z t = z (\mmx -> mmx >>= liftLayer . new) >>= flip k (return $ read t)
	gunfold ctx k z c = z (\mmx -> mmx >>= liftLayer . new) >>= k
	toConstr ctx m = dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Control.Monad.Incremental.Thunk" [mkConstr ty "Thunk" [] Prefix]

-- this instance is too generic and causes overlapping proble, but should be used as a pattern for specific thunk types
--instance (Eq a,Layer l inc r m,Thunk mod l inc r m,MData ctx (l inc r m) a
--		, Sat (ctx (mod l inc r m a)),DeepTypeable (mod l inc r m a)
--		) => MData ctx (l inc r m) (mod l inc r m a) where
--	gfoldl ctx k z t = z new >>= flip k (read t)
--	gunfold ctx k z c = z new >>= k
--	toConstr ctx m = dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
--	dataTypeOf ctx x = return ty
--		where ty = mkDataType "Control.Monad.Adapton.U" [mkConstr ty "U" [] Prefix]

-- * Memoization

-- for evaluation-order reasons related to @gfoldl@ we need to treat layer computations as traversable types, but for correctness of IC we can't memoize them
instance (Typeable inc,Typeable r,Typeable m,Typeable a) => Memo (Inside inc r m a) where
	type Key (Inside incr r m a) = Neq
	{-# INLINE memoKey #-}
	memoKey m = (MkWeak mkDeadWeak,Neq)

-- for evaluation-order reasons related to @gfoldl@ we need to treat layer computations as traversable types, but for correctness of IC we can't memoize them
instance (Typeable inc,Typeable r,Typeable m,Typeable a) => Memo (Outside inc r m a) where
	type Key (Outside incr r m a) = Neq
	{-# INLINE memoKey #-}
	memoKey m = (MkWeak mkDeadWeak,Neq)

-- | A generic query type with a memoization context
type GenericQMemo ctx (thunk :: (* -> (* -> *) -> (* -> *) -> * -> *) -> * -> (* -> *) -> (* -> *) -> * -> *) l inc r m b = GenericQ (MemoCtx ctx) (l inc r m) (thunk l inc r m b)

-- | A generic memoization context type
data MemoCtx ctx a = MemoCtx {
	  memoKeyCtx :: Proxy ctx -> a -> (MkWeak,Key a)
	, keyDynamicCtx :: Proxy ctx -> Proxy a -> Key a -> KeyDynamic
	, memoCtx :: ctx a
	} deriving Typeable

type MemoCtxK ctx a = (Typeable a,Typeable (Key a),Memo a,Sat (ctx a))

instance (MemoCtxK ctx a) => Sat (MemoCtx ctx a) where
	dict = MemoCtx
		(\ctx -> memoKey)
		(\ctx a -> KeyDyn)
		dict

data KeyDynamic where
	KeyDyn :: (Typeable k,Eq k,Hashable k) => k -> KeyDynamic

deriving instance Typeable KeyDynamic
	
instance Hashable KeyDynamic where
	hashWithSalt i (KeyDyn k) = hashWithSalt i k
instance Eq KeyDynamic where
	(KeyDyn k1) == (KeyDyn (k2::k2)) = case cast k1 :: Maybe k2 of
		Nothing -> False
		Just k1 -> k1 == k2
		
proxyMemoCtx :: Proxy ctx -> Proxy (MemoCtx ctx)
proxyMemoCtx _ = Proxy

instance (DeepTypeable inc,DeepTypeable r,DeepTypeable m,DeepTypeable a) => DeepTypeable (Outside inc r m a) where
	typeTree (_::Proxy (Outside inc r m a)) = MkTypeTree (mkName "Control.Monad.Incremental.Outside") args [MkConTree (mkName "Control.Monad.Incremental.Outside") [typeTree (Proxy::Proxy a)]]
		where args = [typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy r),typeTree (Proxy::Proxy m),typeTree (Proxy::Proxy a)]
instance (DeepTypeable inc,DeepTypeable r,DeepTypeable m,DeepTypeable a) => DeepTypeable (Inside inc r m a) where
	typeTree (_::Proxy (Inside inc r m a)) = MkTypeTree (mkName "Control.Monad.Incremental.Inside") args [MkConTree (mkName "Control.Monad.Incremental.Inside") [typeTree (Proxy::Proxy a)]]
		where args = [typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy r),typeTree (Proxy::Proxy m),typeTree (Proxy::Proxy a)]

instance DeepTypeable Inside where
	typeTree _ = MkTypeTree (mkName "Control.Monad.Incremental.Inside") [] []

instance DeepTypeable Outside where
	typeTree _ = MkTypeTree (mkName "Control.Monad.Incremental.Outside") [] []

proxyInside = (Proxy::Proxy Inside)
proxyOutside = (Proxy::Proxy Outside)

-- | A plain thunk type with sharing but is not mutable nor incrementally updated
newtype T (l :: * -> (* -> *) -> (* -> *) -> * -> *) inc (r :: * -> *) (m :: * -> *) a = T (r (l inc r m a)) deriving Typeable

instance (Layer l inc r m) => Thunk T l inc r m where
	new m = liftM T $ inL $ newRef m
	read (T r) =  do
		m <- inL $ readRef r
		x <- m
		inL $ writeRef r $ return x
		return x

proxyIORef = Proxy :: Proxy IORef
proxyIO = Proxy :: Proxy IO

deriving instance Typeable Strict.Pair

debugM' :: (Layer l inc r m,MonadIO m) => String -> l inc r m a -> l inc r m a
debugM' str m = do
	threadid <- inL $ liftIO $ myThreadId
	debug ("{"++ show threadid ++ "}" ++ str) m