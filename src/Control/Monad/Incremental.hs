{-# LANGUAGE TemplateHaskell, OverlappingInstances, UndecidableInstances, DeriveDataTypeable, StandaloneDeriving, ConstraintKinds, DataKinds, PolyKinds, ScopedTypeVariables, GADTs, FlexibleContexts, Rank2Types, TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

module Control.Monad.Incremental where


import Control.Monad.Trans.Class

import Data.Typeable
import Data.WithClass.MGenerics.Text
import Data.WithClass.MData
import Prelude hiding (mod,const,read)

import Data.Memo
import System.Mem.Weak.Exts as Weak
import Data.Hashable
import qualified System.Mem.MemoTable as MemoTable
import Data.WithClass.MGenerics.Aliases
import Language.Haskell.TH.Syntax
import Control.Monad
import Data.IORef
import qualified Data.Strict.Tuple as Strict
import Control.Monad.IO.Class
import Control.Concurrent
import Debug
import GHC.Exts
import Data.Derive.Memo
import Control.Applicative
import System.Mem.StableName.Exts

-- | General class for incremental computation libraries
class (
	   Monad (Outside inc), Monad (Inside inc),Applicative (Outside inc),Applicative (Inside inc)
	) => Incremental (inc :: *) where
	
	data Outside inc a :: *
	data Inside inc a :: *
	
	displayK :: IncK inc a => a -> Inside inc String
	
	unsafeIOToInc :: Layer l inc => IO a -> l inc a
	
	-- lifts a computation at the inner layer to one at the outer layer
	world :: Inside inc a -> Outside inc a
	-- unlifts a computation at the outer layer to one at the inner layer. This function is not to be used normally, as it is unsafe for many IC idioms.
	unsafeWorld :: Outside inc a -> Inside inc a
	
	data IncParams inc :: *
	defaultIncParams :: IncParams inc
	
	runIncremental :: Outside inc a -> IO a
	runIncremental = runIncrementalWithParams defaultIncParams
	runIncrementalWithParams :: IncParams inc -> Outside inc a -> IO a
	
defaultIncParamsProxy :: Incremental inc => Proxy inc -> IncParams inc
defaultIncParamsProxy _ = defaultIncParams
	
runIncrementalProxy :: Incremental inc => Proxy inc -> Outside inc a -> IO a
runIncrementalProxy _ = runIncremental

runIncrementalWithParamsProxy :: Incremental inc => Proxy inc -> IncParams inc -> Outside inc a -> IO a
runIncrementalWithParamsProxy _ = runIncrementalWithParams
	
-- | A class to facilitate the combination of different incremental computation styles
class LiftInc (l :: * -> * -> *) inc1 inc2 where
	liftInc :: l inc1 a -> l inc2 a

class Layers l1 l2 where
	liftLayer :: (Layer l1 inc,Layer l2 inc) => l1 inc a -> l2 inc a
instance Layers Inside Outside where
	liftLayer = world
instance Layers Inside Inside where
	liftLayer = id
instance Layers Outside Outside where
	liftLayer = id

data LayerK = Inside | Outside deriving Typeable

class Typeable l => LayerKind l where
	toLayerKind :: Proxy l -> LayerK
instance LayerKind Inside where
	toLayerKind _ = Inside
instance LayerKind Outside where
	toLayerKind _ = Outside

-- | Incremental computation layers
class (LayerKind l,Monad (l inc),Incremental inc) => Layer l inc where
	inside :: Inside inc a -> l inc a
	outside :: l inc a -> Outside inc a
	isInside :: l inc Bool

instance (Incremental inc) => Layer Outside inc where
	inside = world
	{-# INLINE inside #-}
	outside = id
	{-# INLINE outside #-}
	isInside = return False

instance (Incremental inc) => Layer Inside inc where
	inside = id
	{-# INLINE inside #-}
	outside = world
	{-# INLINE outside #-}
	isInside = return True

deriving instance Typeable Outside
deriving instance Typeable Inside
--deriving instance Typeable IncrementalArgs

type family IncK inc a :: Constraint

-- | A general class for thunks with no assumptions of incrementality. The only expectation is that it supports sharing (if we read the thunk twice the computataion is only performed once)
class (Typeable mod,Layer l inc) => Thunk mod l inc where
	new :: (IncK inc a) => l inc a -> l inc (mod l inc a)
	newc :: (IncK inc a) => a -> l inc (mod l inc a)
	newc = Control.Monad.Incremental.new . return
	read :: (IncK inc a) => mod l inc a -> l inc a

-- | Output modifiable references (can NOT be directly mutated; are updated for changes on other modifiables)
class (Thunk mod l inc,Layer l inc) => Output mod l inc where
	
	thunk :: (IncK inc a) => l inc a -> l inc (mod l inc a)
	
	const :: (IncK inc a) => a -> l inc (mod l inc a)
	const = thunk . return
	{-# INLINE const #-}
	
	force :: (IncK inc a) => mod l inc a -> l inc a
	
	{-# INLINE forceOutside #-}
	forceOutside :: (IncK inc a) => mod l inc a -> Outside inc a
	forceOutside = outside . force
	
	-- * these memoization functions are specific to the Adapton approach, otherwise they are no-ops
	memo :: (IncK inc a,Memo arg) => ((arg -> l inc (mod l inc a)) -> arg -> l inc a) -> (arg -> l inc (mod l inc a))
	memo f arg = thunk $ f (memo f) arg
	
	-- memoization function with a name
	memoAs :: (Memo name,IncK inc a,Memo arg) => name -> ((arg -> l inc (mod l inc a)) -> arg -> l inc a) -> (arg -> l inc (mod l inc a))
	memoAs name f arg = thunk $ f (memoAs name f) arg
	
	memo2 :: (IncK inc a,Memo arg1,Memo arg2) => ((arg1 -> arg2 -> l inc (mod l inc a)) -> arg1 -> arg2 -> l inc a) -> (arg1 -> arg2 -> l inc (mod l inc a))
	memo2 f = curry (memo (uncurry . f . curry))
	{-# INLINE memo2 #-}
	
	memo2As :: (Memo name,IncK inc a,Memo arg1,Memo arg2) => name -> ((arg1 -> arg2 -> l inc (mod l inc a)) -> arg1 -> arg2 -> l inc a) -> (arg1 -> arg2 -> l inc (mod l inc a))
	memo2As name f = curry (memoAs name (uncurry . f . curry))
	{-# INLINE memo2As #-}
	
	memo3 :: (IncK inc a,Memo arg1,Memo arg2,Memo arg3) => ((arg1 -> arg2 -> arg3 -> l inc (mod l inc a)) -> arg1 -> arg2 -> arg3 -> l inc a) -> (arg1 -> arg2 -> arg3 -> l inc (mod l inc a))
	memo3 f = curry3 (memo (uncurry3 . f . curry3))
		where
		curry3 f x y z = f (x,y,z)
		uncurry3 f (x,y,z) = f x y z
	{-# INLINE memo3 #-}
	
	memo3As :: (Memo name,IncK inc a,Memo arg1,Memo arg2,Memo arg3) => name -> ((arg1 -> arg2 -> arg3 -> l inc (mod l inc a)) -> arg1 -> arg2 -> arg3 -> l inc a) -> (arg1 -> arg2 -> arg3 -> l inc (mod l inc a))
	memo3As name f = curry3 (memoAs name (uncurry3 . f . curry3))
		where
		curry3 f x y z = f (x,y,z)
		uncurry3 f (x,y,z) = f x y z
	{-# INLINE memo3As #-}
	
	-- | fix-point memoization for incremental generic queries
	gmemoQ :: (Typeable ctx,IncK inc b) => Proxy ctx -> (GenericQMemo ctx mod l inc b -> GenericQMemo ctx mod l inc b) -> GenericQMemo ctx mod l inc b
	gmemoQ ctx (f :: GenericQMemo ctx mod l inc b -> GenericQMemo ctx mod l inc b) =
		let memo_func :: GenericQMemo ctx mod l inc b
		    memo_func = f memo_func
		in memo_func
	{-# INLINE gmemoQ #-}
	
	gmemoQAs :: (Memo name,Typeable ctx,IncK inc b) => Proxy ctx -> name -> (GenericQMemo ctx mod l inc b -> GenericQMemo ctx mod l inc b) -> GenericQMemo ctx mod l inc b
	gmemoQAs ctx name (f :: GenericQMemo ctx mod l inc b -> GenericQMemo ctx mod l inc b) =
		let memo_func :: GenericQMemo ctx mod l inc b
		    memo_func = f memo_func
		in memo_func
	{-# INLINE gmemoQAs #-}

-- | Input modifiable references (can be directly mutated; are NOT updated for changes on other thunks/modifiables)
-- inputs that support delayed changes are parameterized with the layer at which that computation should run
-- Minimal definition: ref, get, set
class (Thunk mod l inc,Layer l inc) => Input mod l inc where
	
	-- | strict modifiable
	ref :: (IncK inc a) => a -> l inc (mod l inc a)
	-- | lazy modifiable
	
	{-# INLINE mod #-}
	mod :: (IncK inc a) => l inc a -> l inc (mod l inc a)
	mod c = c >>= ref
	
	-- | reads the value of a modifiable
	get :: (IncK inc a) => mod l inc a -> l inc a
	
	-- | strictly changes the value of a modifiable
	set :: (IncK inc a,Layer Outside inc) => mod l inc a -> a -> Outside inc ()
	
	-- | lazily changes the value of a modifiable
	{-# INLINE overwrite #-}
	overwrite :: (IncK inc a,Layer Outside inc) => mod l inc a -> l inc a -> Outside inc ()
	overwrite = \m c -> outside c >>= set m
	
	-- | lazily appends a new change to the pending change sequence of a modifiable
	{-# INLINE modify #-}
	modify :: (IncK inc a,Layer Outside inc) => mod l inc a -> (a -> l inc a) -> Outside inc ()
	modify = \m f -> getOutside m >>= overwrite m . f

	{-# INLINE refOutside #-}
	refOutside :: IncK inc a => a -> Outside inc (mod l inc a)
	refOutside = outside . ref

	{-# INLINE modOutside #-}
	modOutside :: IncK inc a => l inc a -> Outside inc (mod l inc a)
	modOutside = outside . mod
	
	{-# INLINE getOutside #-}
	getOutside :: IncK inc a => mod l inc a -> Outside inc a
	getOutside = outside . get

overwriteAndReturn :: (IncK inc a,Input mod l inc,Layer Outside inc) => mod l inc a -> (l inc a) -> Outside inc (mod l inc a)
overwriteAndReturn t m = overwrite t m >> return t

modifyAndReturn :: (IncK inc a,Input mod l inc,Layer Outside inc) => mod l inc a -> (a -> l inc a) -> Outside inc (mod l inc a)
modifyAndReturn t f = modify t f >> return t

-- * Generics

instance (DeepTypeable inc,MData ctx (Inside inc) a,Typeable a,Sat (ctx (Inside inc a))) => MData ctx (Inside inc) (Inside inc a) where
	gfoldl ctx k z m = z return >>= flip k m
	gunfold ctx k z c = z return >>= k
	toConstr ctx m = dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Control.Monad.Adapton.Inside" [mkConstr ty "return" [] Prefix]
	
instance (Layer Inside inc,DeepTypeable inc,MData ctx (Outside inc) a,Typeable a,Sat (ctx (Inside inc a))) => MData ctx (Outside inc) (Inside inc a) where
	gfoldl ctx k z m = inside m >>= \x -> z (liftM return) >>= flip k (return x)
	gunfold ctx k z c = z (liftM return) >>= k
	toConstr ctx m = dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Control.Monad.Adapton.Inside" [mkConstr ty "return" [] Prefix]

instance (Layer Outside inc,DeepTypeable inc,MData ctx (Outside inc) a,Typeable a,Sat (ctx (Outside inc a))) => MData ctx (Outside inc) (Outside inc a) where
	gfoldl ctx k z m = z return >>= flip k m
	gunfold ctx k z c = z return >>= k
	toConstr ctx m = dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Control.Monad.Adapton.Outside" [mkConstr ty "return" [] Prefix]

instance (Typeable a,IncK inc a,Sat (ctx (mod l1 inc a)),Eq a,MData ctx (l2 inc) (l1 inc a),Layers l1 l2,Layer l1 inc,Layer l2 inc,Thunk mod l1 inc,DeepTypeable inc,DeepTypeable (mod l1 inc a)
		) => MData ctx (l2 inc) (mod l1 inc a) where
	gfoldl ctx k z t = z (\mmx -> mmx >>= liftLayer . new) >>= flip k (return $ read t)
	gunfold ctx k z c = z (\mmx -> mmx >>= liftLayer . new) >>= k
	toConstr ctx m = dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Control.Monad.Incremental.Thunk" [mkConstr ty "Thunk" [] Prefix]

-- this instance is too generic and causes overlapping problems, but should be used as a pattern for specific thunk types
--instance (Eq a,Layer l inc r m,Thunk mod l inc r m,MData ctx (l inc r m) a
--		, Sat (ctx (mod l inc a)),DeepTypeable (mod l inc a)
--		) => MData ctx (l inc r m) (mod l inc a) where
--	gfoldl ctx k z t = z new >>= flip k (read t)
--	gunfold ctx k z c = z new >>= k
--	toConstr ctx m = dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
--	dataTypeOf ctx x = return ty
--		where ty = mkDataType "Control.Monad.Adapton.U" [mkConstr ty "U" [] Prefix]

-- | A generic query type with a memoization context
type GenericQMemo ctx (thunk :: (* -> * -> *) -> * -> * -> *) l inc b = GenericQ (MemoCtx ctx) (l inc) (thunk l inc b)

-- | A generic memoization context type
data MemoCtx ctx a = MemoCtx {
	  memoWeakKeyCtx :: Proxy ctx -> a -> (MkWeak,Key a)
	, keyDynamicCtx :: Proxy ctx -> Proxy a -> Key a -> KeyDynamic
	, memoCtx :: ctx a
	} deriving Typeable

type MemoCtxK ctx a = (Typeable a,Typeable (Key a),Memo a,Sat (ctx a))

instance (MemoCtxK ctx a) => Sat (MemoCtx ctx a) where
	dict = MemoCtx
		(\ctx x -> (memoWeak x,memoKey x))
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

instance (DeepTypeable inc,DeepTypeable a) => DeepTypeable (Outside inc a) where
	typeTree (_::Proxy (Outside inc a)) = MkTypeTree (mkName "Control.Monad.Incremental.Outside") args [MkConTree (mkName "Control.Monad.Incremental.Outside") [typeTree (Proxy::Proxy a)]]
		where args = [typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy a)]
instance (DeepTypeable inc,DeepTypeable a) => DeepTypeable (Inside inc a) where
	typeTree (_::Proxy (Inside inc a)) = MkTypeTree (mkName "Control.Monad.Incremental.Inside") args [MkConTree (mkName "Control.Monad.Incremental.Inside") [typeTree (Proxy::Proxy a)]]
		where args = [typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy a)]

instance DeepTypeable Inside where
	typeTree _ = MkTypeTree (mkName "Control.Monad.Incremental.Inside") [] []

instance DeepTypeable Outside where
	typeTree _ = MkTypeTree (mkName "Control.Monad.Incremental.Outside") [] []

proxyInside = (Proxy::Proxy Inside)
proxyOutside = (Proxy::Proxy Outside)

deriving instance Typeable Strict.Pair

debugInc :: Layer l inc => String -> l inc a -> l inc a
debugInc str m = do
	threadid <- unsafeIOToInc $ myThreadId
	debug ("{"++ show threadid ++ "}" ++ str) m
	
-- * Memoization

-- for evaluation-order reasons related to @gfoldl@ we need to treat layer computations as traversable types, but for correctness of IC we can't memoize them
instance (Typeable inc,Typeable a) => Memo (Inside inc a) where
	type Key (Inside incr a) = Neq
	{-# INLINE memoKey #-}
	memoKey m = Neq
	{-# INLINE memoWeak #-}
	memoWeak = \x -> MkWeak mkDeadWeak

-- for evaluation-order reasons related to @gfoldl@ we need to treat layer computations as traversable types, but for correctness of IC we can't memoize them
instance (Typeable inc,Typeable a) => Memo (Outside inc a) where
	type Key (Outside incr a) = Neq
	{-# INLINE memoKey #-}
	memoKey m = Neq
	{-# INLINE memoWeak #-}
	memoWeak = \x -> MkWeak mkDeadWeak


data MemoPolicy = MemoLinear -- stores at most as much space as a single run of the program; users have to explicitely retain output thunks for them to remain memoized
			  | MemoSuperlinear -- stores all previous executions for live inputs; users do not have to explicitely retain output thunks for them to remain memoized


-- non-incremental thunk

newtype T (l :: * -> * -> *) inc a = T { unT :: l inc a } deriving Typeable

instance Eq (T l inc a) where
	t1 == t2 = stableName t1 == stableName t2

instance (Layer l inc) => Thunk T l inc where
	new = return . T
	{-# INLINE new #-}
	newc = return . T . return
	{-# INLINE newc #-}
	read = unT
	{-# INLINE read #-}

instance (Layer l inc) => Output T l inc where
	thunk = return . T
	{-# INLINE thunk #-}
	const = return . T . return
	{-# INLINE const #-}
	force = unT
	{-# INLINE force #-}

instance DeepTypeable T where
	typeTree _ = MkTypeTree (mkName "Control.Monad.Incremental.T") [] []

instance (DeepTypeable l,DeepTypeable inc,DeepTypeable a) => DeepTypeable (T l inc a) where
	typeTree (_ :: Proxy (T l inc a)) = MkTypeTree (mkName "Control.Monad.Incremental.T") args [MkConTree (mkName "Control.Monad.Incremental.mod") [typeTree (Proxy::Proxy a)]]
		where args = [typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy a)]

$(deriveMemo ''T)
