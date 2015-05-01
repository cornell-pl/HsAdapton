{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, GADTs, RankNTypes, ImpredicativeTypes, DeriveDataTypeable, StandaloneDeriving, KindSignatures, MagicHash #-}

module Control.Monad.Incremental.Internal.Adapton.Types where

import Data.Unique
import System.Mem.Weak
import GHC.Prim

import Data.Typeable
import Control.Monad.IO.Class
import Data.IORef
import Data.IntMap (IntMap(..))
import qualified Data.IntMap as IntMap
import Control.Monad.Incremental
import Data.Strict.List
import Data.DeepTypeable
import Language.Haskell.TH
import Data.WithClass.MData
import Control.Monad
import System.Mem.WeakMap

-- * Thunks

-- | Thunk of @a@
newtype U (l :: * -> * -> *) inc a = U (
		IORef (UData l inc a) -- data
	,	NodeMeta		-- metadata
	)

deriving instance Typeable U

instance DeepTypeable U where
	typeTree _ = MkTypeTree (mkName "Control.Monad.Incremental.Adapton.Types.U") [] []

instance (DeepTypeable l,DeepTypeable inc,DeepTypeable a) => DeepTypeable (U l inc a) where
	typeTree (_ :: Proxy (U l inc a)) = MkTypeTree (mkName "Control.Monad.Incremental.Adapton.Types.U") args [MkConTree (mkName "Control.Monad.Incremental.Adapton.thunk") [typeTree (Proxy::Proxy a)]]
		where args = [typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy a)]

{-# INLINE dataU #-}
dataU :: U l inc a -> IORef (UData l inc a)
dataU (U (d,_)) = d
{-# INLINE metaU #-}
metaU :: U l inc a -> NodeMeta
metaU (U (_,m)) = m

-- unboxed booleans
type UBool = Int#

data UData (l :: * -> * -> *) inc a =
	  Value !UBool !a !(l inc a) !(IORef (Dependencies)) -- a thunk that has been previously evaluated; invariant: the node is dirty if any of its dependencies is dirty; the dirty flag is just an optimization
	| Thunk !(l inc a) -- a thunk that has never been evaluated
	| Const !a -- a constant value
	
{-# INLINE valueUD #-}
valueUD :: UData l inc a -> Maybe a
valueUD (Value _ v _ _) = Just v
valueUD (Const v) = Just v
valueUD _ = Nothing
{-# INLINE forceUD #-}
forceUD :: UData l inc a -> l inc a
forceUD (Value _ _ f _) = f
forceUD (Thunk f) = f
{-# INLINE dependenciesUD #-}
dependenciesUD :: UData l inc a -> IORef (Dependencies)
dependenciesUD (Value _ _ _ d) = d
	
-- | A list of (bidirectional) dependencies from callers to callees
type Dependencies = [(Dependency,IO ())] -- weak pointers to make dependencies bidirectional, and allow finalization of a dependency relationship to kill the coupled dependent relationship
newtype Dependency = Dependency (
		NodeMeta -- the metadata of the source node
	,	IORef Bool -- dirty flag
	,	Inside Adapton Bool -- a checking condition with the previously seen value of the source node;
	,	NodeMeta -- the metadata of the target node
	) deriving Typeable

type WNodeMeta = Weak NodeMeta

{-# INLINE srcMetaW #-}
srcMetaW :: Dependency -> NodeMeta
srcMetaW (Dependency (m,_,_,_)) = m
{-# INLINE dirtyW #-}
dirtyW :: Dependency -> IORef Bool
dirtyW (Dependency (_,d,_,_)) = d
{-# INLINE checkW #-}
checkW :: Dependency -> Inside Adapton Bool
checkW (Dependency (_,_,c,_)) = c
{-# INLINE tgtMetaW #-}
tgtMetaW :: Dependency -> (NodeMeta)
tgtMetaW (Dependency (_,_,_,m)) = m
	
type Dependents = WeakMap Unique Dependent -- order does not really matter, semantically this is a set
type Dependent = Dependency -- weak pointers allow our algorithm to postpone removing them from the list
	
-- * Strict Modifiables

newtype M (l :: * -> * -> *) inc a = M (
		IORef a -- a constant value
	,	NodeMeta
	)

deriving instance Typeable M

instance DeepTypeable M where
	typeTree _ = MkTypeTree (mkName "Control.Monad.Incremental.Adapton.Types.M") [] []

instance (DeepTypeable l,DeepTypeable inc,DeepTypeable a) => DeepTypeable (M l inc a) where
	typeTree (_ :: Proxy (M l inc a)) = MkTypeTree (mkName "Control.Monad.Incremental.Adapton.Types.M") args [MkConTree (mkName "Control.Monad.Incremental.Adapton.ref") [typeTree (Proxy::Proxy a)]]
		where args = [typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy a)]

{-# INLINE dataM #-}
dataM :: M l inc a -> IORef a
dataM (M (d,_)) = d
{-# INLINE metaM #-}
metaM :: M l inc a -> NodeMeta
metaM (M (_,m)) = m

-- * Lazy modifiables

newtype L (l :: * -> * -> *) inc a = L (
		IORef (LData l inc a)
	,	NodeMeta
	)

deriving instance Typeable L

instance DeepTypeable L where
	typeTree _ = MkTypeTree (mkName "Control.Monad.Incremental.Adapton.Types.L") [] []

instance (DeepTypeable l,DeepTypeable inc,DeepTypeable a) => DeepTypeable (L l inc a) where
	typeTree (_ :: Proxy (L l inc a)) = MkTypeTree (mkName "Control.Monad.Incremental.Adapton.Types.L") args [MkConTree (mkName "Control.Monad.Incremental.Adapton.ref") [typeTree (Proxy::Proxy a)]]
		where args = [typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy a)]

{-# INLINE dataL #-}
dataL :: L l inc a -> IORef (LData l inc a)
dataL (L (d,_)) = d
{-# INLINE metaL #-}
metaL :: L l inc a -> NodeMeta
metaL (L (_,m)) = m

-- the boolean indicates whether the lazy modifiable has ever been forced (this information is never used by Adapton, but it may be useful to know)
data LData l inc a =
	  LThunk !UBool !(l inc a) -- an unevaluated thunk
	| LConst !UBool !a -- a constant value

-- * Common metadata for all kinds of nodes

-- the only mutable information about a node are the dependents
newtype NodeMeta = NodeMeta (
		Unique
	,	Dependents -- list of dependents; we purge the list at finalization time
	,	IO () -- function that dirties the corresponding value (thunks only)
	,	IO () -- function that forgets all the old cached thunk data (thunks only)
	,   Maybe (Creator) -- the parent thunk under which the reference was created (modifiables only)
	) deriving Typeable

instance Show (NodeMeta) where
	show nm = "NodeMeta"++show (idNM nm)

type Creator = WNodeMeta

{-# INLINE idNM #-}
idNM :: NodeMeta -> Unique
idNM (NodeMeta (i,_,_,_,_)) = i
{-# INLINE idU #-}
idU :: U l inc a -> Unique
idU = idNM . metaU
{-# INLINE dependentsNM #-}
dependentsNM :: NodeMeta -> (Dependents)
dependentsNM (NodeMeta (_,deps,_,_,_)) = deps
{-# INLINE dirtyUM #-}
dirtyUM :: NodeMeta -> IO ()
dirtyUM (NodeMeta (_,_,d,_,_)) = d
{-# INLINE forgetUM #-}
forgetUM :: NodeMeta -> IO ()
forgetUM (NodeMeta (_,_,_,forget,_)) = forget
{-# INLINE creatorNM #-}
creatorNM :: NodeMeta -> Maybe (Creator)
creatorNM (NodeMeta (_,_,_,_,creator)) = creator

addFinalizerU :: U l inc a -> IO () -> U l inc a
addFinalizerU (U (dta,NodeMeta (idU,dependentsU,dirtyU,forgetU,creatorU))) finalizeThunk = (U (dta,NodeMeta (idU,dependentsU,dirtyU,forgetU >> finalizeThunk,creatorU)))

-- * Auxiliary functions

instance Eq (U l inc a) where
	t1 == t2 = idNM (metaU t1) == idNM (metaU t2)
instance Eq (M l inc a) where
	t1 == t2 = idNM (metaM t1) == idNM (metaM t2)
instance Eq (L l inc a) where
	t1 == t2 = idNM (metaL t1) == idNM (metaL t2)

--intMapM_ :: Monad m => (a -> m ()) -> IntMap a -> m ()
--intMapM_ f = IntMap.foldr (\a m -> f a >> m) (return ())

-- | Type for Adapton incremental computation
data Adapton deriving Typeable


-- | A plain thunk type with explicit sharing but is not mutable nor incrementally updated
newtype S (l :: * -> * -> *) inc a = S (IORef (l inc a)) deriving Typeable

instance (Layer l Adapton) => Thunk S l Adapton where
	new m = unsafeIOToInc $ liftM S $ newIORef m
	read (S r) =  do
		m <- unsafeIOToInc $ readIORef r
		x <- m
		unsafeIOToInc $ writeIORef r $! return x
		return x