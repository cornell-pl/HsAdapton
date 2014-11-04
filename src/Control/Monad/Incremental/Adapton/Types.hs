{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, GADTs, RankNTypes, ImpredicativeTypes, DeriveDataTypeable, StandaloneDeriving, KindSignatures, MagicHash #-}

module Control.Monad.Incremental.Adapton.Types where

import Data.Unique
import System.Mem.Weak
import GHC.Prim
import Control.Monad.Ref
import Data.Typeable
import Control.Monad.IO.Class
import Data.IORef
import Data.IntMap (IntMap(..))
import qualified Data.IntMap as IntMap
import System.Mem.WeakSet
import Control.Monad.Incremental
import Data.Strict.List
import Data.DeepTypeable
import Language.Haskell.TH
import Data.WithClass.MData

-- * Thunks

-- | Thunk of @a@
newtype U (l :: * -> (* -> *) -> (* -> *) -> * -> *) inc (r :: * -> *) (m :: * -> *) a = U (
		r (UData l inc r m a) -- data
	,	NodeMeta inc r m		-- metadata
	)

deriving instance Typeable U

instance DeepTypeable U where
	typeTree _ = MkTypeTree (mkName "Control.Monad.Incremental.Adapton.Types.U") [] []


instance (DeepTypeable l,DeepTypeable inc,DeepTypeable r,DeepTypeable m,DeepTypeable a) => DeepTypeable (U l inc r m a) where
	typeTree (_ :: Proxy (U l inc r m a)) = MkTypeTree (mkName "Control.Monad.Incremental.Adapton.Types.U") args [MkConTree (mkName "Control.Monad.Incremental.Adapton.thunk") [typeTree (Proxy::Proxy a)]]
		where args = [typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy r),typeTree (Proxy::Proxy m),typeTree (Proxy::Proxy a)]

{-# INLINE dataU #-}
dataU :: U l inc r m a -> r (UData l inc r m a)
dataU (U (d,_)) = d
{-# INLINE metaU #-}
metaU :: U l inc r m a -> NodeMeta inc r m
metaU (U (_,m)) = m

-- unboxed booleans
type UBool = Int#

data UData (l :: * -> (* -> *) -> (* -> *) -> * -> *) inc (r :: * -> *) (m :: * -> *) a =
	  Value !UBool !a !(l inc r m a) !(r (Dependencies inc r m)) -- a thunk that has been previously evaluated; invariant: the node is dirty if any of its dependencies is dirty; the dirty flag is just an optimization
	| Thunk !(l inc r m a) -- a thunk that has never been evaluated
	| Const !a -- a constant value
	
{-# INLINE valueUD #-}
valueUD :: UData l inc r m a -> Maybe a
valueUD (Value _ v _ _) = Just v
valueUD (Const v) = Just v
valueUD _ = Nothing
{-# INLINE forceUD #-}
forceUD :: UData l inc r m a -> l inc r m a
forceUD (Value _ _ f _) = f
forceUD (Thunk f) = f
{-# INLINE dependenciesUD #-}
dependenciesUD :: UData l inc r m a -> r (Dependencies inc r m)
dependenciesUD (Value _ _ _ d) = d
	
-- | A list of (bidirectional) dependencies from callers to callees
type Dependencies inc r m = [(Dependency inc r m,IO ())] -- weak pointers to make dependencies bidirectional, and allow finalization of a dependency relationship to kill the coupled dependent relationship
newtype Dependency inc (r :: * -> *) (m :: * -> *) = Dependency (
		NodeMeta inc r m -- the metadata of the source node
	,	r Bool -- dirty flag
	,	Inside inc r m Bool -- a checking condition with the previously seen value of the source node;
	,	NodeMeta inc r m -- the metadata of the target node
	)

type WNodeMeta inc r m = Weak (NodeMeta inc r m)

{-# INLINE srcMetaW #-}
srcMetaW :: Dependency inc r m -> NodeMeta inc r m
srcMetaW (Dependency (m,_,_,_)) = m
{-# INLINE dirtyW #-}
dirtyW :: Dependency inc r m -> r Bool
dirtyW (Dependency (_,d,_,_)) = d
{-# INLINE checkW #-}
checkW :: Dependency inc r m -> Inside inc r m Bool
checkW (Dependency (_,_,c,_)) = c
{-# INLINE tgtMetaW #-}
tgtMetaW :: Dependency inc r m -> (NodeMeta inc r m)
tgtMetaW (Dependency (_,_,_,m)) = m
	
type Dependents inc (r :: * -> *) (m :: * -> *) = WeakSet (Dependent inc r m) -- order does not really matter, semantically this is a set
type Dependent inc r m = Dependency inc r m -- weak pointers allow our algorithm to postpone removing them from the list
	
-- * Strict Modifiables

newtype M (l :: * -> (* -> *) -> (* -> *) -> * -> *) inc (r :: * -> *) (m :: * -> *) a = M (
		r a -- a constant value
	,	NodeMeta inc r m
	)

deriving instance Typeable M

instance DeepTypeable M where
	typeTree _ = MkTypeTree (mkName "Control.Monad.Incremental.Adapton.Types.M") [] []

instance (DeepTypeable l,DeepTypeable inc,DeepTypeable r,DeepTypeable m,DeepTypeable a) => DeepTypeable (M l inc r m a) where
	typeTree (_ :: Proxy (M l inc r m a)) = MkTypeTree (mkName "Control.Monad.Incremental.Adapton.Types.M") args [MkConTree (mkName "Control.Monad.Incremental.Adapton.ref") [typeTree (Proxy::Proxy a)]]
		where args = [typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy r),typeTree (Proxy::Proxy m),typeTree (Proxy::Proxy a)]

{-# INLINE dataM #-}
dataM :: M l inc r m a -> r a
dataM (M (d,_)) = d
{-# INLINE metaM #-}
metaM :: M l inc r m a -> NodeMeta inc r m
metaM (M (_,m)) = m

-- * Lazy modifiables

newtype L (l :: * -> (* -> *) -> (* -> *) -> * -> *) inc (r :: * -> *) (m :: * -> *) a = L (
		r (LData l inc r m a)
	,	NodeMeta inc r m
	)

deriving instance Typeable L

instance DeepTypeable L where
	typeTree _ = MkTypeTree (mkName "Control.Monad.Incremental.Adapton.Types.L") [] []

instance (DeepTypeable l,DeepTypeable inc,DeepTypeable r,DeepTypeable m,DeepTypeable a) => DeepTypeable (L l inc r m a) where
	typeTree (_ :: Proxy (L l inc r m a)) = MkTypeTree (mkName "Control.Monad.Incremental.Adapton.Types.L") args [MkConTree (mkName "Control.Monad.Incremental.Adapton.ref") [typeTree (Proxy::Proxy a)]]
		where args = [typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy r),typeTree (Proxy::Proxy m),typeTree (Proxy::Proxy a)]

{-# INLINE dataL #-}
dataL :: L l inc r m a -> r (LData l inc r m a)
dataL (L (d,_)) = d
{-# INLINE metaL #-}
metaL :: L l inc r m a -> NodeMeta inc r m
metaL (L (_,m)) = m

-- the boolean indicates whether the lazy modifiable has ever been forced (this information is never used by Adapton, but it may be useful to know)
data LData l inc (r :: * -> *) (m :: * -> *) a =
	  LThunk !UBool !(l inc r m a) -- an unevaluated thunk
	| LConst !UBool !a -- a constant value

-- * Common metadata for all kinds of nodes

-- the only mutable information about a node are the dependents
newtype NodeMeta inc (r :: * -> *) (m :: * -> *) = NodeMeta (
		Unique
	,	Dependents inc r m -- list of dependents; we purge the list at finalization time
	,	m () -- function that dirties the corresponding value (thunks only)
	,	m () -- function that forgets all the old cached thunk data (thunks only)
	,   Maybe (Creator inc r m) -- the parent thunk under which the reference was created (modifiables only)
	,	UDataOp inc r m -- generic polymorphic function over the data (thunks only) (used only for debugging)
	)

instance Show (NodeMeta inc r m) where
	show nm = "NodeMeta"++show (idNM nm)

data UDataOp inc r m where
	 UDataOp :: (MonadIO m) => (forall b . b -> (forall l a . r (UData l inc r m a) -> m b) -> m b) -> UDataOp inc r m

mkEmptyUDataOp :: MonadIO m => UDataOp inc r m
mkEmptyUDataOp = UDataOp $ \x f -> return x

mkUDataOp :: MonadIO m => r (UData l inc r m a) -> UDataOp inc r m
mkUDataOp dta = UDataOp $ \x f -> f dta

mkUDataOpWeak :: MonadIO m => Weak (r (UData l inc r m a)) -> UDataOp inc r m
mkUDataOpWeak wdta = UDataOp $ \x f -> do
	mb <- liftIO $ deRefWeak wdta
	case mb of
		Just dta -> f dta
		Nothing -> return x

applyUDataOp :: UDataOp inc r m -> b -> (forall l a . r (UData l inc r m a) -> m b) -> m b
applyUDataOp (UDataOp f) b g = f b g

type Creator inc r m = WNodeMeta inc r m

{-# INLINE idNM #-}
idNM :: NodeMeta inc r m -> Unique
idNM (NodeMeta (i,_,_,_,_,_)) = i
{-# INLINE idU #-}
idU :: U l inc r m a -> Unique
idU = idNM . metaU
{-# INLINE dependentsNM #-}
dependentsNM :: NodeMeta inc r m -> (Dependents inc r m)
dependentsNM (NodeMeta (_,deps,_,_,_,_)) = deps
{-# INLINE dirtyUM #-}
dirtyUM :: NodeMeta inc r m -> m ()
dirtyUM (NodeMeta (_,_,d,_,_,_)) = d
{-# INLINE forgetUM #-}
forgetUM :: NodeMeta inc r m -> m ()
forgetUM (NodeMeta (_,_,_,forget,_,_)) = forget
{-# INLINE creatorNM #-}
creatorNM :: NodeMeta inc r m -> Maybe (Creator inc r m)
creatorNM (NodeMeta (_,_,_,_,creator,_)) = creator
{-# INLINE uDataOpUM #-}
uDataOpUM :: NodeMeta inc r m -> UDataOp inc r m
uDataOpUM (NodeMeta (_,_,_,_,_,d)) = d

addFinalizerU :: MonadIO m => U l inc r m a -> m () -> U l inc r m a
addFinalizerU (U (dta,NodeMeta (idU,dependentsU,dirtyU,forgetU,creatorU,d))) finalizeThunk = (U (dta,NodeMeta (idU,dependentsU,dirtyU,forgetU >> finalizeThunk,creatorU,d)))

-- * Auxiliary functions

instance Eq (U l inc r m a) where
	t1 == t2 = idNM (metaU t1) == idNM (metaU t2)
instance Eq (M l inc r m a) where
	t1 == t2 = idNM (metaM t1) == idNM (metaM t2)
instance Eq (L l inc r m a) where
	t1 == t2 = idNM (metaL t1) == idNM (metaL t2)

mapRef :: MonadRef r m => (a -> a) -> r a -> m ()
mapRef f r = readRef r >>= writeRef r . f

mapRefM_ :: MonadRef r m => (a -> m a) -> r a -> m ()
mapRefM_ f r = readRef r >>= f >>= writeRef r

--intMapM_ :: Monad m => (a -> m ()) -> IntMap a -> m ()
--intMapM_ f = IntMap.foldr (\a m -> f a >> m) (return ())

