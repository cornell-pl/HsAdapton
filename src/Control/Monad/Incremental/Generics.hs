{-# LANGUAGE TypeFamilies, TypeOperators, TemplateHaskell, RankNTypes, ScopedTypeVariables, FlexibleContexts, KindSignatures #-}

module Control.Monad.Incremental.Generics where

import Control.Monad.Incremental
import Data.WithClass.MGenerics.Aliases
import Data.WithClass.MGenerics.Schemes
import Data.WithClass.MData
import Control.Monad.IO.Class
import System.Mem.MemoTable
import System.Mem.WeakRef
import Control.Monad.Incremental.List
import Control.Monad

newtype Gfoldl ctx m c = Gfoldl { unGfoldl :: forall d b. MData ctx m d => c (m d -> m b) -> m d -> m (c b) }

gfoldlInc  :: MData ctx m a => Proxy ctx -> Gfoldl ctx m c -> (forall g. g -> m (c g)) -> a -> m (c a)
gfoldlInc ctx f g x = gfoldl ctx (unGfoldl f) g x

-- ** Generic Transformations (type-preserving)

-- | copies an arbitrary data structure with thunks, generating new thunks
-- for non-output thunks, the new thunks are not dependent on the original ones
copyInc :: Layer l inc r m => Proxy ctx -> GenericT ctx (l inc r m)
copyInc ctx = everywhere ctx return

-- ** Generic Queries (type-unifying)

-- | Summarise all nodes in top-down, left-to-right order, incrementally
-- Unlike the non-incremental @everything@, we don't build an intermediate list
everythingInc :: (Eq b,Output thunk l inc r m,MonadIO m) => Proxy ctx -> (thunk l inc r m b -> l inc r m b -> l inc r m b) -> GenericQMemo ctx thunk l inc r m b -> GenericQMemo ctx thunk l inc r m b
everythingInc ctx k f = gmemoQ ctx (everythingInc' ctx k f)
	where
	everythingInc' :: (Eq b,Output thunk l inc r m) => Proxy ctx -> (thunk l inc r m b -> l inc r m b -> l inc r m b) -> GenericQMemo ctx thunk l inc r m b -> GenericQMemo ctx thunk l inc r m b -> GenericQMemo ctx thunk l inc r m b
	everythingInc' ctx k (GenericQMemo f) every = GenericQMemo $ \x -> f x >>= \b -> gmapQrInc ctx k (force b) every x

-- | Variation of @everythingInc@ with an added stop condition
everythingButInc :: (Eq b,Output thunk l inc r m,MonadIO m) => Proxy ctx -> (thunk l inc r m b -> l inc r m b -> l inc r m b) -> GenericQMemoBut ctx thunk l inc r m b -> GenericQMemo ctx thunk l inc r m b
everythingButInc ctx k f = gmemoQ ctx (everythingButInc' ctx k f)
	where
	everythingButInc' :: (Eq b,Output thunk l inc r m) => Proxy ctx -> (thunk l inc r m b -> l inc r m b -> l inc r m b) -> GenericQMemoBut ctx thunk l inc r m b -> GenericQMemo ctx thunk l inc r m b -> GenericQMemo ctx thunk l inc r m b
	everythingButInc' ctx k (GenericQMemoBut f) every = GenericQMemo $ \x -> do
		(b,stop) <- f x
		if stop
			then return b
			else gmapQrInc ctx k (force b) every x

newtype GenericQMemoBut ctx (thunk :: (* -> (* -> *) -> (* -> *) -> * -> *) -> * -> (* -> *) -> (* -> *) -> * -> *) l inc r m b = GenericQMemoBut {
		unGenericQMemoBut :: GenericQ (MemoCtx ctx) (l inc r m) (thunk l inc r m b,Bool)
	}

-- | Make an incremental generic query from a type-specific case
mkQInc :: (Eq c,Output thunk l inc r m,Typeable a,Typeable b,Monad m) => l inc r m c -> (b -> l inc r m c) -> a -> l inc r m (thunk l inc r m c)
mkQInc z f x = thunk $ z >>= \c -> mkQ c f x

-- | Make an incremental generic query from a type-specific case, with a selective traversal that ignores all types that do not contain @b@
-- this delays a decision about @InType b a@ 
mkQButTypeInc :: (Eq c,Output thunk l inc r m,DeepTypeable a,DeepTypeable b,Monad m) => l inc r m c -> (b -> l inc r m c) -> a -> l inc r m (thunk l inc r m c,Bool)
mkQButTypeInc z (f :: b -> l inc r m c) x = do
	thunk <- mkQInc z f x
	let stop = not $ inDeepTypeable (Proxy::Proxy b) (proxyOf x)
	return (thunk,stop)

newtype QrInc (thunk :: (* -> (* -> *) -> (* -> *) -> * -> *) -> * -> (* -> *) -> (* -> *) -> * -> *) l inc r m b a = QrInc { unQrInc  :: l inc r m b -> l inc r m (thunk l inc r m b) }

-- makes sure that all forces happen within an outer thunk
gmapQrInc :: (Eq b',MData (MemoCtx ctx) (l inc r m) a,Output thunk l inc r m,Eq b) => Proxy ctx -> (thunk l inc r m b' -> l inc r m b -> l inc r m b) -> l inc r m b -> GenericQMemo ctx thunk l inc r m b' -> a -> l inc r m (thunk l inc r m b)
gmapQrInc ctx o r0 f x0 = gfoldlInc (proxyMemoCtx ctx) (k ctx o f) z x0 >>= \q -> unQrInc q r0
	where
	k :: (Eq b',Eq b,Output thunk l inc r m) => Proxy ctx -> (thunk l inc r m b' -> l inc r m b -> l inc r m b) -> GenericQMemo ctx thunk l inc r m b' -> Gfoldl (MemoCtx ctx) (l inc r m) (QrInc thunk l inc r m b)
	k ctx o f = Gfoldl $ \(QrInc c) mx -> return $ QrInc $ \mr -> c $ do
		r2 <- mx >>= unGenericQMemo f
		o r2 mr
	z :: (Output thunk l inc r m,Eq b) => g -> l inc r m (QrInc thunk l inc r m b g)
	z _ = return $ QrInc thunk

-- | generic query that sums all @Int@ values in a type
gsumInc :: (MData (MemoCtx NoCtx) (l inc r m) a,Output thunk l inc r m,MonadIO m) => a -> l inc r m (thunk l inc r m Int)
gsumInc a = do
	let zero = return 0
	unGenericQMemo (everythingButInc proxyNoCtx (\tx my -> force tx >>= \x -> liftM (x+) my) (q zero)) a
  where
	q zero = GenericQMemoBut $ mkQButTypeInc zero (\(i::Int) -> return i)

-- | generic query that sums all @Int@ values in a type
listifyInc :: (Eq b,Eq (JoinListMod thunk l inc r m b),DeepTypeable b,Output thunk l inc r m,MonadIO m) => Proxy ctx -> (b -> l inc r m Bool) -> GenericQMemo ctx thunk l inc r m (JoinListMod' thunk l inc r m b)
listifyInc ctx (p:: b -> l inc r m Bool) = GenericQMemo $ \a -> do
	let zero = return EmptyMod
	unGenericQMemo (everythingButInc ctx (\tx my -> thunk my >>= \ty -> return $ JoinMod tx ty) (q zero)) a
  where
	q zero = GenericQMemoBut $ mkQButTypeInc zero (\(b::b) -> p b >>= \cond -> if cond then return (SingleMod b) else return EmptyMod)
