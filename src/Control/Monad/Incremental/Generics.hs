{-# LANGUAGE ConstraintKinds, StandaloneDeriving, DeriveDataTypeable, TypeFamilies, TypeOperators, TemplateHaskell, RankNTypes, ScopedTypeVariables, FlexibleContexts, KindSignatures #-}

module Control.Monad.Incremental.Generics where

import Control.Monad.Incremental
import Data.WithClass.MGenerics.Aliases
import Data.WithClass.MGenerics.Schemes
import Data.WithClass.MData
import Control.Monad.IO.Class
import System.Mem.MemoTable

import Control.Monad.Incremental.List
import Control.Monad

-- ** Generic Transformations (type-preserving)

-- | copies an arbitrary data structure with thunks, generating new thunks
-- for non-output thunks, the new thunks are not dependent on the original ones
copyInc :: Layer l inc => Proxy ctx -> GenericT ctx (l inc)
copyInc ctx = everywhere ctx return

-- ** Generic Queries (type-unifying)

-- | Summarise all nodes in top-down, left-to-right order, incrementally
-- Unlike the non-incremental @everything@, we don't build an intermediate list
everythingInc :: (IncK inc b,Typeable ctx,Output thunk l inc) => Proxy ctx -> (thunk l inc b -> l inc b -> l inc b) -> GenericQMemo ctx thunk l inc b -> GenericQMemo ctx thunk l inc b
everythingInc ctx k f = gmemoQ ctx (\every x -> f x >>= \b -> gmapQrInc ctx k (force b) every x)

-- | Variation of @everythingInc@ with an added stop condition
everythingButInc :: (IncK inc b,Typeable ctx,Output thunk l inc) => Proxy ctx -> (thunk l inc b -> l inc b -> l inc b) -> GenericQMemoBut ctx thunk l inc b -> GenericQMemo ctx thunk l inc b
everythingButInc ctx k f = gmemoQ ctx (\every x -> f x >>= \(b,stop) -> if stop then return b else gmapQrInc ctx k (force b) every x)

type GenericQMemoBut ctx (thunk :: (* -> * -> *) -> * -> * -> *) l inc b = GenericQ (MemoCtx ctx) (l inc) (thunk l inc b,Bool)

-- | Make an incremental generic query from a type-specific case
mkQInc :: (IncK inc c,Output thunk l inc,Typeable a,Typeable b) => l inc c -> (b -> l inc c) -> a -> l inc (thunk l inc c)
mkQInc z f x = thunk $ z >>= \c -> mkQ c f x

-- | Make an incremental generic query from a type-specific case, with a selective traversal that ignores all types that do not contain @b@
mkQButTypeInc :: (IncK inc c,Output thunk l inc,DeepTypeable a,DeepTypeable b) => l inc c -> (b -> l inc c) -> a -> l inc (thunk l inc c,Bool)
mkQButTypeInc z (f :: b -> l inc c) x = do
	thunk <- mkQInc z f x
	let stop = not $ inDeepTypeable (Proxy::Proxy b) (proxyOf x)
	return (thunk,stop)

newtype QrInc (thunk :: (* -> * -> *) -> * -> * -> *) l inc b a = QrInc { unQrInc  :: l inc b -> l inc (thunk l inc b) }

-- makes sure that all forces happen within an outer thunk
gmapQrInc :: (IncK inc b,MData (MemoCtx ctx) (l inc) a,Output thunk l inc) => Proxy ctx -> (thunk l inc b' -> l inc b -> l inc b) -> l inc b -> GenericQMemo ctx thunk l inc b' -> a -> l inc (thunk l inc b)
gmapQrInc ctx o r0 f x0 = gfoldl (proxyMemoCtx ctx)
	(\(QrInc c) mx -> return $ QrInc $ \mr -> c $ mx >>= f >>= flip o mr)
	(\_ -> return $ QrInc thunk) x0 >>= \q -> unQrInc q r0

-- | generic query that sums all @Int@ values in a type
gsumInc :: (IncK inc Int,MData (MemoCtx NoCtx) (l inc) a,Output thunk l inc) => a -> l inc (thunk l inc Int)
gsumInc = everythingButInc proxyNoCtx
	(\tx my -> force tx >>= \x -> liftM (x+) my)
	(mkQButTypeInc (return 0) return)

-- | generic query that collects all values that satisfy a predicate into a list
listifyInc :: (Typeable ctx,IncK inc (JoinListMod' thunk l inc b),DeepTypeable b,Output thunk l inc) => Proxy ctx -> (b -> l inc Bool) -> GenericQMemo ctx thunk l inc (JoinListMod' thunk l inc b)
listifyInc ctx p = everythingButInc ctx
	(\tx my -> thunk my >>= \ty -> return $ JoinMod tx ty)
	(mkQButTypeInc (return EmptyMod) (\b -> p b >>= \cond -> if cond then return (SingleMod b) else return EmptyMod))

deriving instance Typeable NoCtx



