{-# LANGUAGE TypeFamilies, ScopedTypeVariables, StandaloneDeriving, KindSignatures, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances, DeriveDataTypeable #-}

module Control.Monad.Incremental.Tree where

import System.Mem.WeakTable
import Data.WithClass.MData
import Control.Monad.Incremental
import Data.Proxy
import Control.Monad.Incremental.Display
import Control.Monad.Incremental.LazyNonInc
import Control.Monad.Incremental.Adapton hiding (MData)
import Data.IORef

import Data.DeriveTH                 -- Library for deriving instances for existing types
import Data.DeepTypeable
import Data.WithClass.Derive.DeepTypeable
import Language.Haskell.TH.Syntax hiding (lift,Infix,Fixity)

import System.Mem.StableName
import System.IO.Unsafe
import System.Mem.WeakKey
import System.Mem.Weak as Weak
import Prelude hiding (mod,const,read)

type TreeMod
	(mod :: ((* -> (* -> *) -> (* -> *) -> * -> *) -> * -> (* -> *) -> (* -> *) -> * -> *))
	(l :: * -> (* -> *) -> (* -> *) -> * -> *)
	inc
	(r :: * -> *)
	(m :: * -> *)
	a
	= mod l inc r m (TreeMod' mod l inc r m a)

data TreeMod'
	(mod :: ((* -> (* -> *) -> (* -> *) -> * -> *) -> * -> (* -> *) -> (* -> *) -> * -> *))
	(l :: * -> (* -> *) -> (* -> *) -> * -> *)
	inc
	(r :: * -> *)
	(m :: * -> *)
	a
	= EmptyMod | BinMod a (TreeMod mod l inc r m a) (TreeMod mod l inc r m a)

deriving instance Typeable TreeMod'
deriving instance (Eq a,Eq (TreeMod mod l inc r m a)) => Eq (TreeMod' mod l inc r m a)

instance (DeepTypeable mod,DeepTypeable l,DeepTypeable inc,DeepTypeable r,DeepTypeable m,DeepTypeable a,DeepTypeable (TreeMod mod l inc r m a)) => DeepTypeable (TreeMod' mod l inc r m a) where
	typeTree (_::Proxy (TreeMod' mod l inc r m a)) = MkTypeTree (mkName "Control.Monad.Incremental.Tree.TreeMod'") args [MkConTree (mkName "Control.Monad.Incremental.Tree.EmptyMod") [],MkConTree (mkName "Control.Monad.Incremental.Tree.BinMod") [typeTree (Proxy::Proxy a),typeTree (Proxy::Proxy (TreeMod mod l inc r m a)),typeTree (Proxy::Proxy (TreeMod mod l inc r m a))]]
		where args = [typeTree (Proxy::Proxy mod),typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy r),typeTree (Proxy::Proxy m),typeTree (Proxy::Proxy a)]

type TreeU l r m a = TreeMod U l Adapton r m a
type TreeU' l r m a = TreeMod' U l Adapton r m a

type TreeM l r m a = TreeMod M l Adapton r m a
type TreeM' l r m a = TreeMod' M l Adapton r m a

type TreeL l r m a = TreeMod L l Adapton r m a
type TreeL' l r m a = TreeMod' L l Adapton r m a

type TreeLazyNonIncU l r m a = TreeMod LazyNonIncU l LazyNonInc r m a
type TreeLazyNonIncU' l r m a = TreeMod' LazyNonIncU l LazyNonInc r m a

type TreeLazyNonIncL l r m a = TreeMod LazyNonIncL l LazyNonInc r m a
type TreeLazyNonIncL' l r m a = TreeMod' LazyNonIncL l LazyNonInc r m a

type TreeLazyNonIncM l r m a = TreeMod LazyNonIncM l LazyNonInc r m a
type TreeLazyNonIncM' l r m a = TreeMod' LazyNonIncM l LazyNonInc r m a

instance (Display l1 inc r m a,Display l1 inc r m (TreeMod mod l inc r m a)) => Display l1 inc r m (TreeMod' mod l inc r m a) where
	displaysPrec proxyL proxyInc proxyR proxyM EmptyMod r = return $ "EmptyMod" ++ r
	displaysPrec proxyL proxyInc proxyR proxyM (BinMod x ml mr) rest = do
		sr <- displaysPrec proxyL proxyInc proxyR proxyM mr (')':rest)
		sl <- displaysPrec proxyL proxyInc proxyR proxyM ml (' ':sr)
		sx <- displaysPrec proxyL proxyInc proxyR proxyM x (' ':sl)
		return $ "(TreeMod " ++ sx

instance (NFDataInc l1 inc r m a,NFDataInc l1 inc r m (TreeMod mod l inc r m a)) => NFDataInc l1 inc r m (TreeMod' mod l inc r m a) where
	rnfInc proxyL proxyInc proxyR proxyM EmptyMod = return $! ()
	rnfInc proxyL proxyInc proxyR proxyM (BinMod x ml mr) = do
		a <- rnfInc proxyL proxyInc proxyR proxyM x
		l <- rnfInc proxyL proxyInc proxyR proxyM ml
		r <- rnfInc proxyL proxyInc proxyR proxyM mr
		return $! a `seq` l `seq` r

instance (DeepTypeable mod,DeepTypeable inc,DeepTypeable r,DeepTypeable m,DeepTypeable l,Sat (ctx (TreeMod' mod l inc r m a)),MData ctx (l1 inc r m) a,MData ctx (l1 inc r m) (TreeMod mod l inc r m a))
			=> MData ctx (l1 inc r m) (TreeMod' mod l inc r m a) where
      gfoldl ctx k z EmptyMod = z EmptyMod
      gfoldl ctx k z (BinMod x1 x2 x3) = (((z (\mx1 -> return (\mx2 -> return (\mx3 -> mx1 >>= \x1 -> mx2 >>= \x2 -> mx3 >>= \x3 -> return $ BinMod x1 x2 x3)))) >>= (flip k $ return x1)) >>= (flip k $ return x2)) >>= (flip k $ return x3)
      gunfold ctx k z c = case constrIndex c of
            1 -> z EmptyMod
            2 -> (((z (\mx1 -> return (\mx2 -> return (\mx3 -> mx1 >>= \x1 -> mx2 >>= \x2 -> mx3 >>= \x3 -> return $ BinMod x1 x2 x3)))) >>= k) >>= k) >>= k
      toConstr ctx x@EmptyMod = ((dataTypeOf ctx x) >>= (return . (flip indexConstr 1)))
      toConstr ctx x@(BinMod x1 x2 x3) = ((dataTypeOf ctx x) >>= (return . (flip indexConstr 2)))
      dataTypeOf ctx x = return ty where
            ty = mkDataType "Todo.TreeMod'" [mkConstr ty "EmptyMod" [] Prefix,mkConstr ty "BinMod" [] Prefix]

instance (Typeable mod,Typeable l,Typeable inc,Typeable r,Typeable m,Typeable a) => Memo (TreeMod' mod l inc r m a) where
	type Key (TreeMod' mod l inc r m a) = StableName (TreeMod' mod l inc r m a)
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ Weak.mkWeak x,stableName x)

-- | tree fold
foldTreeInc :: (Typeable a,Thunk mod l inc r m,Eq a,Memo (mod l inc r m (TreeMod' mod l inc r m a)),Output thunk l inc r m,Eq (TreeMod mod l inc r m a),Layer l inc r m) => a -> (a -> a -> l inc r m a) -> TreeMod mod l inc r m a -> l inc r m (thunk l inc r m a)
foldTreeInc z f = memo $ \recur mt -> read mt >>= \t -> case t of
	EmptyMod -> return z
	BinMod x ml mr -> do
		l <- recur ml >>= force
		r <- recur mr >>= force
		x0 <- x `f` l
		x0 `f` r
