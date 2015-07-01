{-# LANGUAGE DeriveGeneric, TemplateHaskell, ConstraintKinds, TypeFamilies, ScopedTypeVariables, StandaloneDeriving, KindSignatures, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances, DeriveDataTypeable #-}

module Control.Monad.Incremental.Tree where


import Data.WithClass.MData
import Control.Monad.Incremental
import Data.Proxy
import Control.Monad.Incremental.Display
import Control.Monad.Incremental.LazyNonInc
import Control.Monad.Incremental.Adapton hiding (MData)
import Data.IORef


import Data.DeriveTH              
import Data.DeepTypeable
import Data.WithClass.Derive.DeepTypeable
import Language.Haskell.TH.Syntax hiding (lift,Infix,Fixity)

import System.Mem.StableName.Exts
import System.IO.Unsafe

import System.Mem.Weak as Weak
import Prelude hiding (mod,const,read)
import Data.Memo
import Data.Derive.Memo
import GHC.Generics

type TreeMod
	(mod :: ((* -> * -> *) -> * -> * -> *))
	(l :: * -> * -> *)
	inc
	a
	= mod l inc (TreeMod' mod l inc a)

data TreeMod'
	(mod :: ((* -> * -> *) -> * -> * -> *))
	(l :: * -> * -> *)
	inc
	a
	= EmptyMod | BinMod a (TreeMod mod l inc a) (TreeMod mod l inc a) deriving (Generic,Typeable)

deriving instance (Eq a,Eq (TreeMod mod l inc a)) => Eq (TreeMod' mod l inc a)

instance (DeepTypeable mod,DeepTypeable l,DeepTypeable inc,DeepTypeable a,DeepTypeable (TreeMod mod l inc a)) => DeepTypeable (TreeMod' mod l inc a) where
	typeTree (_::Proxy (TreeMod' mod l inc a)) = MkTypeTree (mkName "Control.Monad.Incremental.Tree.TreeMod'") args [MkConTree (mkName "Control.Monad.Incremental.Tree.EmptyMod") [],MkConTree (mkName "Control.Monad.Incremental.Tree.BinMod") [typeTree (Proxy::Proxy a),typeTree (Proxy::Proxy (TreeMod mod l inc a)),typeTree (Proxy::Proxy (TreeMod mod l inc a))]]
		where args = [typeTree (Proxy::Proxy mod),typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy a)]

type TreeU l a = TreeMod U l Adapton a
type TreeU' l a = TreeMod' U l Adapton a

type TreeM l a = TreeMod M l Adapton a
type TreeM' l a = TreeMod' M l Adapton a

type TreeL l a = TreeMod L l Adapton a
type TreeL' l a = TreeMod' L l Adapton a

type TreeLazyNonIncU l a = TreeMod LazyNonIncU l LazyNonInc a
type TreeLazyNonIncU' l a = TreeMod' LazyNonIncU l LazyNonInc a

type TreeLazyNonIncL l a = TreeMod LazyNonIncL l LazyNonInc a
type TreeLazyNonIncL' l a = TreeMod' LazyNonIncL l LazyNonInc a

type TreeLazyNonIncM l a = TreeMod LazyNonIncM l LazyNonInc a
type TreeLazyNonIncM' l a = TreeMod' LazyNonIncM l LazyNonInc a

instance (Display l1 inc a,Display l1 inc (TreeMod mod l inc a)) => Display l1 inc (TreeMod' mod l inc a) where
	displaysPrec proxyL proxyInc EmptyMod r = return $ "EmptyMod" ++ r
	displaysPrec proxyL proxyInc (BinMod x ml mr) rest = do
		sr <- displaysPrec proxyL proxyInc mr (')':rest)
		sl <- displaysPrec proxyL proxyInc ml (' ':sr)
		sx <- displaysPrec proxyL proxyInc x (' ':sl)
		return $ "(TreeMod " ++ sx

instance (NFDataInc l1 inc a,NFDataInc l1 inc (TreeMod mod l inc a)) => NFDataInc l1 inc (TreeMod' mod l inc a) where
	nfDataInc proxyL proxyInc EmptyMod = return $! ()
	nfDataInc proxyL proxyInc (BinMod x ml mr) = do
		a <- nfDataInc proxyL proxyInc x
		l <- nfDataInc proxyL proxyInc ml
		r <- nfDataInc proxyL proxyInc mr
		return $! a `seq` l `seq` r

instance (DeepTypeable mod,DeepTypeable inc,DeepTypeable l,Sat (ctx (TreeMod' mod l inc a)),MData ctx (l1 inc) a,MData ctx (l1 inc) (TreeMod mod l inc a))
			=> MData ctx (l1 inc) (TreeMod' mod l inc a) where
      gfoldl ctx k z EmptyMod = z EmptyMod
      gfoldl ctx k z (BinMod x1 x2 x3) = (((z (\mx1 -> return (\mx2 -> return (\mx3 -> mx1 >>= \x1 -> mx2 >>= \x2 -> mx3 >>= \x3 -> return $ BinMod x1 x2 x3)))) >>= (flip k $ return x1)) >>= (flip k $ return x2)) >>= (flip k $ return x3)
      gunfold ctx k z c = case constrIndex c of
            1 -> z EmptyMod
            2 -> (((z (\mx1 -> return (\mx2 -> return (\mx3 -> mx1 >>= \x1 -> mx2 >>= \x2 -> mx3 >>= \x3 -> return $ BinMod x1 x2 x3)))) >>= k) >>= k) >>= k
      toConstr ctx x@EmptyMod = ((dataTypeOf ctx x) >>= (return . (flip indexConstr 1)))
      toConstr ctx x@(BinMod x1 x2 x3) = ((dataTypeOf ctx x) >>= (return . (flip indexConstr 2)))
      dataTypeOf ctx x = return ty where
            ty = mkDataType "Todo.TreeMod'" [mkConstr ty "EmptyMod" [] Data.WithClass.MData.Prefix,mkConstr ty "BinMod" [] Data.WithClass.MData.Prefix]

$(deriveMemo ''TreeMod')

-- | tree fold
foldTreeInc :: (IncK inc a,IncK inc (TreeMod' mod l inc a),Thunk mod l inc,Memo (mod l inc (TreeMod' mod l inc a)),Output thunk l inc,Layer l inc) => a -> (a -> a -> l inc a) -> TreeMod mod l inc a -> l inc (thunk l inc a)
foldTreeInc z f = memo $ \recur mt -> read mt >>= \t -> case t of
	EmptyMod -> return z
	BinMod x ml mr -> do
		l <- recur ml >>= force
		r <- recur mr >>= force
		x0 <- x `f` l
		x0 `f` r
