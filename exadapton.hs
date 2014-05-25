{-# LANGUAGE DeriveDataTypeable, KindSignatures, ExistentialQuantification, DataKinds, FlexibleInstances, DeriveGeneric, FlexibleContexts #-}

module Main where

import Generics.Lenses.Symmetric.Language hiding (get,put)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State (State(..),StateT(..),MonadState)
import qualified Control.Monad.State as State
import GHC.Generics

import Debug.Trace
import System.IO.Unsafe
import Data.Maybe
import Control.Monad.Identity
import Data.IORef

import Control.Monad.Adapton
import System.Mem.MemoTable
import System.Mem.StableName
import Data.Typeable

data ListM' m r a = NilM | ConsM a (ListM m r a) deriving (Eq,Generic,Typeable)
type ListM m r a = M m r (ListM' m r a)

instance (Typeable m,Typeable r,Typeable a) => Memo (ListM' m r a) where
	memoKey = liftM MemoKey . makeStableName

data ListU' l m r a = NilU | ConsU a (ListU l m r a) deriving (Eq,Generic,Typeable)
type ListU l m r a = U l m r (ListU' l m r a)

instance (Typeable l,Typeable m,Typeable r,Typeable a) => Memo (ListU' l m r a) where
	memoKey = liftM MemoKey . makeStableName

-- filter
filterInc :: (Eq a,Typeable a,Layer l m r) => (a -> Bool) -> ListM m r a -> l m r (ListU l m r a)
filterInc p = thunk . filterInc' p

filterInc' :: (Eq a,Typeable a,Layer l m r) => (a -> Bool) -> ListM m r a -> l m r (ListU' l m r a)
filterInc' p mxs = get mxs >>= \xs -> case xs of
	ConsM x mxs' -> if p x
		then liftM (ConsU x) $ filterInc p mxs'
		else filterInc' p mxs'
	NilM -> return NilU

fib :: Layer l m r => Int -> l m r (U l m r Int)
fib = memo fib'

fib' :: Layer l m r => (Int -> l m r (U l m r Int)) -> Int -> l m r Int
fib' memo_fib n = if n <= 1
	then return 1
	else do
		n1 <- memo_fib (n - 1) >>= force
		n2 <- memo_fib (n - 2) >>= force
		return (n1 + n2)
	
test = do
	let x = 1
	let p = (x,x)
	sn1 <- makeStableName $ fst p
	sn2 <- makeStableName $ snd p
	print $ eqStableName sn1 sn2
