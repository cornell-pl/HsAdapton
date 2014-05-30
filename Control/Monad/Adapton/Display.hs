{-# LANGUAGE TypeOperators, ScopedTypeVariables, MultiParamTypeClasses, DeriveDataTypeable, KindSignatures, ExistentialQuantification, DataKinds, FlexibleInstances, DeriveGeneric, FlexibleContexts #-}

module Control.Monad.Adapton.Display where

import Generics.Lenses.Symmetric.Language hiding (get,put)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State (State(..),StateT(..),MonadState)
import qualified Control.Monad.State as State
import GHC.Generics

import Control.Monad.Adapton.DataTypes
import Debug.Trace
import System.IO.Unsafe
import Data.Maybe
import Control.Monad.Identity
import Data.List as List
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.IORef

import Control.Monad.Adapton
import System.Mem.MemoTable (Memo(..),MemoKey(..))
import qualified System.Mem.MemoTable as Memo
import System.Mem.StableName
import System.Mem.Weak
import Control.Monad.Trans
import Data.Typeable

import Control.Monad.Adapton

-- * generic mechanism for displaying data types with refs/thunks

class (Layer l m r) => Display l m r a where
	display :: a -> l m r ()
	display x = showL x >>= liftIO . print
	showL :: a -> l m r String

instance (Display l m r a,Eq a,Typeable a,Memo a) => Display l m r (U m r a) where
	showL t = force t >>= showL >>= \str -> return $ "<u" ++ show (idU t) ++ " " ++ str ++ ">"

instance (Display l m r a,Eq a,Typeable a,Memo a) => Display l m r (M m r a) where
	showL m = get m >>= showL >>= \str -> return $ "<m" ++ show (idU $ unM m) ++ " " ++ str ++ ">"

instance Layer l m r => Display l m r (U1 p) where
	showL U1 = return ""
instance (Display l m r c,Layer l m r) => Display l m r (K1 i c p) where
	showL (K1 c) = showL c
instance (Display l m r (f p),Constructor c) => Display l m r (M1 C c f p) where
	showL m1 = do
		str <- showL (unM1 m1)
		return $ "(" ++ conName m1 ++ " " ++ str ++ ")"
instance (Display l m r (f p)) => Display l m r (M1 D c f p) where
	showL m1 = showL (unM1 m1)
instance (Display l m r (f p)) => Display l m r (M1 S c f p) where
	showL m1 = showL (unM1 m1)
instance (Display l m r (f p),Display l m r (g p)) => Display l m r ((f :+: g) p) where
	showL (L1 x) = showL x
	showL (R1 x) = showL x
instance (Display l m r (f p),Display l m r (g p)) => Display l m r ((f :*: g) p) where
	showL (x :*: y) = do
		str1 <- showL x
		str2 <- showL y
		return $ str1 ++ " " ++ str2
instance Layer l m r => Display l m r Int where
	showL i = return $ show i
