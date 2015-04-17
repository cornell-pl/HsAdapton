{-# LANGUAGE ScopedTypeVariables, UndecidableInstances, TemplateHaskell, DeriveDataTypeable, EmptyDataDecls, GeneralizedNewtypeDeriving, FlexibleContexts, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, KindSignatures #-}

-- STM as a transactional incremental computation framework that just recomputes output thunks from scratch

module Control.Monad.Transactional.STM where

import Control.Monad.Transactional
import Control.Monad.Incremental
import Control.Exception
import qualified Control.Concurrent.STM as STM
import Data.IORef
import Control.Monad
import Control.Applicative
import Prelude hiding (new,read)
import Data.Typeable
import Data.Derive.Memo
import Data.WithClass.Derive.DeepTypeable
import Data.DeriveTH
import Data.DeepTypeable
import Language.Haskell.TH.Syntax
import Data.WithClass.MGenerics.Instances
import Data.Memo

import System.Mem.MemoTable
import System.Mem.StableName.Exts
import System.IO.Unsafe
import System.Mem.Weak

import qualified GHC.Conc.Sync as STM

import Control.Monad.IO.Class
import Control.Monad.Catch

-- ** STM bindings

-- | @STM@ as an incremental computation class
data STM deriving Typeable

type instance IncK STM a = ()

instance Incremental STM where
	newtype Outside STM a = OutsideSTM { unOutsideSTM :: STM.STM a } deriving (Monad,Applicative,Functor)
	newtype Inside STM a = InsideSTM { unInsideSTM :: STM.STM a } deriving (Monad,Applicative,Functor)
	
	world = OutsideSTM . unInsideSTM
	-- | this function is actually safe in this context, since we support no incrementality
	unsafeWorld = InsideSTM . unOutsideSTM
	
	runIncremental (OutsideSTM stm) = STM.atomically stm
	
	data IncParams STM = STMParams
	defaultIncParams = STMParams
	
	unsafeIOToInc = inside . InsideSTM . STM.unsafeIOToSTM

-- just an encapsulator for @TVar@s
newtype STMVar (l :: * -> * -> *) inc a = STMVar { unSTMVar :: STM.TVar a } deriving (Eq,Typeable)

-- STM does not use @IORef@s, but we nonetheless need to pass them as a witness that @IO@ supports references
instance Transactional STM where
	atomically (OutsideSTM stm) = STM.atomically stm
	retry = OutsideSTM STM.retry
	orElse (OutsideSTM stm1) (OutsideSTM stm2) = OutsideSTM $ STM.orElse stm1 stm2

instance MonadThrow (Outside STM) where
	throwM e = OutsideSTM $ STM.throwSTM e
instance MonadCatch (Outside STM) where
	catch (OutsideSTM stm) f = OutsideSTM $ STM.catchSTM stm (unOutsideSTM . f)

-- @TVar@s can be explicitly created and read, but they don't store lazy computations
instance Layer l STM => Thunk STMVar l STM where
	new m = m >>= newc
	newc x = inside $ InsideSTM $ liftM STMVar $ STM.newTVar x
	read (STMVar t) = inside $ InsideSTM $ STM.readTVar t

-- @TVar@s can mutated and be used as the inputs of incremental computations
instance Layer l STM => Input STMVar l STM where
	ref x = inside $ InsideSTM $ liftM STMVar $ STM.newTVar x
	get (STMVar t) = inside $ InsideSTM $ STM.readTVar t
	set (STMVar t) x = inside $ InsideSTM $ STM.writeTVar t x

$(deriveMemo ''STMVar)

instance DeepTypeable STMVar where
	typeTree _ = MkTypeTree (mkName "Control.Monad.Transactional.STM.STMVar") [] []

instance (DeepTypeable l,DeepTypeable inc,DeepTypeable a) => DeepTypeable (STMVar l inc a) where
	typeTree (_ :: Proxy (STMVar l inc a)) = MkTypeTree (mkName "Control.Monad.Transactional.STM.STMVar") args [MkConTree (mkName "Control.Monad.Transactional.STM.STMVar") [typeTree (Proxy::Proxy (STM.TVar a))]]
		where args = [typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy a)]

$(derive makeDeepTypeableAbstract ''STM)