{-# LANGUAGE StandaloneDeriving, ScopedTypeVariables, UndecidableInstances, TemplateHaskell, DeriveDataTypeable, EmptyDataDecls, GeneralizedNewtypeDeriving, FlexibleContexts, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, KindSignatures #-}

-- STM as a transactional incremental computation framework that just recomputes output thunks from scratch

module Control.Concurrent.Transactional.SHFSTM where

import Control.Concurrent.Transactional
import Control.Monad.Incremental
import Control.Exception
import Data.IORef
import Control.Monad
import Control.Applicative
import qualified Control.Concurrent.SHFSTM as SHFSTM
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

import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.WithClass.MData

-- ** STM bindings

-- | @STM@ as an incremental computation class
data SHFSTM deriving Typeable

type instance IncK SHFSTM a = ()

instance Incremental SHFSTM where
	newtype Outside SHFSTM a = OutsideSHFSTM { unOutsideSHFSTM :: SHFSTM.STM a } deriving (Monad,Applicative,Functor)
	newtype Inside SHFSTM a = InsideSHFSTM { unInsideSHFSTM :: SHFSTM.STM a } deriving (Monad,Applicative,Functor)
	
	world = OutsideSHFSTM . unInsideSHFSTM
	-- | this function is actually safe in this context, since we support no incrementality
	unsafeWorld = InsideSHFSTM . unOutsideSHFSTM
	
	runIncrementalWithParams params stm = SHFSTM.atomically (unOutsideSHFSTM $ outside stm)
	
	data IncParams SHFSTM = SHFSTMParams
	defaultIncParams = SHFSTMParams
	
	unsafeIOToInc = inside . InsideSHFSTM . SHFSTM.unsafeIOToSTM

-- just an encapsulator for @TVar@s
newtype SHFSTMVar (l :: * -> * -> *) inc a = SHFSTMVar { unSHFSTMVar :: SHFSTM.TVar a } deriving (Eq,Typeable)

-- STM does not use @IORef@s, but we nonetheless need to pass them as a witness that @IO@ supports references
instance Transactional SHFSTM where
	retry = OutsideSHFSTM SHFSTM.retry
	orElse (OutsideSHFSTM stm1) (OutsideSHFSTM stm2) = OutsideSHFSTM $ SHFSTM.orElse stm1 stm2

instance MonadThrow (Outside SHFSTM) where
	throwM e = error "no exceptions for SHFSTM"
instance MonadCatch (Outside SHFSTM) where
	catch (OutsideSHFSTM stm) f = error "no exceptions for SHFSTM"

-- @TVar@s can be explicitly created and read, but they don't store lazy computations
instance Layer l SHFSTM => Thunk SHFSTMVar l SHFSTM where
	new m = m >>= newc
	newc x = inside $ InsideSHFSTM $ liftM SHFSTMVar $ SHFSTM.newTVar x
	read (SHFSTMVar t) = inside $ InsideSHFSTM $ SHFSTM.readTVar t

-- @TVar@s can mutated and be used as the inputs of incremental computations
instance Layer l SHFSTM => Input SHFSTMVar l SHFSTM where
	ref x = inside $ InsideSHFSTM $ liftM SHFSTMVar $ SHFSTM.newTVar x
	get (SHFSTMVar t) = inside $ InsideSHFSTM $ SHFSTM.readTVar t
	set (SHFSTMVar t) x = inside $ InsideSHFSTM $ SHFSTM.writeTVar t x

$(deriveMemo ''SHFSTMVar)

instance DeepTypeable SHFSTMVar where
	typeTree _ = MkTypeTree (mkName "Control.Concurrent.Transactional.SHFSTM.SHFSTMVar") [] []

instance (DeepTypeable l,DeepTypeable inc,DeepTypeable a) => DeepTypeable (SHFSTMVar l inc a) where
	typeTree (_ :: Proxy (SHFSTMVar l inc a)) = MkTypeTree (mkName "Control.Concurrent.Transactional.SHFSTM.SHFSTMVar") args [MkConTree (mkName "Control.Concurrent.Transactional.SHFSTM.SHFSTMVar") [typeTree (Proxy::Proxy (SHFSTM.TVar a))]]
		where args = [typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy a)]

$(derive makeDeepTypeableAbstract ''SHFSTM)

instance (DeepTypeable a,Monad m,Sat (ctx (SHFSTM.STM a))) => MData ctx m (SHFSTM.STM a) where
  toConstr ctx _   = error "toConstr"
  gunfold ctx _ _  = error "gunfold"
  dataTypeOf ctx _ = return $ mkNoRepType "Control.Concurrent.SHFSTM"

instance DeepTypeable SHFSTM.STM where
	typeTree _ = MkTypeTree (mkName "GHC.Conc.STM") [] []

instance (DeepTypeable a) => DeepTypeable (SHFSTM.STM a) where
	typeTree (_::Proxy (SHFSTM.STM a)) = MkTypeTree (mkName "Control.Concurrent.SHFSTM.STM") [typeTree (Proxy::Proxy a)] [MkConTree (mkName "Control.Concurrent.SHFSTM.STM") [typeTree (Proxy::Proxy a)]]

instance (DeepTypeable a,Monad m,Sat (ctx (SHFSTM.TVar a))) => MData ctx m (SHFSTM.TVar a) where
  toConstr ctx _   = error "toConstr"
  gunfold ctx _ _  = error "gunfold"
  dataTypeOf ctx _ = return $ mkNoRepType "Control.Concurrent.SHFSTM.TVar"

instance DeepTypeable SHFSTM.TVar where
	typeTree _ = MkTypeTree (mkName "Control.Concurrent.SHFSTM.TVar") [] []

instance (DeepTypeable a) => DeepTypeable (SHFSTM.TVar a) where
	typeTree (_::Proxy (SHFSTM.TVar a)) = MkTypeTree (mkName "Control.Concurrent.SHFSTM.TVar") [typeTree (Proxy::Proxy a)] [MkConTree (mkName "Control.Concurrent.SHFSTM.newTVar") [typeTree (Proxy::Proxy a)]]
	
deriving instance Typeable SHFSTM.TVar
deriving instance Typeable SHFSTM.STM
	