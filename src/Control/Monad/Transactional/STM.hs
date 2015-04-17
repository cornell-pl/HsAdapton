{-# LANGUAGE UndecidableInstances, TemplateHaskell, DeriveDataTypeable, EmptyDataDecls, GeneralizedNewtypeDeriving, FlexibleContexts, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, KindSignatures #-}

-- STM as a transactional incremental computation framework that just recomputes output thunks from scratch

module Control.Monad.Transactional.STM where

import Control.Monad.Transactional
import Control.Monad.Incremental.LazyNonInc (LazyNonIncU(..))
import Control.Monad.Incremental
import Control.Exception
import Control.Concurrent.STM as STM
import Data.IORef
import Control.Monad
import Control.Applicative
import Prelude hiding (new,read)
import Data.Typeable
import Data.Derive.Memo
import Data.Memo

import System.Mem.MemoTable
import System.Mem.StableName.Exts
import System.IO.Unsafe
import System.Mem.Weak

import GHC.Conc.Sync

import Control.Monad.IO.Class
import Control.Monad.Catch

-- ** STM bindings

-- | @STM@ as an incremental computation class
data IncSTM deriving Typeable

type instance IncK IncSTM a = ()

instance Incremental IncSTM where
	newtype Outside IncSTM a = OutsideSTM { unOutsideSTM :: STM a } deriving (Monad,Applicative,Functor)
	newtype Inside IncSTM a = InsideSTM { unInsideSTM :: STM a } deriving (Monad,Applicative,Functor)
	
	world = OutsideSTM . unInsideSTM
	-- | this function is actually safe in this context, since we support no incrementality
	unsafeWorld = InsideSTM . unOutsideSTM
	
	runIncremental (OutsideSTM stm) = STM.atomically stm
	
	data IncParams IncSTM = IncSTMParams
	defaultIncParams = IncSTMParams
	
	unsafeIOToInc = inside . InsideSTM . unsafeIOToSTM

-- just an encapsulator for @TVar@s
newtype IncTVar (l :: * -> * -> *) inc a = IncTVar { unIncTVar :: TVar a } deriving (Eq,Typeable)

-- STM does not use @IORef@s, but we nonetheless need to pass them as a witness that @IO@ supports references
instance Transactional IncSTM where
	atomically (OutsideSTM stm) = STM.atomically stm
	retry = OutsideSTM STM.retry
	orElse (OutsideSTM stm1) (OutsideSTM stm2) = OutsideSTM $ STM.orElse stm1 stm2

instance MonadThrow (Outside IncSTM) where
	throwM e = OutsideSTM $ throwSTM e
instance MonadCatch (Outside IncSTM) where
	catch (OutsideSTM stm) f = OutsideSTM $ catchSTM stm (unOutsideSTM . f)

-- @TVar@s can be explicitly created and read, but they don't store lazy computations
instance Layer l IncSTM => Thunk IncTVar l IncSTM where
	new m = m >>= newc
	newc x = inside $ InsideSTM $ liftM IncTVar $ STM.newTVar x
	read (IncTVar t) = inside $ InsideSTM $ STM.readTVar t

-- @TVar@s can mutated and be used as the inputs of incremental computations
instance Layer l IncSTM => Input IncTVar l IncSTM where
	ref x = inside $ InsideSTM $ liftM IncTVar $ STM.newTVar x
	get (IncTVar t) = inside $ InsideSTM $ STM.readTVar t
	set (IncTVar t) x = inside $ InsideSTM $ STM.writeTVar t x

-- | The type of output thunks for an incremental version of STM
-- | Output thunks in STM are lazy and may depend on input @IncTVar@s, but they are simply recomputed every time
type IncUVar = LazyNonIncU

$(deriveMemo ''IncTVar)
--instance (Typeable inc,Typeable l,Typeable r,Typeable m,Typeable a,WeakRef r) => Memo (IncTVar l inc r m a) where
--	type Key (IncTVar l inc r m a) = StableName (IncTVar l inc r m a)
--	{-# INLINE memoKey #-}
--	memoKey = stableName
--	{-# INLINE memoWeak #-}
--	memoWeak = \x -> MkWeak $ mkWeak x



