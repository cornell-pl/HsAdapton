{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls, GeneralizedNewtypeDeriving, FlexibleContexts, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, KindSignatures #-}

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
import Control.Monad.Lazy
import System.Mem.MemoTable
import System.Mem.StableName
import System.IO.Unsafe
import System.Mem.Weak
import System.Mem.WeakKey
import GHC.Conc.Sync
import System.Mem.WeakTable


-- ** STM bindings

-- | @STM@ as an incremental computation class
data IncSTM deriving Typeable

instance Incremental IncSTM IORef IO where
	newtype Outside IncSTM IORef IO a = OutsideSTM { unOutsideSTM :: STM a } deriving (MonadLazy,Monad,Applicative,Functor)
	newtype Inside IncSTM IORef IO a = InsideSTM { unInsideSTM :: STM a } deriving (MonadLazy,Monad,Applicative,Functor)
	
	world = OutsideSTM . unInsideSTM
	-- | this function is actually safe in this context, since we support no incrementality
	unsafeWorld = InsideSTM . unOutsideSTM
	
	runIncremental (OutsideSTM stm) = STM.atomically stm

-- just an encapsulator for @TVar@s
newtype IncTVar (l :: * -> (* -> *) -> (* -> *) -> * -> *) inc (r :: * -> *) (m :: * -> *) a = IncTVar { unIncTVar :: TVar a } deriving (Eq,Typeable)

-- @InLayer@ exposes an implementation detail, that is not explicit for STMs
instance InLayer Outside IncSTM IORef IO where
	inL = OutsideSTM . unsafeIOToSTM
	{-# INLINE inL #-}
instance InLayer Inside IncSTM IORef IO where
	inL = InsideSTM . unsafeIOToSTM
	{-# INLINE inL #-}

-- STM does not use @IORef@s, but we nonetheless need to pass them as a witness that @IO@ supports references
instance Transactional IncSTM IORef IO where
	atomically (OutsideSTM stm) = STM.atomically stm
	retry = OutsideSTM STM.retry
	orElse (OutsideSTM stm1) (OutsideSTM stm2) = OutsideSTM $ STM.orElse stm1 stm2
	throw e = OutsideSTM $ throwSTM e
	catch (OutsideSTM stm) f = OutsideSTM $ catchSTM stm (unOutsideSTM . f)

-- @TVar@s can be explicitly created and read, but they don't store lazy computations
instance Layer l IncSTM IORef IO => Thunk IncTVar l IncSTM IORef IO where
	new m = m >>= newc
	newc x = inside $ InsideSTM $ liftM IncTVar $ STM.newTVar x
	read (IncTVar t) = inside $ InsideSTM $ STM.readTVar t

-- @TVar@s can mutated and be used as the inputs of incremental computations
instance Layer l IncSTM IORef IO => Input IncTVar l IncSTM IORef IO where
	ref x = inside $ InsideSTM $ liftM IncTVar $ STM.newTVar x
	get (IncTVar t) = inside $ InsideSTM $ STM.readTVar t
	set (IncTVar t) x = inside $ InsideSTM $ STM.writeTVar t x

-- | The type of output thunks for an incremental version of STM
-- | Output thunks in STM are lazy and may depend on input @IncTVar@s, but they are simply recomputed every time
type IncUVar = LazyNonIncU

-- default instance
instance MonadLazy STM

instance (Typeable inc,Typeable l,Typeable r,Typeable m,Typeable a,WeakRef r) => Memo (IncTVar l inc r m a) where
	type Key (IncTVar l inc r m a) = StableName (IncTVar l inc r m a)
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,stableName x)



