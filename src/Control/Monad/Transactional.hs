{-# LANGUAGE EmptyDataDecls, GeneralizedNewtypeDeriving, FlexibleContexts, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, KindSignatures #-}

module Control.Monad.Transactional where

import Control.Monad.Incremental
import Control.Exception
import Control.Concurrent.STM as STM
import Data.IORef
import Control.Monad
import Prelude hiding (new,read)

-- ** Transactional incremental computations

type STxM = Outside

-- | A generic class for computations with transactional support, in the style of STM
class Incremental inc r m => Transactional inc r m where
	
	atomically :: STxM inc r m a -> m a
	
	retry :: STxM inc r m a
	
	orElse :: STxM inc r m a -> STxM inc r m a -> STxM inc r m a
	
	throw :: Exception e => e -> STxM inc r m a
	catch :: Exception e => STxM inc r m a -> (e -> STxM inc r m a) -> STxM inc r m a

-- ** STM bindings

-- | @STM@ as an incremental computation class
data IncSTM

instance Incremental IncSTM IORef IO where
	newtype Outside IncSTM IORef IO a = OutsideSTM { unOutsideSTM :: STM a } deriving (Monad)
	newtype Inside IncSTM IORef IO a = InsideSTM { unInsideSTM :: STM a } deriving (Monad)
	
	world = OutsideSTM . unInsideSTM
	
	runIncremental (OutsideSTM stm) = STM.atomically stm

-- just an encapsulator for @TVar@s
newtype IncTVar (l :: * -> (* -> *) -> (* -> *) -> * -> *) inc (r :: * -> *) (m :: * -> *) a = IncTVar { unIncTVar :: TVar a }

-- @InLayer@ exposes an implementation detail, that is not explicit for STMs
instance InLayer Outside IncSTM IORef IO where
	inL = error "STM does not support arbitrary IO"
	{-# INLINE inL #-}
instance InLayer Inside IncSTM IORef IO where
	inL = error "STM does not support arbitrary IO"
	{-# INLINE inL #-}

-- STM does not use @IORef@s, but we nonetheless need to pass them as a witness that @IO@ supports references
instance Transactional IncSTM IORef IO where
	atomically (OutsideSTM stm) = STM.atomically stm
	retry = OutsideSTM STM.retry
	orElse (OutsideSTM stm1) (OutsideSTM stm2) = OutsideSTM $ STM.orElse stm1 stm2
	throw e = OutsideSTM $ throwSTM e
	catch (OutsideSTM stm) f = OutsideSTM $ catchSTM stm (unOutsideSTM . f)

instance Layer l IncSTM IORef IO => Thunk IncTVar l IncSTM IORef IO where
	new m = m >>= newc
	newc x = inside $ InsideSTM $ liftM IncTVar $ STM.newTVar x
	read (IncTVar t) = inside $ InsideSTM $ STM.readTVar t

instance Layer l IncSTM IORef IO => Input IncTVar l IncSTM IORef IO where
	ref x = inside $ InsideSTM $ liftM IncTVar $ STM.newTVar x
	get (IncTVar t) = inside $ InsideSTM $ STM.readTVar t
	set (IncTVar t) x = inside $ InsideSTM $ STM.writeTVar t x



