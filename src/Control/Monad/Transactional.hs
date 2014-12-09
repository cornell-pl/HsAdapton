{-# LANGUAGE EmptyDataDecls, GeneralizedNewtypeDeriving, FlexibleContexts, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, KindSignatures #-}

module Control.Monad.Transactional where

import Control.Monad.Incremental
import Control.Exception
import Data.IORef
import Control.Monad
import Prelude hiding (new,read)

-- ** Transactional incremental computations

type STxM = Outside
type ReadOnlySTxM = Inside

-- | A generic class for computations with transactional support, in the style of STM
class Incremental inc r m => Transactional inc r m where
	
	atomically :: STxM inc r m a -> m a
	
	-- A read-only transaction with the nice property that it always succeeds, i.e., never conflicts with concurrent transactions
	-- This is possible because the transaction can ignore concurrent modifications that occurred since its starting time
	-- For example, a long-running read-only transaction can safely ignore parallel modifications that occur during its execution, and see only a stale state. It still remains consistent in respect to its client or local thread.
	-- Note that it can still allocate new data
	-- Note also that we can't use the other transactional blocks in a read-only transaction, since they are unsafe at the inner layer
	readAtomically :: ReadOnlySTxM inc r m a -> m a
	readAtomically = atomically . inside
	
	retry :: STxM inc r m a
	
	orElse :: STxM inc r m a -> STxM inc r m a -> STxM inc r m a
	
	throw :: Exception e => e -> STxM inc r m a
	catch :: Exception e => STxM inc r m a -> (e -> STxM inc r m a) -> STxM inc r m a

instance Transactional inc r m => MonadPlus (STxM inc r m) where
	mzero = retry
	mplus = orElse



