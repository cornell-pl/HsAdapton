{-# LANGUAGE EmptyDataDecls, GeneralizedNewtypeDeriving, FlexibleContexts, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, KindSignatures #-}

module Control.Monad.Transactional (
	module Control.Monad.Catch
	, STxM, ReadOnlySTxM
	, Transactional(..)
	, throw
	) where

import Control.Applicative
import Control.Monad.Incremental
import Control.Exception hiding (throw)
import Data.IORef
import Control.Monad
import Prelude hiding (new,read)
import Control.Monad.Catch

-- ** Transactional incremental computations

type STxM = Outside
type ReadOnlySTxM = Inside

-- | A generic class for computations with transactional support, in the style of STM
class (MonadCatch (Outside inc),MonadThrow (Outside inc),Incremental inc) => Transactional inc where
	
	atomically :: STxM inc a -> IO a
	atomically = runIncremental
	
	atomicallyWithParams :: IncParams inc -> STxM inc a -> IO a
	atomicallyWithParams = runIncrementalWithParams
	
	-- A read-only transaction.
	-- Note that it can still allocate new data.
	-- Note also that we can't use the other transactional blocks in a read-only transaction.
	readAtomically :: ReadOnlySTxM inc a -> IO a
	readAtomically = runIncremental . inside
	
	readAtomicallyWithParams :: IncParams inc -> ReadOnlySTxM inc a -> IO a
	readAtomicallyWithParams params m = atomicallyWithParams params (inside m)
	
	retry :: STxM inc a
	
	orElse :: STxM inc a -> STxM inc a -> STxM inc a

instance Transactional inc => MonadPlus (STxM inc) where
	mzero = retry
	mplus = orElse
instance Transactional inc => Alternative (STxM inc) where
	empty = retry
	(<|>) = orElse

throw :: (MonadThrow m,Exception e) => e -> m a
throw = throwM




