{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, StandaloneDeriving, DeriveDataTypeable, DataKinds, ConstraintKinds, EmptyDataDecls, GeneralizedNewtypeDeriving, FlexibleContexts, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, KindSignatures #-}

module Control.Concurrent.Transactional (
	module Control.Monad.Catch
	, STxM, ReadOnlySTxM
	, Transactional(..)
	, throw
	, Resolve(..), Isolation(..), IsolationKind(..), CumulativeInput(..)
	) where

import Control.Applicative
import Control.Monad.Incremental
import Control.Exception hiding (throw)
import Data.IORef
import Control.Monad
import Prelude hiding (new,read)
import Control.Monad.Catch
import Data.Typeable

import Data.DeriveTH
import Data.WithClass.Derive.DeepTypeable
import Data.DeepTypeable
import Language.Haskell.TH.Syntax

-- ** Transactional incremental computations

type STxM = Outside
type ReadOnlySTxM = Inside

-- | A generic class for computations with transactional support, in the style of STM
class (MonadCatch (Outside inc),MonadThrow (Outside inc),Incremental inc) => Transactional inc where
	
	atomically :: STxM inc a -> IO a
	atomically = runIncremental
	
	atomicallyWithParams :: IncParams inc -> STxM inc a -> IO a
	atomicallyWithParams = runIncrementalWithParams
	
	readAtomically :: ReadOnlySTxM inc a -> IO a
	readAtomically = readAtomicallyWithParams defaultIncParams

	readAtomicallyWithParams :: IncParams inc -> ReadOnlySTxM inc a -> IO a
	readAtomicallyWithParams params = atomicallyWithParams params . outside
	
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

data Isolation =
	  Versioned -- joining transaction wins
	| Forgetful -- joined transaction wins
	| Cumulative -- user-provided merge function
  deriving Typeable
deriving instance Typeable Versioned
deriving instance Typeable Forgetful
deriving instance Typeable Cumulative

class Typeable i => IsolationKind (i :: Isolation) where
	toIsolation :: Proxy i -> Isolation
instance IsolationKind Versioned where
	toIsolation _ = Versioned
instance IsolationKind Forgetful where
	toIsolation _ = Forgetful
instance IsolationKind Cumulative where
	toIsolation _ = Cumulative

instance DeepTypeable Versioned where
	typeTree (_::Proxy Versioned) = MkTypeTree (mkName "Control.Concurrent.Transactional.Versioned") [] []
instance DeepTypeable Forgetful where
	typeTree (_::Proxy Forgetful) = MkTypeTree (mkName "Control.Concurrent.Transactional.Forgetful") [] []
instance DeepTypeable Cumulative where
	typeTree (_::Proxy Cumulative) = MkTypeTree (mkName "Control.Concurrent.Transactional.Cumulative") [] []

-- | original when forked -> current parent -> child -> merged value
-- inner layer: we don't want to allow changes
-- runs on the child's state
type Resolve inc a = a -> a -> a -> Inside inc a

-- | cumulative variable for transactional futures
class (Input mod l inc) => CumulativeInput mod l inc where	
	cumulative :: (IncK inc a) => Resolve inc a -> l inc a -> l inc (mod l inc a)


