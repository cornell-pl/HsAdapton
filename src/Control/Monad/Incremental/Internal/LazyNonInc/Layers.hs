{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, EmptyDataDecls, GeneralizedNewtypeDeriving, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}

module Control.Monad.Incremental.Internal.LazyNonInc.Layers (
	  module Control.Monad.Incremental
	, LazyNonInc
	, IncParams(..)
	, proxyLazyNonInc
	, Outside(..)
	, Inside(..)
	) where

import Control.Monad.Incremental

import Control.Monad.IO.Class
import Data.Typeable


import Control.Monad.Trans

import Data.DeriveTH                 -- Library for deriving instances for existing types
import Data.DeepTypeable
import Data.WithClass.Derive.DeepTypeable
import Language.Haskell.TH.Syntax hiding (lift,Infix,Fixity)
import Control.Applicative

-- | Type for LazyNonInc non-incremental computation
data LazyNonInc deriving Typeable

$( derive makeDeepTypeableAbstract ''LazyNonInc )

type instance IncK LazyNonInc a = ()

instance Incremental LazyNonInc where
	
	newtype Outside LazyNonInc a = LazyNonIncOuter { runLazyNonIncOuter :: IO a } deriving (Functor,Applicative,Monad)
	newtype Inside LazyNonInc a = LazyNonIncInner { runLazyNonIncInner :: IO a } deriving (Functor,Applicative,Monad)
	
	world = LazyNonIncOuter . runLazyNonIncInner
	{-# INLINE world #-}
	-- actually not unsafe in this case
	unsafeWorld = LazyNonIncInner . runLazyNonIncOuter
	{-# INLINE unsafeWorld #-}
	
	runIncremental = runLazyNonIncOuter . outside
	{-# INLINE runIncremental #-}
	runIncrementalWithParams _ = runLazyNonIncOuter . outside
	{-# INLINE runIncrementalWithParams #-}
	
	unsafeIOToInc = inside . LazyNonIncInner
	
	data IncParams LazyNonInc = LazyNonIncParams
	defaultIncParams = LazyNonIncParams

proxyLazyNonInc :: Proxy LazyNonInc
proxyLazyNonInc = Proxy






