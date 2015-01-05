{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, EmptyDataDecls, GeneralizedNewtypeDeriving, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}

module Control.Monad.Incremental.LazyNonInc.Layers where

import Control.Monad.Incremental
import Control.Monad.Ref
import Control.Monad.IO.Class
import Data.Typeable
import System.Mem.WeakKey
import Control.Monad.Lazy
import Control.Monad.Trans

import Data.DeriveTH                 -- Library for deriving instances for existing types
import Data.DeepTypeable
import Data.WithClass.Derive.DeepTypeable
import Language.Haskell.TH.Syntax hiding (lift,Infix,Fixity)
import Control.Applicative

-- | Type for LazyNonInc non-incremental computation
data LazyNonInc deriving Typeable

$( derive makeDeepTypeableAbstract ''LazyNonInc )

instance (Typeable r,Typeable m,MonadRef r m,WeakRef r) => Incremental LazyNonInc r m where
	
	newtype Outside LazyNonInc r m a = LazyNonIncOuter { runLazyNonIncOuter :: m a } deriving (Functor,Applicative,Monad,MonadIO,MonadRef r,MonadLazy)
	newtype Inside LazyNonInc r m a = LazyNonIncInner { runLazyNonIncInner :: m a } deriving (Functor,Applicative,Monad,MonadIO,MonadRef r,MonadLazy)
	
	world = LazyNonIncOuter . runLazyNonIncInner
	{-# INLINE world #-}
	-- actually not unsafe in this case
	unsafeWorld = LazyNonIncInner . runLazyNonIncOuter
	{-# INLINE unsafeWorld #-}
	
	runIncremental = runLazyNonIncOuter
	{-# INLINE runIncremental #-}

instance MonadTrans (Outside LazyNonInc r) where
	lift = LazyNonIncOuter
	{-# INLINE lift #-}
instance MonadTrans (Inside LazyNonInc r) where
	lift = LazyNonIncInner
	{-# INLINE lift #-}
instance (Typeable r,Typeable m) => InLayer Outside LazyNonInc r m where
	inL = LazyNonIncOuter
	{-# INLINE inL #-}
instance (Typeable r,Typeable m) => InLayer Inside LazyNonInc r m where
	inL = LazyNonIncInner
	{-# INLINE inL #-}

proxyLazyNonInc :: Proxy LazyNonInc
proxyLazyNonInc = Proxy






