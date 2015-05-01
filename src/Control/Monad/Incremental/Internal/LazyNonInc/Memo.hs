{-# LANGUAGE UndecidableInstances, BangPatterns, FunctionalDependencies, MultiParamTypeClasses, MagicHash, ScopedTypeVariables, GADTs, FlexibleContexts, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module Control.Monad.Incremental.Internal.LazyNonInc.Memo (
	Memo(..), Hashable(..)
	) where

import System.Mem.MemoTable
import Data.Hashable
import Control.Monad.Incremental.Internal.LazyNonInc.Types
import Control.Monad.Incremental.Internal.LazyNonInc.Layers
import Control.Monad.Incremental.Internal.LazyNonInc.Display
import Data.Unique

import Control.Monad.IO.Class
import Debug
import Control.Concurrent
import System.Mem.Weak.Exts
import Control.Monad.Incremental

import System.IO.Unsafe
import System.Mem.StableName.Exts
import Data.Typeable
import Data.Memo

instance (Typeable inc,Typeable l,Typeable a) => Memo (LazyNonIncM l inc a) where
	type Key (LazyNonIncM l inc a) = StableName (LazyNonIncM l inc a)
	{-# INLINE memoKey #-}
	memoKey = stableName
	{-# INLINE memoWeak #-}
	memoWeak = \(LazyNonIncM r) -> MkWeak $ mkWeakRefKey r
           
instance (Typeable inc,Typeable l,Typeable a) => Memo (LazyNonIncU l inc a) where
	type Key (LazyNonIncU l inc a) = StableName (LazyNonIncU l inc a)
	{-# INLINE memoKey #-}
	memoKey = stableName
	{-# INLINE memoWeak #-}
	memoWeak = \(LazyNonIncU r) -> MkWeak $ mkWeakRefKey r

instance (Typeable inc,Typeable l,Typeable a) => Memo (LazyNonIncL l inc a) where
	type Key (LazyNonIncL l inc a) = StableName (LazyNonIncL l inc a)
	{-# INLINE memoKey #-}
	memoKey = stableName
	{-# INLINE memoWeak #-}
	memoWeak = \(LazyNonIncL r) -> MkWeak $ mkWeakRefKey r

instance Hashable (LazyNonIncU l inc a) where
	hashWithSalt i u = hashWithSalt i (hashStableName $ stableName u)
instance Hashable (LazyNonIncM l inc a) where
	hashWithSalt i m = hashWithSalt i (hashStableName $ stableName m)
instance Hashable (LazyNonIncL l inc a) where
	hashWithSalt i l = hashWithSalt i (hashStableName $ stableName l)
