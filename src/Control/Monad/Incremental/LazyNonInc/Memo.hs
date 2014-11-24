{-# LANGUAGE BangPatterns, FunctionalDependencies, MultiParamTypeClasses, MagicHash, ScopedTypeVariables, GADTs, FlexibleContexts, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module Control.Monad.Incremental.LazyNonInc.Memo (
	Memo(..), Hashable(..)
	) where

import System.Mem.MemoTable
import Data.Hashable
import Control.Monad.Incremental.LazyNonInc.Types
import Control.Monad.Incremental.LazyNonInc.Layers
import Control.Monad.Incremental.LazyNonInc.Display
import Data.Unique
import Control.Monad.IO.Class
import Debug
import Control.Concurrent
import System.Mem.Weak
import System.Mem.WeakTable as WeakTable
import Control.Monad.Incremental
import Control.Monad.Ref
import System.IO.Unsafe
import System.Mem.StableName
import System.Mem.WeakKey as WeakKey

instance WeakRef r => Memo (LazyNonIncM l inc r m a) where
	type Key (LazyNonIncM l inc r m a) = StableName (LazyNonIncM l inc r m a)
	{-# INLINE memoKey #-}
	memoKey x@(LazyNonIncM r) = (MkWeak $ WeakKey.mkWeakRefKey r,unsafePerformIO $ makeStableName x)
                                 
instance WeakRef r => Memo (LazyNonIncU l inc r m a) where
	type Key (LazyNonIncU l inc r m a) = StableName (LazyNonIncU l inc r m a)
	{-# INLINE memoKey #-}
	memoKey x@(LazyNonIncU r) = (MkWeak $ WeakKey.mkWeakRefKey r,unsafePerformIO $ makeStableName x)

instance WeakRef r => Memo (LazyNonIncL l inc r m a) where
	type Key (LazyNonIncL l inc r m a) = StableName (LazyNonIncL l inc r m a)
	{-# INLINE memoKey #-}
	memoKey x@(LazyNonIncL r) = (MkWeak $ WeakKey.mkWeakRefKey r,unsafePerformIO $ makeStableName x)

instance Hashable (LazyNonIncU l inc r m a) where
	hashWithSalt i u = hashWithSalt i (hashStableName $ unsafePerformIO $ makeStableName u)
instance Hashable (LazyNonIncM l inc r m a) where
	hashWithSalt i m = hashWithSalt i (hashStableName $ unsafePerformIO $ makeStableName m)
instance Hashable (LazyNonIncL l inc r m a) where
	hashWithSalt i l = hashWithSalt i (hashStableName $ unsafePerformIO $ makeStableName l)
