{-# LANGUAGE UndecidableInstances, BangPatterns, FunctionalDependencies, MultiParamTypeClasses, MagicHash, ScopedTypeVariables, GADTs, FlexibleContexts, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

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
import Data.Typeable

instance (Typeable inc,Typeable l,Typeable r,Typeable m,Typeable a,WeakKey (r a),WeakRef r) => Memo (LazyNonIncM l inc r m a) where
	type Key (LazyNonIncM l inc r m a) = StableName (LazyNonIncM l inc r m a)
	{-# INLINE memoKey #-}
	memoKey = stableName
	{-# INLINE memoWeak #-}
	memoWeak = \(LazyNonIncM r) -> MkWeak $ WeakKey.mkWeakRefKey r
           
instance (Typeable inc,Typeable l,Typeable r,Typeable m,Typeable a,WeakKey (r a),WeakRef r) => Memo (LazyNonIncU l inc r m a) where
	type Key (LazyNonIncU l inc r m a) = StableName (LazyNonIncU l inc r m a)
	{-# INLINE memoKey #-}
	memoKey = stableName
	{-# INLINE memoWeak #-}
	memoWeak = \(LazyNonIncU r) -> MkWeak $ WeakKey.mkWeakRefKey r

instance (Typeable inc,Typeable l,Typeable r,Typeable m,Typeable a,WeakKey (r a),WeakRef r) => Memo (LazyNonIncL l inc r m a) where
	type Key (LazyNonIncL l inc r m a) = StableName (LazyNonIncL l inc r m a)
	{-# INLINE memoKey #-}
	memoKey = stableName
	{-# INLINE memoWeak #-}
	memoWeak = \(LazyNonIncL r) -> MkWeak $ WeakKey.mkWeakRefKey r

instance Hashable (LazyNonIncU l inc r m a) where
	hashWithSalt i u = hashWithSalt i (hashStableName $ stableName u)
instance Hashable (LazyNonIncM l inc r m a) where
	hashWithSalt i m = hashWithSalt i (hashStableName $ stableName m)
instance Hashable (LazyNonIncL l inc r m a) where
	hashWithSalt i l = hashWithSalt i (hashStableName $ stableName l)
