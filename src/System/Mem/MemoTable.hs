{-# LANGUAGE CPP, UndecidableInstances, TemplateHaskell, DeriveDataTypeable, Rank2Types, ImpredicativeTypes, BangPatterns, FunctionalDependencies, MultiParamTypeClasses, MagicHash, ScopedTypeVariables, GADTs, FlexibleContexts, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module System.Mem.MemoTable (
	  module Data.HashTable.Weak.IO
	, MemoTable(..), memoLazy, memoStrict, memoLazyM, memoStrictM
	) where

-- | Implementation of memo tables using hash tables and weak pointers as presented in http://community.haskell.org/~simonmar/papers/weak.pdf.
-- | Requires the package hashtables.

import Prelude hiding (lookup)
import Data.Memo
import System.Mem.Weak.Exts as Weak
import System.Mem.StableName.Exts
import System.IO.Unsafe

import Control.Monad
import Data.Hashable
import GHC.Word
import GHC.Int
import Language.Haskell.Exts.Syntax as Exts
import Language.Haskell as H
import Data.HashTable.Weak.IO
--import Data.HashTable.IO
import Data.Derive.Internal.Derivation
import Data.DeriveTH
import Data.Derive.Memo

import GHC.Base
import Control.Monad.Trans
import Data.Typeable
import Data.Unique
import Data.Typeable
import Data.Global.Dynamic as Dyn
import Control.Monad.ST

import Debug

-- for IC lookup speed is the most important
-- since the hashtable using for the weaktable has an expensive resizing cost, defining a larger initial memotable size can improve a lot
--type MemoTable a b = BasicHashTable a b
type MemoTable a b = BasicHashTable a b

{-# INLINE memoLazy #-}
-- | Memoizes a function with pointer equality, that does not evaluate the arguments and preserves laziness
memoLazy :: (Typeable b,Memo a) => (a -> b) -> a -> b
memoLazy = memo False

{-# INLINE memoStrict #-}
-- | Memoizes a function with strict equality, that loses laziness but Adaptonreases sharing of memoized results
memoStrict :: (Typeable b,Memo a) => (a -> b) -> a -> b
memoStrict = memo True

{-# NOINLINE memo #-}
-- | @memo@ takes a function with arbitrary range and domain, and returns a memoized version of the function
memo :: (Typeable b,Memo a) => Bool -> (a -> b) -> a -> b
memo isStrict f =
	let tbl = Dyn.declareWeakBasicHashTable 1 (stableName f)
	in memo' isStrict f tbl
	
{-# NOINLINE memo' #-}
memo' :: (Memo a) => Bool -> (a -> b) -> MemoTable (Key a) b -> a -> b
memo' isStrict f tbl arg = unsafePerformIO $ do
	let (mkWeak,k) = if isStrict then (memoWeak $! arg,memoKey $! arg) else (memoWeak arg,memoKey arg)
	lkp <- debug ("memo search1 ") $ liftIO $ lookup tbl k
	case lkp of
		Nothing -> do
			let val = f arg
			liftIO $ insertWithMkWeak tbl k val mkWeak
			return val
		Just val -> debug ("memo hit1") $ return val   

{-# INLINE memoLazyM #-}
memoLazyM :: (Typeable m,Typeable b,Typeable a,MonadIO m,Memo a) => (a -> m b) -> a -> m b
memoLazyM = memoM False

{-# INLINE memoStrictM #-}
memoStrictM :: (Typeable m,Typeable b,Typeable a,MonadIO m,Memo a) => (a -> m b) -> a -> m b
memoStrictM = memoM True

-- | memoizes a monadic function. the difference to @memo@ is that the result @b@ is memoized, rather than the monadic computation @m b@
--this is only to be used with our internal moinads,MonadIO memoizing in this way, e.g., a State monad would not produce valid results
memoM :: (Typeable m,Typeable b,Typeable a,MonadIO m,Memo a) => Bool -> (a -> m b) -> a -> m b
memoM isStrict f =
	let tbl = Dyn.declareWeakBasicHashTable 1 (stableName f)
	in  memoM' isStrict f tbl

memoM' :: (MonadIO m,Memo a) => Bool -> (a -> m b) -> MemoTable (Key a) b -> a -> m b
memoM' isStrict f tbl arg = do
		let (mkWeak,k) = if isStrict then (memoWeak $! arg,memoKey $! arg) else (memoWeak arg,memoKey arg)
		lkp <- debug ("memo search2 ") $ liftIO $ lookup tbl k
		case lkp of
			Nothing -> do
				val <- f arg
				liftIO $ insertWithMkWeak tbl k val mkWeak
				return val
			Just val -> debug ("memoM hit2 ") $ return val

$(deriveMemoId ''Int)
$(deriveMemoId ''())
$(deriveMemoId ''Integer)
$(deriveMemoId ''Bool)
$(deriveMemoId ''Char)
$(deriveMemoId ''Float)
$(deriveMemoId ''Double)
$(deriveMemoId ''Word)
$(deriveMemoId ''Word8)
$(deriveMemoId ''Word16)
$(deriveMemoId ''Word32)
$(deriveMemoId ''Word64)
$(deriveMemoId ''Int8)
$(deriveMemoId ''Int16)
$(deriveMemoId ''Int32)
$(deriveMemoId ''Int64)
