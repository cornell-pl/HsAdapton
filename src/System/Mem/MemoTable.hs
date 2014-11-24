{-# LANGUAGE DeriveDataTypeable, Rank2Types, ImpredicativeTypes, BangPatterns, FunctionalDependencies, MultiParamTypeClasses, MagicHash, ScopedTypeVariables, GADTs, FlexibleContexts, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module System.Mem.MemoTable where

-- | Implementation of memo tables using hash tables and weak pointers as presented in http://community.haskell.org/~simonmar/papers/weak.pdf.
-- | Requires the package hashtables.

import System.Mem.WeakTable as WeakTable
import System.Mem.Weak as Weak
import System.Mem.WeakKey as WeakKey
import System.Mem.StableName
import System.IO.Unsafe
import Data.HashTable.IO as HashIO
import Data.HashTable.ST.Basic
import Control.Monad
import Data.Hashable
import GHC.Word
import GHC.Int

import GHC.Base
import Control.Monad.Trans
import Data.Typeable
import Data.Unique
import Data.Map as Map
import Data.Set as Set

import Debug

instance Show (StableName a) where
	show x = "StableName" ++ show (hashStableName x)

-- | Class for indexing arbitrary values in a memotable
-- the keys should uniquely identify the values, what is necessary for correct incrementality
class (Hashable (Key a),Show (Key a),Eq (Key a)) => Memo a where
	type Key a :: *
	-- | returns a function that creates a weak pointers with the argument as key and unique key for a given value
	memoKey :: a -> (MkWeak,Key a)

instance Memo Int where
	type Key Int = Int
	{-# INLINE memoKey #-}
	memoKey i = (MkWeak $ Weak.mkWeak i,i)
instance Memo Integer where
	type Key Integer = Integer
	{-# INLINE memoKey #-}
	memoKey i = (MkWeak $ Weak.mkWeak i,i)
instance Memo Bool where
	type Key Bool = Bool
	{-# INLINE memoKey #-}
	memoKey b = (MkWeak $ Weak.mkWeak b,b)
instance Memo Char where
	type Key Char = Char
	{-# INLINE memoKey #-}
	memoKey c = (MkWeak $ Weak.mkWeak c,c)
instance Memo Float where
	type Key Float = Float
	{-# INLINE memoKey #-}
	memoKey f = (MkWeak $ Weak.mkWeak f,f)
instance Memo Double where
	type Key Double = Double
	{-# INLINE memoKey #-}
	memoKey d = (MkWeak $ Weak.mkWeak d,d)
instance Memo Word where
	type Key Word = Word
	{-# INLINE memoKey #-}
	memoKey d = (MkWeak $ Weak.mkWeak d,d)
instance Memo Word8 where
	type Key Word8 = Word8
	{-# INLINE memoKey #-}
	memoKey d = (MkWeak $ Weak.mkWeak d,d)
instance Memo Word16 where
	type Key Word16 = Word16
	{-# INLINE memoKey #-}
	memoKey d = (MkWeak $ Weak.mkWeak d,d)
instance Memo Word32 where
	type Key Word32 = Word32
	{-# INLINE memoKey #-}
	memoKey d = (MkWeak $ Weak.mkWeak d,d)
instance Memo Word64 where
	type Key Word64 = Word64
	{-# INLINE memoKey #-}
	memoKey d = (MkWeak $ Weak.mkWeak d,d)
instance Memo Int8 where
	type Key Int8 = Int8
	{-# INLINE memoKey #-}
	memoKey d = (MkWeak $ Weak.mkWeak d,d)
instance Memo Int16 where
	type Key Int16 = Int16
	{-# INLINE memoKey #-}
	memoKey d = (MkWeak $ Weak.mkWeak d,d)
instance Memo Int32 where
	type Key Int32 = Int32
	{-# INLINE memoKey #-}
	memoKey d = (MkWeak $ Weak.mkWeak d,d)
instance Memo Int64 where
	type Key Int64 = Int64
	{-# INLINE memoKey #-}
	memoKey d = (MkWeak $ Weak.mkWeak d,d)

instance Memo (Map a b) where
	type Key (Map a b) = StableName (Map a b)
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)
instance Memo (Maybe a) where
	type Key (Maybe a) = StableName (Maybe a)
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)
instance Memo (Either a b) where
	type Key (Either a b) = StableName (Either a b)
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)
instance Memo (Set a) where
	type Key (Set a) = StableName (Set a)
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)
instance Memo [a] where
	type Key [a] = StableName [a]
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)

-- for standard memoization over any other data type besides primitive types, use this instance
--instance Memo a where
--	type Key a = StableName a
--	{-# INLINE memoKey #-}
--	memoKey x = (MkWeak $ mkWeak x,unsafePerformIO $ makeStableName x)
-- or for no memoization at all, this instance
--instance Memo a where
--	type Key a = Neq
--	{-# INLINE memoKey #-}
--	memoKey x = (MkWeak mkDeadWeak,Neq)

-- | An empty type whose values are always different
data Neq = Neq deriving (Show,Typeable)

instance Hashable Neq where
	hashWithSalt i _ = hashWithSalt i (0::Int)
instance Eq Neq where
	_ == _ = False

type MemoTable a b = WeakTable a b

{-# INLINE memoLazy #-}
-- | Memoizes a function with pointer equality, that does not evaluate the arguments and preserves laziness
memoLazy :: (Memo a) => (a -> b) -> a -> b
memoLazy = memo False

{-# INLINE memoStrict #-}
-- | Memoizes a function with strict equality, that loses laziness but Adaptonreases sharing of memoized results
memoStrict :: (Memo a) => (a -> b) -> a -> b
memoStrict = memo True

{-# NOINLINE memo #-}
-- | @memo@ takes a function with arbitrary range and domain, and returns a memoized version of the function
memo :: (Memo a) => Bool -> (a -> b) -> a -> b
memo isStrict f =
	let tbl = unsafePerformIO $ WeakTable.newFor f
	in memo' isStrict f tbl
	
{-# NOINLINE memo' #-}
memo' :: (Memo a) => Bool -> (a -> b) -> MemoTable (Key a) b -> a -> b
memo' isStrict f tbl arg = unsafePerformIO $ do
	let (mkWeak,k) = if isStrict then memoKey $! arg else memoKey arg
	lkp <- debug ("memo search1 " ) $ WeakTable.lookup tbl k
	case lkp of
		Nothing -> do
			let val = f arg
			WeakTable.insertWithMkWeak tbl mkWeak k val
			return val
		Just val -> debug ("memo hit1") $ return val   
   
--memo :: (Eq a,Memo arg) => ((arg -> l inc r m (mod l inc r m a)) -> arg -> l inc r m a) -> (arg -> l inc r m (mod l inc r m a))

fixMemoStrictM :: (MonadIO m,Memo arg) => ((arg -> m a) -> arg -> m a) -> (arg -> m a)
fixMemoStrictM f = let memo_func = memoStrictM (f memo_func) in memo_func

fixMemoStrictM2 :: (MonadIO m,Memo arg1,Memo arg2) => ((arg1 -> arg2 -> m a) -> arg1 -> arg2 -> m a) -> (arg1 -> arg2 -> m a)
fixMemoStrictM2 f = curry (fixMemoStrictM (uncurry . f . curry))

--memoU :: (MonadIO m,Eq a,Output U Inside inc r m,Memo arg) => ((arg -> Inside inc r m (U Inside inc r m a)) -> arg -> Inside inc r m a) -> (arg -> Inside inc r m (U Inside inc r m a))
--memoU f = let memo_func = memoNonRecU (thunk . f memo_func) in memo_func

{-# INLINE memoLazyM #-}
memoLazyM :: (MonadIO m,Memo a) => (a -> m b) -> a -> m b
memoLazyM = memoM False

{-# INLINE memoStrictM #-}
memoStrictM :: (MonadIO m,Memo a) => (a -> m b) -> a -> m b
memoStrictM = memoM True

-- | memoizes a monadic function. the difference to @memo@ is that the result @b@ is memoized, rather than the monadic computation @m b@
--this is only to be used with our internal moinads, memoizing in this way, e.g., a State monad would not produce valid results
memoM :: (MonadIO m,Memo a) => Bool -> (a -> m b) -> a -> m b
memoM isStrict f =
	let tbl = unsafePerformIO $ WeakTable.newFor f
	in memoM' isStrict f tbl

memoM' :: (MonadIO m,Memo a) => Bool -> (a -> m b) -> MemoTable (Key a) b -> a -> m b
memoM' isStrict f tbl arg = do
		let (mkWeak,k) = if isStrict then memoKey $! arg else memoKey arg
		lkp <- debug ("memo search2 ") $ liftIO $ WeakTable.lookup tbl k
		case lkp of
			Nothing -> do
				val <- f arg
				liftIO $ WeakTable.insertWithMkWeak tbl mkWeak k val
				return val
			Just val -> debug ("memoM hit2 ") $ return val

instance Show Unique where
	show u = show $ hashUnique u

instance Hashable Unique where
	hashWithSalt i u = hashWithSalt i (hashUnique u)
	
instance (Memo a,Memo b) => Memo (a,b) where
	type Key (a,b) = (Key a,Key b)
	{-# INLINE memoKey #-}
	memoKey (x,y) = (andMkWeak wx wy,(kx,ky))
		where (wx,kx) = memoKey x
		      (wy,ky) = memoKey y

instance (Memo a,Memo b,Memo c) => Memo (a,b,c) where
	type Key (a,b,c) = (Key a,Key b,Key c)
	{-# INLINE memoKey #-}
	memoKey (x,y,z) = (andMkWeak wx (andMkWeak wy wz),(kx,ky,kz))
		where (wx,kx) = memoKey x
		      (wy,ky) = memoKey y
		      (wz,kz) = memoKey z




