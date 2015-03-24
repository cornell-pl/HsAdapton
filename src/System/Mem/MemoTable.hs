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
import Data.Global.Dynamic

import GHC.Base
import Control.Monad.Trans
import Data.Typeable
import Data.Unique
import Data.Map as Map
import Data.Set as Set
import Data.Typeable

import Debug

instance Show (StableName a) where
	show x = "StableName" ++ show (hashStableName x)

-- | Class for indexing arbitrary values in a memotable
-- the keys should uniquely identify the values, what is necessary for correct incrementality
class (Typeable a,Typeable (Key a),Hashable (Key a),Eq (Key a)) => Memo a where
	type Key a :: *
	-- | returns a function that creates a weak pointers with the argument as key and unique key for a given value
	memoKey :: a -> Key a
	memoWeak :: a -> MkWeak

instance Memo Int where
	type Key Int = Int
	{-# INLINE memoKey #-}
	memoKey = id
	{-# INLINE memoWeak #-}
	memoWeak = \x -> MkWeak $ mkWeak x
instance Memo Integer where
	type Key Integer = Integer
	{-# INLINE memoKey #-}
	memoKey = id
	{-# INLINE memoWeak #-}
	memoWeak = \x -> MkWeak $ mkWeak x
instance Memo Bool where
	type Key Bool = Bool
	{-# INLINE memoKey #-}
	memoKey = id
	{-# INLINE memoWeak #-}
	memoWeak = \x -> MkWeak $ mkWeak x
instance Memo Char where
	type Key Char = Char
	{-# INLINE memoKey #-}
	memoKey = id
	{-# INLINE memoWeak #-}
	memoWeak = \x -> MkWeak $ mkWeak x
instance Memo Float where
	type Key Float = Float
	{-# INLINE memoKey #-}
	memoKey = id
	{-# INLINE memoWeak #-}
	memoWeak = \x -> MkWeak $ mkWeak x
instance Memo Double where
	type Key Double = Double
	{-# INLINE memoKey #-}
	memoKey = id
	{-# INLINE memoWeak #-}
	memoWeak = \x -> MkWeak $ mkWeak x
instance Memo Word where
	type Key Word = Word
	{-# INLINE memoKey #-}
	memoKey = id
	{-# INLINE memoWeak #-}
	memoWeak = \x -> MkWeak $ mkWeak x
instance Memo Word8 where
	type Key Word8 = Word8
	{-# INLINE memoKey #-}
	memoKey = id
	{-# INLINE memoWeak #-}
	memoWeak = \x -> MkWeak $ mkWeak x
instance Memo Word16 where
	type Key Word16 = Word16
	{-# INLINE memoKey #-}
	memoKey = id
	{-# INLINE memoWeak #-}
	memoWeak = \x -> MkWeak $ mkWeak x
instance Memo Word32 where
	type Key Word32 = Word32
	{-# INLINE memoKey #-}
	memoKey = id
	{-# INLINE memoWeak #-}
	memoWeak = \x -> MkWeak $ mkWeak x
instance Memo Word64 where
	type Key Word64 = Word64
	{-# INLINE memoKey #-}
	memoKey = id
	{-# INLINE memoWeak #-}
	memoWeak = \x -> MkWeak $ mkWeak x
instance Memo Int8 where
	type Key Int8 = Int8
	{-# INLINE memoKey #-}
	memoKey = id
	{-# INLINE memoWeak #-}
	memoWeak = \x -> MkWeak $ mkWeak x
instance Memo Int16 where
	type Key Int16 = Int16
	{-# INLINE memoKey #-}
	memoKey = id
	{-# INLINE memoWeak #-}
	memoWeak = \x -> MkWeak $ mkWeak x
instance Memo Int32 where
	type Key Int32 = Int32
	{-# INLINE memoKey #-}
	memoKey = id
	{-# INLINE memoWeak #-}
	memoWeak = \x -> MkWeak $ mkWeak x
instance Memo Int64 where
	type Key Int64 = Int64
	{-# INLINE memoKey #-}
	memoKey = id
	{-# INLINE memoWeak #-}
	memoWeak = \x -> MkWeak $ mkWeak x

instance (Typeable a,Typeable b) => Memo (Map a b) where
	type Key (Map a b) = StableName (Map a b)
	{-# INLINE memoKey #-}
	memoKey = stableName
	{-# INLINE memoWeak #-}
	memoWeak = \x -> MkWeak $ mkWeak x
instance Memo a => Memo (Maybe a) where
	type Key (Maybe a) = Maybe (Key a)
	{-# INLINE memoKey #-}
	memoKey = fmap memoKey
	{-# INLINE memoWeak #-}
	memoWeak Nothing = MkWeak $ mkWeak Nothing
	memoWeak (Just a) = MkWeak $ mkWeak a
instance (Memo a,Memo b) => Memo (Either a b) where
	type Key (Either a b) = Either (Key a) (Key b)
	{-# INLINE memoKey #-}
	memoKey = either (Left . memoKey) (Right . memoKey)
	{-# INLINE memoWeak #-}
	memoWeak = either memoWeak memoWeak
instance Typeable a => Memo (Set a) where
	type Key (Set a) = StableName (Set a)
	{-# INLINE memoKey #-}
	memoKey = stableName
	{-# INLINE memoWeak #-}
	memoWeak = \x -> MkWeak $ mkWeak x
instance Typeable a => Memo [a] where
	type Key [a] = StableName [a]
	{-# INLINE memoKey #-}
	memoKey = stableName
	{-# INLINE memoWeak #-}
	memoWeak = \x -> MkWeak $ mkWeak x

instance Memo () where
	type Key () = ()
	{-# INLINE memoKey #-}
	memoKey = id
	{-# INLINE memoWeak #-}
	memoWeak = \x -> MkWeak $ mkWeak x

-- for standard memoization over any other data type besides primitive types, use this instance
--instance Memo a where
--	type Key a = StableName a
--	{-# INLINE memoKey #-}
--	memoKey = stableName
--	{-# INLINE memoWeak #-}
--	memoWeak = \x -> MkWeak $ mkWeak x
-- or for no memoization at all, this instance
--instance Memo a where
--	type Key a = Neq
--	{-# INLINE memoKey #-}
--	memoKey = \x -> Neq
--	{-# INLINE memoWeak #-}
--	memoWeak = \x -> MkWeak mkDeadWeak

-- | An empty type whose values are always different
data Neq = Neq deriving (Show,Typeable)

instance Hashable Neq where
	hashWithSalt i _ = hashWithSalt i (0::Int)
instance Eq Neq where
	_ == _ = False

type MemoTable a b = WeakTable a b

--{-# INLINE memoLazy #-}
---- | Memoizes a function with pointer equality, that does not evaluate the arguments and preserves laziness
--memoLazy :: (Memo a) => (a -> b) -> a -> b
--memoLazy = memo False
--
--{-# INLINE memoStrict #-}
---- | Memoizes a function with strict equality, that loses laziness but Adaptonreases sharing of memoized results
--memoStrict :: (Memo a) => (a -> b) -> a -> b
--memoStrict = memo True
--
--{-# NOINLINE memo #-}
---- | @memo@ takes a function with arbitrary range and domain, and returns a memoized version of the function
--memo :: (Memo a) => Bool -> (a -> b) -> a -> b
--memo isStrict f =
--	let tbl = WeakTable.unsafeNewFor f
--	in memo' isStrict f tbl
--	
--{-# NOINLINE memo' #-}
--memo' :: (Memo a) => Bool -> (a -> b) -> MemoTable (Key a) b -> a -> b
--memo' isStrict f tbl arg = unsafePerformIO $ do
--	let (mkWeak,k) = if isStrict then memoKey $! arg else memoKey arg
--	lkp <- debug ("memo search1 " ) $ WeakTable.lookup tbl k
--	case lkp of
--		Nothing -> do
--			let val = f arg
--			WeakTable.insertWithMkWeak tbl mkWeak k val
--			return val
--		Just val -> debug ("memo hit1") $ return val   
   
--memo :: (Eq a,Memo arg) => ((arg -> l inc r m (mod l inc r m a)) -> arg -> l inc r m a) -> (arg -> l inc r m (mod l inc r m a))

fixMemoStrictM :: (Typeable m,Typeable a,MonadIO m,Memo arg) => ((arg -> m a) -> arg -> m a) -> (arg -> m a)
fixMemoStrictM f = let memo_func = memoStrictM (f memo_func) in memo_func

fixMemoStrictM2 :: (Typeable m,Typeable a,MonadIO m,Memo arg1,Memo arg2) => ((arg1 -> arg2 -> m a) -> arg1 -> arg2 -> m a) -> (arg1 -> arg2 -> m a)
fixMemoStrictM2 f = curry (fixMemoStrictM (uncurry . f . curry))

--memoU :: (MonadIO m,Eq a,Output U Inside inc r m,Memo arg) => ((arg -> Inside inc r m (U Inside inc r m a)) -> arg -> Inside inc r m a) -> (arg -> Inside inc r m (U Inside inc r m a))
--memoU f = let memo_func = memoNonRecU (thunk . f memo_func) in memo_func

{-# INLINE memoLazyM #-}
memoLazyM :: (Typeable m,Typeable b,Typeable a,MonadIO m,Memo a) => (a -> m b) -> a -> m b
memoLazyM = memoM False

{-# INLINE memoStrictM #-}
memoStrictM :: (Typeable m,Typeable b,Typeable a,MonadIO m,Memo a) => (a -> m b) -> a -> m b
memoStrictM = memoM True

-- | memoizes a monadic function. the difference to @memo@ is that the result @b@ is memoized, rather than the monadic computation @m b@
--this is only to be used with our internal moinads, memoizing in this way, e.g., a State monad would not produce valid results
memoM :: (Typeable m,Typeable b,Typeable a,MonadIO m,Memo a) => Bool -> (a -> m b) -> a -> m b
memoM isStrict f =
	let tbl = declareWeakTable f (stableName f)
	in memoM' isStrict f tbl

memoM' :: (MonadIO m,Memo a) => Bool -> (a -> m b) -> MemoTable (Key a) b -> a -> m b
memoM' isStrict f tbl arg = do
		let (mkWeak,k) = if isStrict then (memoWeak $! arg,memoKey $! arg) else (memoWeak arg,memoKey arg)
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
	memoKey (x,y) = (memoKey x,memoKey y)
	{-# INLINE memoWeak #-}
	memoWeak (x,y) = memoWeak x `andMkWeak` memoWeak y

instance (Memo a,Memo b,Memo c) => Memo (a,b,c) where
	type Key (a,b,c) = (Key a,Key b,Key c)
	{-# INLINE memoKey #-}
	memoKey (x,y,z) = (memoKey x,memoKey y,memoKey z)
	{-# INLINE memoWeak #-}
	memoWeak (x,y,z) = memoWeak x `andMkWeak` memoWeak y `andMkWeak` memoWeak z




