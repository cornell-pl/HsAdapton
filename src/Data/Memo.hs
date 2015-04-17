{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, TypeFamilies #-}

module Data.Memo where

import System.Mem.Weak.Exts

import System.Mem.StableName.Exts

import Data.Set
import Data.Map
import GHC.Int
import GHC.Word
import Data.Typeable
import Data.Hashable
import Data.Unique


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

instance Show (StableName a) where
	show x = "StableName" ++ show (hashStableName x)

-- | Class for indexing arbitrary values in a memotable
-- the keys should uniquely identify the values, what is necessary for correct incrementality
class (Typeable a,Typeable (Key a),Hashable (Key a),Eq (Key a)) => Memo a where
	type Key a :: *
	-- | returns a function that creates a weak pointers with the argument as key and unique key for a given value
	memoKey :: a -> Key a
	memoWeak :: a -> MkWeak

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

-- | An empty type whose values are always different
data Neq = Neq deriving (Show,Typeable)

instance Hashable Neq where
	hashWithSalt i _ = hashWithSalt i (0::Int)
instance Eq Neq where
	_ == _ = False