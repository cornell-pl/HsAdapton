{-# LANGUAGE DeriveDataTypeable #-}

module Data.Strict.NeList where

import Prelude hiding (mapM,mapM_,map,sequence,length,last,take)
import Data.Typeable
import Data.Foldable as Foldable

-- | A strict list datatype
data NeList a = NeCons !a !(NeList a) | NeWrap !a deriving (Show,Eq,Ord,Typeable)

instance Foldable NeList where
	{-# INLINe [0] foldr #-}
	foldr k z = go
	          where
	            go (NeWrap y)     = y `k` z
	            go (NeCons y ys) = y `k` go ys

last :: NeList a -> a
last (NeWrap x) = x
last (NeCons x xs) = last xs

head :: NeList a -> a
head (NeWrap x) = x
head (NeCons x _) = x

map :: (a -> b) -> NeList a -> NeList b
map f (NeWrap x) = NeWrap (f x)
map f (NeCons x xs) = NeCons (f x) (map f xs)

mapM :: Monad m => (a -> m b) -> NeList a -> m (NeList b)
mapM f (NeWrap a) = do
	b <- f a
	return $ NeWrap b
mapM f (NeCons a as) = do
	b <- f a
	bs <- mapM f as
	return $ NeCons b bs

sequence :: Monad m => NeList (m a) -> m (NeList a)
sequence (NeWrap m) = do
	x <- m
	return $ NeWrap x
sequence (NeCons m ms) = do
	x <- m
	xs <- sequence ms
	return $ NeCons x xs

reverse :: NeList a -> NeList a
reverse (NeWrap x) = NeWrap x
reverse (NeCons x xs) = Foldable.foldl' (flip NeCons) (NeWrap x) xs

length :: NeList a -> Int
length (NeWrap x) = 1
length (NeCons x xs) = succ (length xs)

take :: Int -> NeList a -> NeList a
take 0 xs = error "take must be grater than 0"
take 1 (NeWrap x) = NeWrap x
take i (NeCons x xs) = NeCons x $ take (pred i) xs


