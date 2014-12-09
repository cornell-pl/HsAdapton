{-# LANGUAGE DeriveDataTypeable #-}

module Data.Strict.List where

import Prelude hiding (mapM,mapM_,map)
import Data.Typeable
import Data.Foldable

-- | A strict list datatype
data SList a = SCons !a !(SList a) | SNil deriving (Show,Eq,Ord,Typeable)

instance Foldable SList where
	{-# INLINE [0] foldr #-}
	foldr k z = go
	          where
	            go SNil     = z
	            go (SCons y ys) = y `k` go ys

head :: SList a -> a
head (SCons x _) = x

map :: (a -> b) -> SList a -> SList b
map f SNil = SNil
map f (SCons x xs) = SCons (f x) (map f xs)

mapM :: Monad m => (a -> m b) -> SList a -> m (SList b)
mapM f SNil = return SNil
mapM f (SCons a as) = do
	b <- f a
	bs <- mapM f as
	return $ SCons b bs