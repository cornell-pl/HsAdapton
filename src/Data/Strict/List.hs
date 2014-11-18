{-# LANGUAGE DeriveDataTypeable #-}

module Data.Strict.List where

import Prelude hiding (mapM,mapM_,map)
import Data.Typeable

-- | A strict list datatype
data SList a = SCons !a !(SList a) | SNil deriving (Show,Eq,Ord,Typeable)

head :: SList a -> a
head (SCons x _) = x

foldM :: Monad m => (a -> b -> m a) -> a -> SList b -> m a
foldM f a SNil = return a
foldM f a (SCons b bs) = a `f` b >>= \a' -> foldM f a' bs

map :: (a -> b) -> SList a -> SList b
map f SNil = SNil
map f (SCons x xs) = SCons (f x) (map f xs)

mapM :: Monad m => (a -> m b) -> SList a -> m (SList b)
mapM f SNil = return SNil
mapM f (SCons a as) = do
	b <- f a
	bs <- mapM f as
	return $ SCons b bs
	
mapM_ :: Monad m => (a -> m b) -> SList a -> m ()
mapM_ f SNil = return ()
mapM_ f (SCons a as) = f a >> mapM_ f as

{-# INLINE [0] foldr #-}
foldr :: (a -> b -> b) -> b -> SList a -> b
foldr k z = go
          where
            go SNil     = z
            go (SCons y ys) = y `k` go ys

toList :: SList a -> [a]
toList SNil = []
toList (SCons x xs) = x : toList xs