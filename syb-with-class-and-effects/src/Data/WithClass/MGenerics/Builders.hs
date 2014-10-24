{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Builders
-- Copyright   :  (c) 2008 Universiteit Utrecht
-- License     :  BSD-style
-- 
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module provides generic builder functions. These functions construct
-- values of a given type.
-----------------------------------------------------------------------------

module Data.MGenerics.Builders (empty, constrs) where

import Data.MData
import Data.Data (Data)
import qualified Data.Data as Data
import Data.MGenerics.Aliases (extB,GenericB)

-- | Construct the empty value for a datatype. For algebraic datatypes, the
-- leftmost constructor is chosen.
empty :: GenericB m
empty = general undefined
      `extB` char 
      `extB` int
      `extB` integer
      `extB` float 
      `extB` double where
  -- Generic case
  general :: MData m a => a -> m a
  general proxy = dataTypeOf proxy >>= \p -> fromConstrB empty (indexConstr p 1)
  
  -- Base cases
  char    = return '\NUL'
  int     = return 0      :: Monad m => m Int
  integer = return 0      :: Monad m => m Integer
  float   = return 0.0    :: Monad m => m Float
  double  = return 0.0    :: Monad m => m Double


-- | Return a list of values of a datatype. Each value is one of the possible
-- constructors of the datatype, populated with 'empty' values.
constrs :: forall m . (forall a. MData m a => m [a])
constrs = general undefined
      `extB` char
      `extB` int
      `extB` integer
      `extB` float
      `extB` double where
  -- Generic case
  general :: MData m a => a -> m [a]
  general proxy = dataTypeOf proxy >>= \p -> mapM (fromConstrB empty)
              (dataTypeConstrs p) where

  -- Base cases
  char    = return "\NUL"
  int     = return [0   :: Int]
  integer = return [0   :: Integer]
  float   = return [0.0 :: Float]
  double  = return [0.0 :: Double]
