{-# LANGUAGE DeriveDataTypeable #-}

module Data.Strict.Maybe where

import Data.Typeable

data Maybe a = Just !a | Nothing deriving (Show,Eq,Ord,Typeable)