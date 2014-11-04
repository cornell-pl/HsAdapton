{-# LANGUAGE DeriveDataTypeable #-}

module Data.Strict.Maybe where

import Data.Typeable

data SMaybe a = SJust !a | SNothing deriving (Show,Eq,Ord,Typeable)