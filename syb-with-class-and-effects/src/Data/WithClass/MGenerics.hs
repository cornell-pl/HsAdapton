{-# OPTIONS_GHC -cpp                  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2004
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Data.Generics.Basics)
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell 
-- See <http://www.cs.uu.nl/wiki/GenericProgramming/SYB>. To scrap your
-- boilerplate it is sufficient to import the present module, which simply
-- re-exports all themes of the Data.Generics library.
--
-----------------------------------------------------------------------------

module Data.WithClass.MGenerics (

  -- * All Data.MGenerics modules
  module Data.WithClass.MData,               -- primitives and instances of the Data class
  module Data.WithClass.MGenerics.Aliases,   -- aliases for type case, generic types
  module Data.WithClass.MGenerics.Schemes,   -- traversal schemes (everywhere etc.)
  module Data.WithClass.MGenerics.Text,      -- generic read and show
--  module Data.WithClass.MGenerics.Twins,     -- twin traversal, e.g., generic eq
--  module Data.WithClass.MGenerics.Builders,  -- term builders

 ) where

------------------------------------------------------------------------------

import Data.WithClass.MData
import Data.WithClass.MGenerics.Instances ()
import Data.WithClass.MGenerics.Aliases
import Data.WithClass.MGenerics.Schemes
import Data.WithClass.MGenerics.Text
--import Data.WithClass.MGenerics.Twins
--import Data.WithClass.MGenerics.Builders
