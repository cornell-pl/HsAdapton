{-# LANGUAGE IncoherentInstances, Rank2Types, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

{-# OPTIONS_GHC -cpp                  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Text
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2003
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  generics@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Data.Generics.Basics)
--
-- \"Scrap your boilerplate\" --- Generic programming in Haskell 
-- See <http://www.cs.uu.nl/wiki/GenericProgramming/SYB>. The present module
-- provides generic operations for text serialisation of terms.
--
-----------------------------------------------------------------------------

module Data.WithClass.MGenerics.Text (

    -- * Generic show
    gshow, gshows,

    -- * Generic read
--    gread

 ) where

------------------------------------------------------------------------------

#ifdef __HADDOCK__
import Prelude
#endif
import Control.Monad
import Data.WithClass.MData
import Data.Data(Data)
import Data.WithClass.MGenerics.Aliases
import Text.ParserCombinators.ReadP
import Data.Generics.SYB.WithClass.Context

------------------------------------------------------------------------------

-- | Generic show: an alternative to \"deriving Show\"
gshow :: MData NoCtx m a => a -> m String
gshow x = gshows proxyNoCtx x ""

type MShowS m = String -> m String

-- | Generic shows
gshows :: MData ctx m a => Proxy ctx -> a -> MShowS m
gshows ctx = defshow `extQ` mshowString
  where aux a = return $ mshowChar ' ' <=< gshows ctx a
        defshow t s = do
			ms <- gmapQ ctx aux t
			str <- liftM showConstr $ toConstr ctx t
			let e = mshowChar '(' <=< (mshowString str) <=< (foldr (<=<) return ms) <=< mshowChar ')'
			e s

mshowChar :: Monad m => Char -> MShowS m
mshowChar c s = return (c:s)

mshow :: (Show a,Monad m) => a -> MShowS m
mshow x s = return $ show x ++ s

mshowString :: Monad m => String -> MShowS m
mshowString c s = return (c++s)

---- | Generic read: an alternative to \"deriving Read\"
--gread :: Data a => ReadS a
--
--{-
--
--This is a read operation which insists on prefix notation.  (The
--Haskell 98 read deals with infix operators subject to associativity
--and precedence as well.) We use fromConstrM to "parse" the input. To be
--precise, fromConstrM is used for all types except String. The
--type-specific case for String uses basic String read.
--
---}
--
--gread = readP_to_S gread'
--
-- where
--
--  -- Helper for recursive read
--  gread' :: Data a' => ReadP a'
--  gread' = allButString `extR` stringCase
--
--   where
--
--    -- A specific case for strings
--    stringCase :: ReadP String
--    stringCase = readS_to_P reads
--
--    -- Determine result type
--    myDataType = dataTypeOf (getArg allButString)
--     where
--      getArg :: ReadP a'' -> a''
--      getArg = undefined
--
--    -- The generic default for gread
--    allButString =
--      do
--                -- Drop "  (  "
--         skipSpaces                     -- Discard leading space
--         _ <- char '('                  -- Parse '('
--         skipSpaces                     -- Discard following space
--
--                -- Do the real work
--         str  <- parseConstr            -- Get a lexeme for the constructor
--         con  <- str2con str            -- Convert it to a Constr (may fail)
--         x    <- fromConstrM gread' con -- Read the children
--
--                -- Drop "  )  "
--         skipSpaces                     -- Discard leading space
--         _ <- char ')'                  -- Parse ')'
--         skipSpaces                     -- Discard following space
--
--         return x
--
--    -- Turn string into constructor driven by the requested result type,
--    -- failing in the monad if it isn't a constructor of this data type
--    str2con :: String -> ReadP Constr
--    str2con = maybe mzero return
--            . readConstr myDataType
--
--    -- Get a Constr's string at the front of an input string
--    parseConstr :: ReadP String
--    parseConstr =
--               string "[]"     -- Compound lexeme "[]"
--          <++  string "()"     -- singleton "()"
--          <++  infixOp         -- Infix operator in parantheses
--          <++  readS_to_P lex  -- Ordinary constructors and literals
--
--    -- Handle infix operators such as (:)
--    infixOp :: ReadP String
--    infixOp = do c1  <- char '('
--                 str <- munch1 (not . (==) ')')
--                 c2  <- char ')'
--                 return $ [c1] ++ str ++ [c2]
--
