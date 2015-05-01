{-# LANGUAGE ConstraintKinds, UndecidableInstances, FlexibleContexts, StandaloneDeriving, DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STxM.STxMVar
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires STxM)
--
-- STxMVar: Transactional MVars, for use in the STxM monad
-- (GHC only)
--
-----------------------------------------------------------------------------

module Control.Concurrent.Transactional.TMVar (
        STxMVar,STxMVarK,
        newSTxMVar,
        newEmptySTxMVar,
        takeSTxMVar,
        putSTxMVar,
        readSTxMVar,
        tryReadSTxMVar,
        swapSTxMVar,
        tryTakeSTxMVar,
        tryPutSTxMVar,
        isEmptySTxMVar
  ) where

import Prelude hiding (read)
import Control.Monad.Incremental
import Control.Concurrent.Transactional

import Data.Typeable (Typeable)

type STxMVarK mod inc a = (IncK inc (Maybe a),Transactional inc,Input mod STxM inc)

newtype STxMVar mod inc a = STxMVar (mod STxM inc (Maybe a)) deriving (Typeable)
deriving instance Eq (mod STxM inc (Maybe a)) => Eq (STxMVar mod inc a)
{- ^
A 'STxMVar' is a synchronising variable, used
for communication between concurrent threads.  It can be thought of
as a box, which may be empty or full.
-}

-- |Create a 'STxMVar' which contains the supplied value.
newSTxMVar :: STxMVarK mod inc a => a -> STxM inc (STxMVar mod inc a)
newSTxMVar a = do
  t <- ref (Just a)
  return (STxMVar t)

-- |Create a 'STxMVar' which is initially empty.
newEmptySTxMVar :: STxMVarK mod inc a => STxM inc (STxMVar mod inc a)
newEmptySTxMVar = do
  t <- ref Nothing
  return (STxMVar t)

-- |Return the contents of the 'STxMVar'.  If the 'STxMVar' is currently
-- empty, the transaction will 'retry'.  After a 'takeSTxMVar',
-- the 'STxMVar' is left empty.
takeSTxMVar :: STxMVarK mod inc a => STxMVar mod inc a -> STxM inc a
takeSTxMVar (STxMVar t) = do
  m <- read t
  case m of
    Nothing -> retry
    Just a  -> do set t Nothing; return a

-- | A version of 'takeSTxMVar' that does not 'retry'.  The 'tryTakeSTxMVar'
-- function returns 'Nothing' if the 'STxMVar' was empty, or @'Just' a@ if
-- the 'STxMVar' was full with contents @a@.  After 'tryTakeSTxMVar', the
-- 'STxMVar' is left empty.
tryTakeSTxMVar :: STxMVarK mod inc a => STxMVar mod inc a -> STxM inc (Maybe a)
tryTakeSTxMVar (STxMVar t) = do
  m <- read t
  case m of
    Nothing -> return Nothing
    Just a  -> do set t Nothing; return (Just a)

-- |Put a value into a 'STxMVar'.  If the 'STxMVar' is currently full,
-- 'putSTxMVar' will 'retry'.
putSTxMVar :: STxMVarK mod inc a => STxMVar mod inc a -> a -> STxM inc ()
putSTxMVar (STxMVar t) a = do
  m <- read t
  case m of
    Nothing -> do set t (Just a); return ()
    Just _  -> retry

-- | A version of 'putSTxMVar' that does not 'retry'.  The 'tryPutSTxMVar'
-- function attempts to put the value @a@ into the 'STxMVar', returning
-- 'True' if it was successful, or 'False' otherwise.
tryPutSTxMVar :: STxMVarK mod inc a => STxMVar mod inc a -> a -> STxM inc Bool
tryPutSTxMVar (STxMVar t) a = do
  m <- read t
  case m of
    Nothing -> do set t (Just a); return True
    Just _  -> return False

-- | This is a combination of 'takeSTxMVar' and 'putSTxMVar'; ie. it
-- takes the value from the 'STxMVar', puts it back, and also returns
-- it.
readSTxMVar :: STxMVarK mod inc a => STxMVar mod inc a -> STxM inc a
readSTxMVar (STxMVar t) = do
  m <- read t
  case m of
    Nothing -> retry
    Just a  -> return a

-- | A version of 'readSTxMVar' which does not retry. Instead it
-- returns @Nothing@ if no value is available.
tryReadSTxMVar :: STxMVarK mod inc a => STxMVar mod inc a -> STxM inc (Maybe a)
tryReadSTxMVar (STxMVar t) = read t

-- |Swap the contents of a 'STxMVar' for a new value.
swapSTxMVar :: STxMVarK mod inc a => STxMVar mod inc a -> a -> STxM inc a
swapSTxMVar (STxMVar t) new = do
  m <- read t
  case m of
    Nothing -> retry
    Just old -> do set t (Just new); return old

-- |Check whether a given 'STxMVar' is empty.
isEmptySTxMVar :: STxMVarK mod inc a => STxMVar mod inc a -> STxM inc Bool
isEmptySTxMVar (STxMVar t) = do
  m <- read t
  case m of
    Nothing -> return True
    Just _  -> return False

