-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.SHFSTM.Internal.Debug
-- Copyright   :  (c) D. Sabel, Goethe-University, Frankfurt a.M., Germany
-- License     :  BSD-style 
-- 
-- Maintainer  :  sabel <at> ki.cs.uni-frankfurt.de
-- Stability   :  experimental
-- Portability :  non-portable (needs GHC and extensions)
--
--  
-- This module implements the interface to the STM implementation.
-----------------------------------------------------------------------------

module Control.Concurrent.SHFSTM.Internal.Debug ( 
 sPutStrLn
 ) where

import Control.Exception
import Control.Concurrent
import System.IO.Unsafe
 
-- | 'sPutStrLn' can be used to print exclusively to stdout, if all print-operations
--   use this primitive. It is used for debugging purposes.

sPutStrLn :: String -> IO ()
sPutStrLn str =
 mask_ $ 
  do
   putMVar printv ()
   putStrLn str
   takeMVar printv
   
{-# NOINLINE printv #-}
printv :: MVar ()
printv = unsafePerformIO $ newEmptyMVar    
   
