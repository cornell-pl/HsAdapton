-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.SHFSTM
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


{-# LANGUAGE CPP, DeriveDataTypeable, ExistentialQuantification #-}

module Control.Concurrent.SHFSTM (
 -- * Types
 STM(),
 TVar(),
 -- * Operations in the 'STM'-monad
 newTVar,
 readTVar,
 writeTVar,
 retry,
 orElse,
 -- * Execution of 'STM'-transactions
 atomically, unsafeIOToSTM,
 newTVarIO
 ) where

import Control.Monad
import Data.Maybe
import Control.Exception
import Prelude hiding(catch)
import System.IO.Unsafe
import Data.IORef
import qualified Data.List
import Data.Map as Map
import Data.Set as Set
import Control.Concurrent
import Control.Exception
import Data.Maybe
import Data.Typeable
import Control.Concurrent.SHFSTM.Internal
import Control.Concurrent.SHFSTM.Internal.TVar
#ifdef DEBUG    
import Control.Concurrent.SHFSTM.Internal.Debug(sPutStrLn)
#endif
import Control.Applicative
-- | The STM-monad

unsafeIOToSTM :: IO a -> STM a
unsafeIOToSTM io = STMIO io return


-- The  data type STM, the operations store the continuation    
data STM a = Return a 
           | forall b. NewTVar b (TVar b -> STM a)
           | forall b. ReadTVar (TVar b) (b -> STM a)
           | forall b. WriteTVar (TVar b) b (STM a)
           | Retry  
           | forall b. OrElse (STM b) (STM b) (b -> STM a) 
           | forall b . STMIO (IO b) (b -> STM a)



instance  Functor STM where
   fmap f x = x >>= (return . f)
   
instance Applicative STM where
  pure = Return
  mf <*> m = mf >>= \f -> liftM f m

-- | The monad instance for 'STM'
instance Monad STM where
  return = Return
  m >>= f = 
    case m of
      Return x -> f x
      Retry  -> Retry
      NewTVar x cont -> NewTVar x (\i -> (cont i >>= f)) 
      ReadTVar x cont -> ReadTVar x (\i -> (cont i >>= f))
      WriteTVar v x cont -> WriteTVar v x (cont >>= f)
      OrElse a1 a2 cont -> OrElse a1 a2 (\i -> cont i >>= f)
      STMIO a1 cont -> STMIO a1 (\i -> cont i >>= f)
      

-- | 'newTVar' creates a new 'TVar' in the 'STM' monad

newTVar :: a             -- ^ the content of the TVar
        -> STM (TVar a)  -- ^ the result
newTVar a = NewTVar a return

-- | 'readTVar' reads the content of a 'TVar' in the 'STM' monad

readTVar :: TVar a       -- ^ the to-be-read 'TVar'
            -> STM a     -- ^ the result is an 'STM' action that returns the read content of the 'TVar'
readTVar a = ReadTVar a return


-- | 'writeTVar' writes new content into a 'TVar' in the 'STM' monad
writeTVar :: TVar a  -- ^ the to-be-written 'TVar'
          -> a       -- ^ the new content
          -> STM ()
writeTVar v x = WriteTVar v x (return ())


-- | 'orElse' composes two transactions: 
-- 
-- In 'orElse' @e1@ @e2@ first the transaction @e1@ is executed.
-- If it is successful (i.e. commits), then 'orElse' @e1@ @e2@ is successful.
-- If a 'retry' pops up, then 'orElse' @e1@ @e2@ proceeds with @e2@ ignoring
-- then effects made by execution of @e1@.

orElse :: STM a    -- ^ the first transaction
          -> STM a -- ^ the second transaction
          -> STM a -- ^ the composed transaction
          
orElse a1 a2 = OrElse a1 a2 return

-- | 'retry' aborts and restarts a transaction
--
--   Note that 'retry' has a /different/ semantics if it is used inside an argument of 'orElse':
--
--   If 'retry' pops up in the left @e1@ argument of 'orElse' @e1@ @e2@, then the transaction 
--   corresponding to @ e1 @ is roll-back and transaction @e2@ is executed.
--
--   If 'retry' pops up in the right argument @e2@, then it behaves like a ususal 'retry'

retry :: STM a
retry = Retry
  

-- | 'newTVarIO' creates new 'TVar' in the 'IO'-monad,
--
--   @ newTVarIO a @ is equivalent to 'atomically' @(@'newTVar'@ a)@
newTVarIO :: a            -- ^ the content of the TVar
          -> IO (TVar a)  -- ^ the result is a TVar in the 'IO'-monad
newTVarIO x = atomically (newTVar x)

   
-- | 'atomically' executes an 'STM' transaction atomically in the 'IO'-monad
atomically :: STM a -> IO a
atomically act =
  do
    mid <- myThreadId
#ifdef DEBUG    
    sPutStrLn (show mid ++ " starts transaction")
#endif
    tlog <- emptyTLOG
    catch (performSTM tlog act) 
          (\e -> case e of
                   RetryException   ->  do
                                          uninterruptibleMask_ ( do 
                                               
#ifdef DEBUG    
                                                   sPutStrLn ((show mid) ++  " got retry")
#endif
                                                   
                                                   globalRetry tlog)
                                          atomically act
                   other -> putStrLn ("other exception" ++ show mid) >> error "error")


                   
performSTM tlog act =
  case act of 
    Return a -> do
               commit tlog
               return a
    Retry  -> do
#ifdef DEBUG    
               mid <- myThreadId
               sPutStrLn ((show mid) ++  "USERDEFINED RETRY")
#endif
               waitForExternalRetry -- forever until a retry-exception is received
    NewTVar x cont -> do
                       tv <- newTVarWithLog tlog x
                       performSTM tlog (cont tv) 
    ReadTVar x cont -> do 
                        res <- readTVarWithLog tlog x
                        performSTM tlog (cont res) 
    WriteTVar v x cont -> do
                           writeTVarWithLog tlog v x
                           performSTM tlog cont 
    OrElse act1 act2 cont -> do           
                              orElseWithLog tlog -- adjust for left orElse
                              resl <- performOrElseLeft tlog act1
                              case resl of
                                Just a -> performSTM tlog (cont a) 
                                Nothing -> do
                                            orRetryWithLog tlog
                                            performSTM tlog (act2 >>= cont) 
    STMIO act1 cont -> act1 >>= performSTM tlog . cont
                                
performOrElseLeft :: (TLOG) -> STM a -> IO (Maybe a)
performOrElseLeft tlog  act = 
   case act of 
    Return a -> do
               return $ Just a
    Retry  -> do
               return Nothing
    NewTVar x cont -> do
                       tv <- newTVarWithLog tlog x
                       performOrElseLeft tlog (cont tv)
    ReadTVar x cont -> do 
                        res <- readTVarWithLog tlog x
                        performOrElseLeft tlog (cont res)
    WriteTVar v x cont -> do
                           writeTVarWithLog tlog v x
                           performOrElseLeft tlog cont 
    OrElse act1 act2 cont -> do           
                              orElseWithLog tlog -- adjust for left orElse
                              resl <- performOrElseLeft tlog act1
                              case resl of
                                Just a -> performOrElseLeft tlog (cont a) 
                                Nothing -> do
                                            orRetryWithLog tlog
                                            performOrElseLeft tlog (act2 >>= cont) 
    STMIO act1 cont -> act1 >>= liftM Just . performSTM tlog . cont

                                            
waitForExternalRetry = 
 do
  x <- newEmptyMVar
  wait x
   where wait x  =  
            catch (takeMVar x >> return undefined) 
                  (\e -> case e of BlockedIndefinitelyOnMVar -> putStrLn "BLOCKED IN RETRYWAT" >> wait x
                                   _ -> throw e)
    