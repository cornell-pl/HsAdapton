{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

module Control.Concurrent.Transactional.Internal.TxAdapton.Algorithm where

import Control.Monad.Incremental
import Control.Concurrent.Transactional.Internal.TxAdapton.Types

atomicallyTx :: (TxLayer Inside c,TxLayer Outside c,TxLayer l c) => IncParams (TxAdapton c) -> String -> l (TxAdapton c) a -> IO a