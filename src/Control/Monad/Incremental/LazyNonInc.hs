module Control.Monad.Incremental.LazyNonInc (
	  module Control.Monad.Incremental
	, module Control.Monad.Incremental.Display
	, module Control.Monad.Incremental.Draw

	, module Control.Monad.Incremental.Internal.LazyNonInc.Algorithm
	, module Control.Monad.Incremental.Internal.LazyNonInc.Display
	, module Control.Monad.Incremental.Internal.LazyNonInc.Layers
	, module Control.Monad.Incremental.Internal.LazyNonInc.Types
	, module Control.Monad.Incremental.Internal.LazyNonInc.Memo
	) where

import Control.Monad.Incremental
import Control.Monad.Incremental.Display
import Control.Monad.Incremental.Draw

import Control.Monad.Incremental.Internal.LazyNonInc.Algorithm ()
import Control.Monad.Incremental.Internal.LazyNonInc.Display
import Control.Monad.Incremental.Internal.LazyNonInc.Layers (LazyNonInc,proxyLazyNonInc)
import Control.Monad.Incremental.Internal.LazyNonInc.Types (LazyNonIncU,LazyNonIncL,LazyNonIncM)
import Control.Monad.Incremental.Internal.LazyNonInc.Memo

