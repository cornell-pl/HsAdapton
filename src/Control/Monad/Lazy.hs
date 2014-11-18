module Control.Monad.Lazy where

-- | See https://www.fpcomplete.com/user/snoyberg/general-haskell/advanced/evaluation-order-and-state-tokens for a very nice introduction to unsafe GHC trickery

import Control.Monad
import System.IO.Unsafe
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Monad.Reader as Reader
import Control.Monad.State.Lazy as State
import Control.Monad.State.Strict as StateS

-- | Don't specify non-default instances of this class unless you really really know what you are doing!
class Monad m => MonadLazy m where
	-- performs an action only on demand but allows to define exact *internal* execution order for parts of your datastructure (a.k.a @unsafeInterleaveIO@)
	lazily :: m a -> m a
	lazily = id
	-- performs an action as if it was a pure computation (a.k.a. @unsafePerformIO@)
	anytime :: m a -> m a
	anytime = id

instance MonadLazy IO where
--	lazily = unsafeInterleaveIO
--	anytime = return . unsafePerformIO

instance MonadLazy m => MonadLazy (Reader.ReaderT r m) where
	lazily = Reader.mapReaderT lazily
	anytime = Reader.mapReaderT anytime
	
instance MonadLazy m => MonadLazy (State.StateT r m) where
	lazily = State.mapStateT lazily
	anytime = State.mapStateT anytime

instance MonadLazy m => MonadLazy (StateS.StateT r m) where
	lazily = StateS.mapStateT lazily
	anytime = StateS.mapStateT anytime