{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses, FlexibleInstances, RankNTypes, MagicHash, UnboxedTuples #-}

-- A class for precise finalization of weak pointers.

-- Important note from the documentation of System.Mem.Weak:
-- Finalizers can be used reliably for types that are created explicitly and have identity, such as IORef and MVar. However, to place a finalizer on one of these types, you should use the specific operation provided for that type, e.g. mkWeakIORef and addMVarFinalizer respectively (the non-uniformity is accidental). These operations attach the finalizer to the primitive object inside the box (e.g. MutVar# in the case of IORef), because attaching the finalizer to the box itself fails when the outer box is optimised away by the compiler.

module System.Mem.WeakRef where

import GHC.IORef
import GHC.STRef
import GHC.Weak
import GHC.Base
import Control.Monad.Ref
import System.Mem.Weak as Weak
import Control.Monad.Reader

mkWeakWithIORefKey :: IORef a -> b -> IO () -> IO (Weak b)
mkWeakWithIORefKey k@(IORef (STRef r#)) v f = IO $ \s ->
  case mkWeak# r# v f s of (# s1, w #) -> (# s1, Weak w #)

-- | class to create weak pointers with references (that typically (IORef,STRef) have unique addresses) as keys
class WeakRef r where
	mkWeakWithRefKey :: r a -> b -> Maybe (IO ()) -> IO (Weak b)

instance WeakRef IORef where
	mkWeakWithRefKey = \r v mb -> mkWeakWithIORefKey r v (maybe (return ()) id mb)
	{-# INLINE mkWeakWithRefKey #-}
	
{-# INLINE mkWeakRef #-}
mkWeakRef :: WeakRef r => r a -> Maybe (IO ()) -> IO (Weak (r a))
mkWeakRef = \r f -> mkWeakWithRefKey r r f

-- | Makes a dummy @Weak@ pointer that is already dead
mkDeadWeak v f = do
	w <- Weak.mkWeak () v f
	Weak.finalize w
	return w
	
newtype MkWeak = MkWeak { unMkWeak :: forall v . v -> Maybe (IO ()) -> IO (Weak v) }

-- | Creates a weak reference that is alive as long as two keys are
andMkWeak :: MkWeak -> MkWeak -> MkWeak
andMkWeak (MkWeak mkWeak1) (MkWeak mkWeak2) = MkWeak $ \v f -> do
	w1 <- mkWeak1 v f
	mkWeak2 () (Just $ finalize w1) -- the second reference does not add a dependency from the key to the value but only a finalizer; when the second key dies, finalize the first weak reference
	return w1

-- | Creates a weak reference that is alive as long as at least one of the keys is 
orMkWeak :: MkWeak -> MkWeak -> MkWeak
orMkWeak (MkWeak mkWeak1) (MkWeak mkWeak2) = MkWeak $ \v f -> do
	w1 <- mkWeak1 v f
	mkWeak2 v Nothing -- the first reference cannot be dead unless the second one is, due to the value dependencies
	return w1

instance (MonadRef r m) => MonadRef r (ReaderT a m) where
	readRef r = lift $ readRef r
	newRef x = lift $ newRef x
	writeRef r x = lift $ writeRef r x

	
