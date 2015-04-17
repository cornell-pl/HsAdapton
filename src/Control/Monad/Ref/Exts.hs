module Control.Monad.Ref.Exts (
	  module Control.Monad.Ref
	, mapRef, mapRefM_
	) where

import Control.Monad.Ref

--instance (MonadRef r m) => MonadRef r (ReaderT a m) where
--	readRef r = lift $ readRef r
--	newRef x = lift $ newRef x
--	writeRef r x = lift $ writeRef r x

{-# INLINE mapRef #-}
mapRef :: MonadRef r m => (a -> a) -> r a -> m ()
mapRef f r = do
	v <- readRef r
	let v' = f v
	v' `seq` writeRef r v'

{-# INLINE mapRefM_ #-}
mapRefM_ :: MonadRef r m => (a -> m a) -> r a -> m ()
mapRefM_ f r = do
	v <- readRef r
	v' <- f v
	v' `seq` writeRef r v'

copyRef r = readRef r >>= newRef