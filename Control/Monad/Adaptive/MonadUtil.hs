module Control.Monad.Adaptive.MonadUtil where

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b a c = do b' <- b; if b' then a else c

whenM :: Monad m => m Bool -> m () -> m ()
whenM b a = ifM b a (return ())

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b a = ifM b (return ()) a
