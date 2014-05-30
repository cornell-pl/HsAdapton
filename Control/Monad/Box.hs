{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Control.Monad.Box where

import Data.Functor.Identity
import Control.Monad.ST.Lazy
import Data.STRef.Lazy
import Data.IORef
import Control.Monad.Ref
import Control.Monad

class Monad m => Box f m where
	box :: c -> m (f c)
	unbox :: f c -> m c
	inbox :: f c -> (c -> m c) -> m (f c)

instance Monad m => Box Identity m where
	box = return . Identity
	unbox = return . runIdentity
	inbox (Identity x) f = liftM Identity $ f x

instance Box IORef IO where
	box = newRef
	unbox = readRef
	inbox r f = mapRefM f r

instance Box (STRef s) (ST s) where
	box = newRef
	unbox = readRef
	inbox r f = mapRefM f r
