{-# LANGUAGE UndecidableInstances, ConstraintKinds, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Control.Monad.Incremental.Adapton.Display where

import Control.Monad.Incremental.Display
import Control.Monad.Incremental
import Control.Monad.Incremental.Adapton.Types
import Control.Monad.Incremental.Adapton.Layers

--import Blaze.ByteString.Builder
--import qualified Data.ByteString.Lazy.Char8 as B
--import Blaze.ByteString.Builder.Internal
--import Blaze.ByteString.Builder.Internal.Types

import Control.Monad.Lazy
import Control.Monad.Trans
import Data.Typeable

-- * Display

instance (IncK inc a,MonadLazy (Outside inc r m),Display Outside inc r m a,Input M l inc r m) => Display Outside inc r m (M l inc r m a) where
	displaysPrec proxyL proxyInc proxyR proxyM m rest = getOutside m >>= \x -> lazily $ displaysPrec proxyL proxyInc proxyR proxyM x rest
	{-# INLINE displaysPrec #-}

instance (IncK inc a,MonadLazy (Inside inc r m),Display Inside inc r m a,Input M Inside inc r m) => Display Inside inc r m (M Inside inc r m a) where
	displaysPrec proxyL proxyInc proxyR proxyM m rest = get m >>= \x -> lazily $ displaysPrec proxyL proxyInc proxyR proxyM x rest
	{-# INLINE displaysPrec #-}
	
instance (IncK inc a,MonadLazy (Outside inc r m),Display Outside inc r m a,Output U l inc r m) => Display Outside inc r m (U l inc r m a) where
	displaysPrec proxyL proxyInc proxyR proxyM m rest = forceOutside m >>= \x -> lazily $ displaysPrec proxyL proxyInc proxyR proxyM x rest
	{-# INLINE displaysPrec #-}

instance (IncK inc a,MonadLazy (Inside inc r m),Display Inside inc r m a,Output U Inside inc r m) => Display Inside inc r m (U Inside inc r m a) where
	displaysPrec proxyL proxyInc proxyR proxyM m rest = force m >>= \x -> lazily $ displaysPrec proxyL proxyInc proxyR proxyM x rest
	{-# INLINE displaysPrec #-}

instance (IncK inc a,MonadLazy (Outside inc r m),Display Outside inc r m a,Input L l inc r m) => Display Outside inc r m (L l inc r m a) where
	displaysPrec proxyL proxyInc proxyR proxyM m rest = getOutside m >>= \x -> lazily $ displaysPrec proxyL proxyInc proxyR proxyM x rest
	{-# INLINE displaysPrec #-}

instance (IncK inc a,MonadLazy (Inside inc r m),Display Inside inc r m a,Input L Inside inc r m) => Display Inside inc r m (L Inside inc r m a) where
	displaysPrec proxyL proxyInc proxyR proxyM m rest = get m >>= \x -> lazily $ displaysPrec proxyL proxyInc proxyR proxyM x rest
	{-# INLINE displaysPrec #-}

-- * Serialize

--instance (Eq a,Input M l inc r m,InIO (Outside inc r m),Serialize Outside inc r m a) => Serialize Outside inc r m (M l inc r m a) where
--	serialize proxy m = fromPut $ putLiftIO (inIO $ getOutside m) >>= putBuilder . serialize proxy
--	{-# INLINE serialize #-}
--
--instance (Eq a,Input M Inside inc r m,InIO (Inside inc r m),Serialize Inside inc r m a) => Serialize Inside inc r m (M Inside inc r m a) where
--	serialize proxy m = fromPut $ putLiftIO (inIO $ get m) >>= putBuilder . serialize proxy
--	{-# INLINE serialize #-}
--
--instance (Eq a,Output U l inc r m,InIO (Outside inc r m),Serialize Outside inc r m a) => Serialize Outside inc r m (U l inc r m a) where
--	serialize proxy m = fromPut $ putLiftIO (inIO $ forceOutside m) >>= putBuilder . serialize proxy
--	{-# INLINE serialize #-}
--
--instance (Eq a,Output U Inside inc r m,InIO (Inside inc r m),Serialize Inside inc r m a) => Serialize Inside inc r m (U Inside inc r m a) where
--	serialize proxy m = fromPut $ putLiftIO (inIO $ force m) >>= putBuilder . serialize proxy
--	{-# INLINE serialize #-}
--
--instance (Eq a,Input L l inc r m,InIO (Outside inc r m),Serialize Outside inc r m a) => Serialize Outside inc r m (L l inc r m a) where
--	serialize proxy m = fromPut $ putLiftIO (inIO $ getOutside m) >>= putBuilder . serialize proxy
--	{-# INLINE serialize #-}
--
--instance (Eq a,Input L Inside inc r m,InIO (Inside inc r m),Serialize Inside inc r m a) => Serialize Inside inc r m (L Inside inc r m a) where
--	serialize proxy m = fromPut $ putLiftIO (inIO $ get m) >>= putBuilder . serialize proxy
--	{-# INLINE serialize #-}

-- * NFDataInc

instance (IncK inc a,NFDataInc Outside inc r m a,Input M l inc r m) => NFDataInc Outside inc r m (M l inc r m a) where
	rnfInc proxyL proxyInc proxyR proxyM m = getOutside m >>= rnfInc proxyL proxyInc proxyR proxyM
	{-# INLINE rnfInc #-}

instance (IncK inc a,NFDataInc Inside inc r m a,Input M Inside inc r m) => NFDataInc Inside inc r m (M Inside inc r m a) where
	rnfInc proxyL proxyInc proxyR proxyM m = get m >>= rnfInc proxyL proxyInc proxyR proxyM
	{-# INLINE rnfInc #-}
	
instance (IncK inc a,NFDataInc Outside inc r m a,Output U l inc r m) => NFDataInc Outside inc r m (U l inc r m a) where
	rnfInc proxyL proxyInc proxyR proxyM m = forceOutside m >>= rnfInc proxyL proxyInc proxyR proxyM
	{-# INLINE rnfInc #-}

instance (IncK inc a,NFDataInc Inside inc r m a,Output U Inside inc r m) => NFDataInc Inside inc r m (U Inside inc r m a) where
	rnfInc proxyL proxyInc proxyR proxyM m = force m >>= rnfInc proxyL proxyInc proxyR proxyM
	{-# INLINE rnfInc #-}

instance (IncK inc a,NFDataInc Outside inc r m a,Input L l inc r m) => NFDataInc Outside inc r m (L l inc r m a) where
	rnfInc proxyL proxyInc proxyR proxyM m = getOutside m >>= rnfInc proxyL proxyInc proxyR proxyM
	{-# INLINE rnfInc #-}

instance (IncK inc a,NFDataInc Inside inc r m a,Input L Inside inc r m) => NFDataInc Inside inc r m (L Inside inc r m a) where
	rnfInc proxyL proxyInc proxyR proxyM m = get m >>= rnfInc proxyL proxyInc proxyR proxyM
	{-# INLINE rnfInc #-}


-- * Show instances

instance Show (U l inc r m a) where
	show t = "U" ++ show (idNM $ metaU t)
instance Show (M l inc r m a) where
	show t = "M" ++ show (idNM $ metaM t)
instance Show (L l inc r m a) where
	show t = "L" ++ show (idNM $ metaL t)