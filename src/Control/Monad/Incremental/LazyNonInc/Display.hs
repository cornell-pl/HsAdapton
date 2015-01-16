{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Control.Monad.Incremental.LazyNonInc.Display where

import Control.Monad.Incremental.Display
import Control.Monad.Incremental
import Control.Monad.Incremental.LazyNonInc.Types
import Control.Monad.Incremental.LazyNonInc.Layers
import Data.Typeable

--import Blaze.ByteString.Builder
--import qualified Data.ByteString.Lazy.Char8 as B
--import Blaze.ByteString.Builder.Internal

import Control.Monad.Lazy
import Control.Monad.Trans

-- * Display

instance (Typeable a,MonadLazy (Outside inc r m),Eq a,Display Outside inc r m a,Output LazyNonIncU l inc r m) => Display Outside inc r m (LazyNonIncU l inc r m a) where
	displaysPrec proxyL proxyInc proxyR proxyM m rest = forceOutside m >>= \x -> lazily $ displaysPrec proxyL proxyInc proxyR proxyM x rest
	{-# INLINE displaysPrec #-}

instance (Typeable a,MonadLazy (Inside inc r m),Eq a,Display Inside inc r m a,Output LazyNonIncU Inside inc r m) => Display Inside inc r m (LazyNonIncU Inside inc r m a) where
	displaysPrec proxyL proxyInc proxyR proxyM m rest = force m >>= \x -> lazily $ displaysPrec proxyL proxyInc proxyR proxyM x rest
	{-# INLINE displaysPrec #-}

instance (Typeable a,MonadLazy (Outside inc r m),Eq a,Display Outside inc r m a,Input LazyNonIncL l inc r m) => Display Outside inc r m (LazyNonIncL l inc r m a) where
	displaysPrec proxyL proxyInc proxyR proxyM m rest = getOutside m >>= \x -> lazily $ displaysPrec proxyL proxyInc proxyR proxyM x rest
	{-# INLINE displaysPrec #-}

instance (Typeable a,MonadLazy (Inside inc r m),Eq a,Display Inside inc r m a,Input LazyNonIncL Inside inc r m) => Display Inside inc r m (LazyNonIncL Inside inc r m a) where
	displaysPrec proxyL proxyInc proxyR proxyM m rest = get m >>= \x -> lazily $ displaysPrec proxyL proxyInc proxyR proxyM x rest
	{-# INLINE displaysPrec #-}

instance (Typeable a,MonadLazy (Outside inc r m),Eq a,Display Outside inc r m a,Input LazyNonIncM l inc r m) => Display Outside inc r m (LazyNonIncM l inc r m a) where
	displaysPrec proxyL proxyInc proxyR proxyM m rest = getOutside m >>= \x -> lazily $ displaysPrec proxyL proxyInc proxyR proxyM x rest
	{-# INLINE displaysPrec #-}

instance (Typeable a,MonadLazy (Inside inc r m),Eq a,Display Inside inc r m a,Input LazyNonIncM Inside inc r m) => Display Inside inc r m (LazyNonIncM Inside inc r m a) where
	displaysPrec proxyL proxyInc proxyR proxyM m rest = get m >>= \x -> lazily $ displaysPrec proxyL proxyInc proxyR proxyM x rest
	{-# INLINE displaysPrec #-}

-- * Serialize

--instance (Eq a,Serialize Outside inc r m a,Output LazyNonIncU l inc r m,InIO (Outside inc r m)) => Serialize Outside inc r m (LazyNonIncU l inc r m a) where
--	serialize proxy m = fromPut $ putLiftIO (inIO $ forceOutside m) >>= putBuilder . serialize proxy
--	{-# INLINE serialize #-}
--
--instance (Eq a,Serialize Inside inc r m a,Output LazyNonIncU Inside inc r m,InIO (Inside inc r m)) => Serialize Inside inc r m (LazyNonIncU Inside inc r m a) where
--	serialize proxy m = fromPut $ putLiftIO (inIO $ force m) >>= putBuilder . serialize proxy
--	{-# INLINE serialize #-}
--
--instance (Eq a,Serialize Outside inc r m a,Input LazyNonIncL l inc r m,InIO (Outside inc r m)) => Serialize Outside inc r m (LazyNonIncL l inc r m a) where
--	serialize proxy m = fromPut $ putLiftIO (inIO $ getOutside m) >>= putBuilder . serialize proxy
--	{-# INLINE serialize #-}
--
--instance (Eq a,Serialize Inside inc r m a,Input LazyNonIncL Inside inc r m,InIO (Inside inc r m)) => Serialize Inside inc r m (LazyNonIncL Inside inc r m a) where
--	serialize proxy m = fromPut $ putLiftIO (inIO $ get m) >>= putBuilder . serialize proxy
--	{-# INLINE serialize #-}
--		
--instance (Eq a,Serialize Outside inc r m a,Input LazyNonIncM l inc r m,InIO (Outside inc r m)) => Serialize Outside inc r m (LazyNonIncM l inc r m a) where
--	serialize proxy m = fromPut $ putLiftIO (inIO $ getOutside m) >>= putBuilder . serialize proxy
--	{-# INLINE serialize #-}
--
--instance (Eq a,Serialize Inside inc r m a,Input LazyNonIncM Inside inc r m,InIO (Inside inc r m)) => Serialize Inside inc r m (LazyNonIncM Inside inc r m a) where
--	serialize proxy m = fromPut $ putLiftIO (inIO $ get m) >>= putBuilder . serialize proxy
--	{-# INLINE serialize #-}


-- * NFDataInc

instance (Typeable a,Eq a,NFDataInc Outside inc r m a,Output LazyNonIncU l inc r m) => NFDataInc Outside inc r m (LazyNonIncU l inc r m a) where
	rnfInc proxyL proxyInc proxyR proxyM m = forceOutside m >>= rnfInc proxyL proxyInc proxyR proxyM
	{-# INLINE rnfInc #-}

instance (Typeable a,Eq a,NFDataInc Inside inc r m a,Output LazyNonIncU Inside inc r m) => NFDataInc Inside inc r m (LazyNonIncU Inside inc r m a) where
	rnfInc proxyL proxyInc proxyR proxyM m = force m >>= rnfInc proxyL proxyInc proxyR proxyM
	{-# INLINE rnfInc #-}

instance (Typeable a,Eq a,NFDataInc Outside inc r m a,Input LazyNonIncL l inc r m) => NFDataInc Outside inc r m (LazyNonIncL l inc r m a) where
	rnfInc proxyL proxyInc proxyR proxyM m = getOutside m >>= rnfInc proxyL proxyInc proxyR proxyM
	{-# INLINE rnfInc #-}

instance (Typeable a,Eq a,NFDataInc Inside inc r m a,Input LazyNonIncL Inside inc r m) => NFDataInc Inside inc r m (LazyNonIncL Inside inc r m a) where
	rnfInc proxyL proxyInc proxyR proxyM m = get m >>= rnfInc proxyL proxyInc proxyR proxyM
	{-# INLINE rnfInc #-}

instance (Typeable a,Eq a,NFDataInc Outside inc r m a,Input LazyNonIncM l inc r m) => NFDataInc Outside inc r m (LazyNonIncM l inc r m a) where
	rnfInc proxyL proxyInc proxyR proxyM m = getOutside m >>= rnfInc proxyL proxyInc proxyR proxyM
	{-# INLINE rnfInc #-}

instance (Typeable a,Eq a,NFDataInc Inside inc r m a,Input LazyNonIncM Inside inc r m) => NFDataInc Inside inc r m (LazyNonIncM Inside inc r m a) where
	rnfInc proxyL proxyInc proxyR proxyM m = get m >>= rnfInc proxyL proxyInc proxyR proxyM
	{-# INLINE rnfInc #-}


-- * Show instances

instance Show (LazyNonIncU l inc r m a) where
	show t = "LazyNonIncU" 
instance Show (LazyNonIncM l inc r m a) where
	show t = "LazyNonIncM" 
instance Show (LazyNonIncL l inc r m a) where
	show t = "LazyNonIncL" 
