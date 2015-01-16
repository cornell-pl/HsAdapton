{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Control.Monad.Transactional.TxAdapton.Display where

import Control.Monad.Incremental.Display
import Control.Monad.Incremental
import Control.Monad.Transactional.TxAdapton.Types
import Control.Monad.Transactional.TxAdapton.Layers

import Control.Monad.Lazy
import Control.Monad.Trans
import Control.Monad
import Data.Typeable

-- * Display

instance (Typeable a,MonadLazy (Outside TxAdapton r m),Eq a,Display Outside TxAdapton r m a,Input TxM l TxAdapton r m) => Display Outside TxAdapton r m (TxM l TxAdapton r m a) where
	displaysPrec proxyL proxyInc proxyR proxyM m rest = getOutside m >>= \x -> lazily $ liftM (\x -> "<" ++ (show $ idTxNM $ metaTxM m) ++ " " ++ x) $ displaysPrec proxyL proxyInc proxyR proxyM x ('>':rest)
	{-# INLINE displaysPrec #-}

instance (Typeable a,MonadLazy (Inside TxAdapton r m),Eq a,Display Inside TxAdapton r m a,Input TxM Inside TxAdapton r m) => Display Inside TxAdapton r m (TxM Inside TxAdapton r m a) where
	displaysPrec proxyL proxyInc proxyR proxyM m rest = get m >>= \x -> lazily $ liftM (\x -> "<" ++ (show $ idTxNM $ metaTxM m) ++ " " ++ x) $ displaysPrec proxyL proxyInc proxyR proxyM x ('>':rest)
	{-# INLINE displaysPrec #-}
	
instance (Typeable a,MonadLazy (Outside TxAdapton r m),Eq a,Display Outside TxAdapton r m a,Output TxU l TxAdapton r m) => Display Outside TxAdapton r m (TxU l TxAdapton r m a) where
	displaysPrec proxyL proxyInc proxyR proxyM m rest = forceOutside m >>= \x -> lazily $ liftM (\x -> "<" ++ (show $ idTxNM $ metaTxU m) ++ " " ++ x) $ displaysPrec proxyL proxyInc proxyR proxyM x ('>':rest)
	{-# INLINE displaysPrec #-}

instance (Typeable a,MonadLazy (Inside TxAdapton r m),Eq a,Display Inside TxAdapton r m a,Output TxU Inside TxAdapton r m) => Display Inside TxAdapton r m (TxU Inside TxAdapton r m a) where
	displaysPrec proxyL proxyInc proxyR proxyM m rest = force m >>= \x -> lazily $ liftM (\x -> "<" ++ (show $ idTxNM $ metaTxU m) ++ " " ++ x) $ displaysPrec proxyL proxyInc proxyR proxyM x ('>':rest)
	{-# INLINE displaysPrec #-}

--instance (MonadLazy (Outside inc r m),Eq a,Display Outside inc r m a,Input L l inc r m) => Display Outside inc r m (L l inc r m a) where
--	displaysPrec m rest = getOutside m >>= \x -> lazily $ displaysPrec x rest
--	{-# INLINE displaysPrec #-}
--
--instance (MonadLazy (Inside inc r m),Eq a,Display Inside inc r m a,Input L Inside inc r m) => Display Inside inc r m (L Inside inc r m a) where
--	displaysPrec m rest = get m >>= \x -> lazily $ displaysPrec x rest
--	{-# INLINE displaysPrec #-}

-- * NFDataInc

instance (Typeable a,Eq a,NFDataInc Outside TxAdapton r m a,Input TxM l TxAdapton r m) => NFDataInc Outside TxAdapton r m (TxM l TxAdapton r m a) where
	rnfInc proxyL proxyInc proxyR proxyM m = getOutside m >>= rnfInc proxyL proxyInc proxyR proxyM
	{-# INLINE rnfInc #-}

instance (Typeable a,Eq a,NFDataInc Inside TxAdapton r m a,Input TxM Inside TxAdapton r m) => NFDataInc Inside TxAdapton r m (TxM Inside TxAdapton r m a) where
	rnfInc proxyL proxyInc proxyR proxyM m = get m >>= rnfInc proxyL proxyInc proxyR proxyM
	{-# INLINE rnfInc #-}
	
instance (Typeable a,Eq a,NFDataInc Outside TxAdapton r m a,Output TxU l TxAdapton r m) => NFDataInc Outside TxAdapton r m (TxU l TxAdapton r m a) where
	rnfInc proxyL proxyInc proxyR proxyM m = forceOutside m >>= rnfInc proxyL proxyInc proxyR proxyM
	{-# INLINE rnfInc #-}

instance (Typeable a,Eq a,NFDataInc Inside TxAdapton r m a,Output TxU Inside TxAdapton r m) => NFDataInc Inside TxAdapton r m (TxU Inside TxAdapton r m a) where
	rnfInc proxyL proxyInc proxyR proxyM m = force m >>= rnfInc proxyL proxyInc proxyR proxyM
	{-# INLINE rnfInc #-}

--instance (Eq a,NFDataInc Outside inc r m a,Input L l inc r m) => NFDataInc Outside inc r m (L l inc r m a) where
--	rnfInc m = getOutside m >>= rnfInc
--	{-# INLINE rnfInc #-}
--
--instance (Eq a,NFDataInc Inside inc r m a,Input L Inside inc r m) => NFDataInc Inside inc r m (L Inside inc r m a) where
--	rnfInc m = get m >>= rnfInc
--	{-# INLINE rnfInc #-}


-- * Show instances

instance Show (TxU l inc r m a) where
	show t = "U" ++ show (idTxNM $ metaTxU t)
instance Show (TxM l inc r m a) where
	show t = "M" ++ show (idTxNM $ metaTxM t)
--instance Show (L l inc r m a) where
--	show t = "L" ++ show (idNM $ metaL t)