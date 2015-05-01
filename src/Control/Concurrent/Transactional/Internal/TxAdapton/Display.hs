{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, ConstraintKinds, UndecidableInstances #-}

module Control.Concurrent.Transactional.Internal.TxAdapton.Display where

import Control.Monad.Incremental.Display
import Control.Monad.Incremental
import Control.Concurrent.Transactional.Internal.TxAdapton.Types
import Control.Concurrent.Transactional.Internal.TxAdapton.Layers


import Control.Monad.Trans
import Control.Monad
import Data.Typeable

-- * Display

instance (IncK (TxAdapton c) a,Display Outside (TxAdapton c) a,Input (TxM i c) l (TxAdapton c)) => Display Outside (TxAdapton c) (TxM i c l (TxAdapton c) a) where
	displaysPrec proxyL proxyInc m rest = getOutside m >>= \x -> liftM (\x -> "<" ++ (show $ idTxNM $ metaTxM m) ++ " " ++ x) $ displaysPrec proxyL proxyInc x ('>':rest)
	{-# INLINE displaysPrec #-}

instance (IncK (TxAdapton c) a,Display Inside (TxAdapton c) a,Input (TxM i c) Inside (TxAdapton c)) => Display Inside (TxAdapton c) (TxM i c Inside (TxAdapton c) a) where
	displaysPrec proxyL proxyInc m rest = get m >>= \x -> liftM (\x -> "<" ++ (show $ idTxNM $ metaTxM m) ++ " " ++ x) $ displaysPrec proxyL proxyInc x ('>':rest)
	{-# INLINE displaysPrec #-}
	
instance (IncK (TxAdapton c) a,Display Outside (TxAdapton c) a,Output (TxU c) l (TxAdapton c)) => Display Outside (TxAdapton c) (TxU c l (TxAdapton c) a) where
	displaysPrec proxyL proxyInc m rest = forceOutside m >>= \x -> liftM (\x -> "<" ++ (show $ idTxNM $ metaTxU m) ++ " " ++ x) $ displaysPrec proxyL proxyInc x ('>':rest)
	{-# INLINE displaysPrec #-}

instance (IncK (TxAdapton c) a,Display Inside (TxAdapton c) a,Output (TxU c) Inside (TxAdapton c)) => Display Inside (TxAdapton c) (TxU c Inside (TxAdapton c) a) where
	displaysPrec proxyL proxyInc m rest = force m >>= \x -> liftM (\x -> "<" ++ (show $ idTxNM $ metaTxU m) ++ " " ++ x) $ displaysPrec proxyL proxyInc x ('>':rest)
	{-# INLINE displaysPrec #-}

--instance (MonadLazy (Outside inc),Eq a,Display Outside inc a,Input L l inc) => Display Outside inc (L l inc a) where
--	displaysPrec m rest = getOutside m >>= \x -> lazily $ displaysPrec x rest
--	{-# INLINE displaysPrec #-}
--
--instance (MonadLazy (Inside inc),Eq a,Display Inside inc a,Input L Inside inc) => Display Inside inc (L Inside inc a) where
--	displaysPrec m rest = get m >>= \x -> lazily $ displaysPrec x rest
--	{-# INLINE displaysPrec #-}

-- * NFDataInc

instance (IncK (TxAdapton c) a,NFDataInc Outside (TxAdapton c) a,Input (TxM i c) l (TxAdapton c)) => NFDataInc Outside (TxAdapton c) (TxM i c l (TxAdapton c) a) where
	nfDataInc proxyL proxyInc m = getOutside m >>= nfDataInc proxyL proxyInc
	{-# INLINE nfDataInc #-}

instance (IncK (TxAdapton c) a,NFDataInc Inside (TxAdapton c) a,Input (TxM i c) Inside (TxAdapton c)) => NFDataInc Inside (TxAdapton c) (TxM i c Inside (TxAdapton c) a) where
	nfDataInc proxyL proxyInc m = get m >>= nfDataInc proxyL proxyInc
	{-# INLINE nfDataInc #-}
	
instance (IncK (TxAdapton c) a,NFDataInc Outside (TxAdapton c) a,Output (TxU c) l (TxAdapton c)) => NFDataInc Outside (TxAdapton c) (TxU c l (TxAdapton c) a) where
	nfDataInc proxyL proxyInc m = forceOutside m >>= nfDataInc proxyL proxyInc
	{-# INLINE nfDataInc #-}

instance (IncK (TxAdapton c) a,NFDataInc Inside (TxAdapton c) a,Output (TxU c) Inside (TxAdapton c)) => NFDataInc Inside (TxAdapton c) (TxU c Inside (TxAdapton c) a) where
	nfDataInc proxyL proxyInc m = force m >>= nfDataInc proxyL proxyInc
	{-# INLINE nfDataInc #-}

--instance (Eq a,NFDataInc Outside inc a,Input L l inc) => NFDataInc Outside inc (L l inc a) where
--	nfDataInc m = getOutside m >>= nfDataInc
--	{-# INLINE nfDataInc #-}
--
--instance (Eq a,NFDataInc Inside inc a,Input L Inside inc) => NFDataInc Inside inc (L Inside inc a) where
--	nfDataInc m = get m >>= nfDataInc
--	{-# INLINE nfDataInc #-}


-- * Show instances

instance Show (TxU c l inc a) where
	show t = "U" ++ show (idTxNM $ metaTxU t)
instance Show (TxM i c l inc a) where
	show t = "M" ++ show (idTxNM $ metaTxM t)
--instance Show (L l inc a) where
--	show t = "L" ++ show (idNM $ metaL t)