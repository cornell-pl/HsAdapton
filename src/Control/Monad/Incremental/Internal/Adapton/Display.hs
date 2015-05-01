{-# LANGUAGE UndecidableInstances, ConstraintKinds, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Control.Monad.Incremental.Internal.Adapton.Display where

import Control.Monad.Incremental.Display
import Control.Monad.Incremental
import Control.Monad.Incremental.Internal.Adapton.Types
import Control.Monad.Incremental.Internal.Adapton.Layers

import Control.Monad.Trans
import Data.Typeable
import Control.Monad

-- * Display

instance (IncK inc a,Display Outside inc  a,Input M l inc ) => Display Outside inc  (M l inc  a) where
	displaysPrec proxyL proxyInc m rest = getOutside m >>= \x -> liftM (\x -> "<" ++ (show $ idNM $ metaM m) ++ " " ++ x) $ displaysPrec proxyL proxyInc x ('>':rest)
	{-# INLINE displaysPrec #-}

instance (IncK inc a,Display Inside inc  a,Input M Inside inc ) => Display Inside inc  (M Inside inc  a) where
	displaysPrec proxyL proxyInc m rest = get m >>= \x -> liftM (\x -> "<" ++ (show $ idNM $ metaM m) ++ " " ++ x) $ displaysPrec proxyL proxyInc x ('>':rest)
	{-# INLINE displaysPrec #-}
	
instance (IncK inc a,Display Outside inc  a,Output U l inc ) => Display Outside inc  (U l inc  a) where
	displaysPrec proxyL proxyInc m rest = forceOutside m >>= \x -> liftM (\x -> "<" ++ (show $ idNM $ metaU m) ++ " " ++ x) $ displaysPrec proxyL proxyInc x ('>':rest)
	{-# INLINE displaysPrec #-}

instance (IncK inc a,Display Inside inc  a,Output U Inside inc ) => Display Inside inc  (U Inside inc  a) where
	displaysPrec proxyL proxyInc m rest = force m >>= \x -> liftM (\x -> "<" ++ (show $ idNM $ metaU m) ++ " " ++ x) $ displaysPrec proxyL proxyInc x ('>':rest)
	{-# INLINE displaysPrec #-}

instance (IncK inc a,Display Outside inc  a,Input L l inc ) => Display Outside inc  (L l inc  a) where
	displaysPrec proxyL proxyInc m rest = getOutside m >>= \x -> liftM (\x -> "<" ++ (show $ idNM $ metaL m) ++ " " ++ x) $ displaysPrec proxyL proxyInc x ('>':rest)
	{-# INLINE displaysPrec #-}

instance (IncK inc a,Display Inside inc  a,Input L Inside inc ) => Display Inside inc  (L Inside inc  a) where
	displaysPrec proxyL proxyInc m rest = get m >>= \x -> liftM (\x -> "<" ++ (show $ idNM $ metaL m) ++ " " ++ x) $ displaysPrec proxyL proxyInc x ('>':rest)
	{-# INLINE displaysPrec #-}

-- * NFDataInc

instance (IncK inc a,NFDataInc Outside inc  a,Input M l inc ) => NFDataInc Outside inc  (M l inc  a) where
	nfDataInc proxyL proxyInc m = getOutside m >>= nfDataInc proxyL proxyInc
	{-# INLINE nfDataInc #-}

instance (IncK inc a,NFDataInc Inside inc  a,Input M Inside inc ) => NFDataInc Inside inc  (M Inside inc  a) where
	nfDataInc proxyL proxyInc m = get m >>= nfDataInc proxyL proxyInc
	{-# INLINE nfDataInc #-}
	
instance (IncK inc a,NFDataInc Outside inc  a,Output U l inc ) => NFDataInc Outside inc  (U l inc  a) where
	nfDataInc proxyL proxyInc m = forceOutside m >>= nfDataInc proxyL proxyInc
	{-# INLINE nfDataInc #-}

instance (IncK inc a,NFDataInc Inside inc  a,Output U Inside inc ) => NFDataInc Inside inc  (U Inside inc  a) where
	nfDataInc proxyL proxyInc m = force m >>= nfDataInc proxyL proxyInc
	{-# INLINE nfDataInc #-}

instance (IncK inc a,NFDataInc Outside inc  a,Input L l inc ) => NFDataInc Outside inc  (L l inc  a) where
	nfDataInc proxyL proxyInc m = getOutside m >>= nfDataInc proxyL proxyInc
	{-# INLINE nfDataInc #-}

instance (IncK inc a,NFDataInc Inside inc  a,Input L Inside inc ) => NFDataInc Inside inc  (L Inside inc  a) where
	nfDataInc proxyL proxyInc m = get m >>= nfDataInc proxyL proxyInc
	{-# INLINE nfDataInc #-}


-- * Show instances

instance Show (U l inc  a) where
	show t = "U" ++ show (idNM $ metaU t)
instance Show (M l inc  a) where
	show t = "M" ++ show (idNM $ metaM t)
instance Show (L l inc  a) where
	show t = "L" ++ show (idNM $ metaL t)