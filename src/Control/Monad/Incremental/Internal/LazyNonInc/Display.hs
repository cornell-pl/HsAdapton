{-# LANGUAGE UndecidableInstances, ConstraintKinds, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Control.Monad.Incremental.Internal.LazyNonInc.Display where

import Control.Monad.Incremental.Display
import Control.Monad.Incremental
import Control.Monad.Incremental.Internal.LazyNonInc.Types
import Control.Monad.Incremental.Internal.LazyNonInc.Layers
import Data.Typeable


import Control.Monad.Trans

-- * Display

instance (IncK inc a,Display Outside inc a,Output LazyNonIncU l inc) => Display Outside inc (LazyNonIncU l inc a) where
	displaysPrec proxyL proxyInc m rest = forceOutside m >>= \x -> displaysPrec proxyL proxyInc x rest
	{-# INLINE displaysPrec #-}

instance (IncK inc a,Display Inside inc a,Output LazyNonIncU Inside inc) => Display Inside inc (LazyNonIncU Inside inc a) where
	displaysPrec proxyL proxyInc m rest = force m >>= \x -> displaysPrec proxyL proxyInc x rest
	{-# INLINE displaysPrec #-}

instance (IncK inc a,Display Outside inc a,Input LazyNonIncL l inc) => Display Outside inc (LazyNonIncL l inc a) where
	displaysPrec proxyL proxyInc m rest = getOutside m >>= \x -> displaysPrec proxyL proxyInc x rest
	{-# INLINE displaysPrec #-}

instance (IncK inc a,Display Inside inc a,Input LazyNonIncL Inside inc) => Display Inside inc (LazyNonIncL Inside inc a) where
	displaysPrec proxyL proxyInc m rest = get m >>= \x -> displaysPrec proxyL proxyInc x rest
	{-# INLINE displaysPrec #-}

instance (IncK inc a,Display Outside inc a,Input LazyNonIncM l inc) => Display Outside inc (LazyNonIncM l inc a) where
	displaysPrec proxyL proxyInc m rest = getOutside m >>= \x -> displaysPrec proxyL proxyInc x rest
	{-# INLINE displaysPrec #-}

instance (IncK inc a,Display Inside inc a,Input LazyNonIncM Inside inc) => Display Inside inc (LazyNonIncM Inside inc a) where
	displaysPrec proxyL proxyInc m rest = get m >>= \x -> displaysPrec proxyL proxyInc x rest
	{-# INLINE displaysPrec #-}


-- * NFDataInc

instance (IncK inc a,NFDataInc Outside inc a,Output LazyNonIncU l inc) => NFDataInc Outside inc (LazyNonIncU l inc a) where
	nfDataInc proxyL proxyInc m = forceOutside m >>= nfDataInc proxyL proxyInc
	{-# INLINE nfDataInc #-}

instance (IncK inc a,NFDataInc Inside inc a,Output LazyNonIncU Inside inc) => NFDataInc Inside inc (LazyNonIncU Inside inc a) where
	nfDataInc proxyL proxyInc m = force m >>= nfDataInc proxyL proxyInc
	{-# INLINE nfDataInc #-}

instance (IncK inc a,NFDataInc Outside inc a,Input LazyNonIncL l inc) => NFDataInc Outside inc (LazyNonIncL l inc a) where
	nfDataInc proxyL proxyInc m = getOutside m >>= nfDataInc proxyL proxyInc
	{-# INLINE nfDataInc #-}

instance (IncK inc a,NFDataInc Inside inc a,Input LazyNonIncL Inside inc) => NFDataInc Inside inc (LazyNonIncL Inside inc a) where
	nfDataInc proxyL proxyInc m = get m >>= nfDataInc proxyL proxyInc
	{-# INLINE nfDataInc #-}

instance (IncK inc a,NFDataInc Outside inc a,Input LazyNonIncM l inc) => NFDataInc Outside inc (LazyNonIncM l inc a) where
	nfDataInc proxyL proxyInc m = getOutside m >>= nfDataInc proxyL proxyInc
	{-# INLINE nfDataInc #-}

instance (IncK inc a,NFDataInc Inside inc a,Input LazyNonIncM Inside inc) => NFDataInc Inside inc (LazyNonIncM Inside inc a) where
	nfDataInc proxyL proxyInc m = get m >>= nfDataInc proxyL proxyInc
	{-# INLINE nfDataInc #-}


-- * Show instances

instance Show (LazyNonIncU l inc a) where
	show t = "LazyNonIncU" 
instance Show (LazyNonIncM l inc a) where
	show t = "LazyNonIncM" 
instance Show (LazyNonIncL l inc a) where
	show t = "LazyNonIncL" 
