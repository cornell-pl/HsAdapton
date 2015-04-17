module System.Mem.StableName.Exts (
	  module System.Mem.StableName
	, stableName
	) where

import System.Mem.StableName
import System.IO.Unsafe

{-# NOINLINE stableName #-}
stableName :: a -> StableName a
stableName = unsafePerformIO . makeStableName