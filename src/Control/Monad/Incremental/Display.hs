{-# LANGUAGE OverlappingInstances, UndecidableInstances, FlexibleContexts, FlexibleInstances, KindSignatures, MultiParamTypeClasses #-}

module Control.Monad.Incremental.Display (
	  Display(..), DisplayS(..), DisplayDict(..)
	, display, displayProxy
	, NFDataInc(..), NFDataIncDict(..)
	, rnfInc, nfDataIncProxy
	) where

import Control.Monad.Incremental
import Control.Monad.IO.Class

import Data.Monoid
import Data.Typeable
import Control.DeepSeq
import Control.Monad.Trans
import Control.Monad
import Data.Typeable
import Data.WithClass.MData
import Control.Monad.Ref


class (Layer l inc) => Display l inc a where
	displaysPrec :: Proxy l -> Proxy inc -> a -> DisplayS l inc

data DisplayDict l inc a = DisplayDict { displayDict :: Proxy l -> Proxy inc -> a -> DisplayS l inc }

instance (Display l inc a) => Sat (DisplayDict l inc a) where
	dict = DisplayDict { displayDict = displaysPrec }

display :: (Layer l inc,Display l inc a) => a -> l inc String
display x = displaysPrec Proxy Proxy x ""

type DisplayS l inc = String -> l inc String

instance Display Inside inc a => Display Inside inc (Inside inc a) where
	displaysPrec proxyL proxyInc m rest = m >>= flip (displaysPrec proxyL proxyInc) rest
instance Display Outside inc a => Display Outside inc (Outside inc a) where
	displaysPrec proxyL proxyInc m rest = m >>= flip (displaysPrec proxyL proxyInc) rest
instance Display Outside inc a => Display Outside inc (Inside inc a) where
	displaysPrec proxyL proxyInc m rest = inside m >>= flip (displaysPrec proxyL proxyInc) rest

instance Layer l inc => Display l inc Char where
	displaysPrec proxyL proxyInc i rest = return $ show i++rest

instance Layer l inc => Display l inc Bool where
	displaysPrec proxyL proxyInc i rest = return $ show i++rest

instance Layer l inc => Display l inc Float where
	displaysPrec proxyL proxyInc i rest = return $ show i++rest

instance Layer l inc => Display l inc Double where
	displaysPrec proxyL proxyInc i rest = return $ show i++rest

instance Layer l inc => Display l inc Int where
	displaysPrec proxyL proxyInc i rest = return $ show i++rest

instance Layer l inc => Display l inc Integer where
	displaysPrec proxyL proxyInc i rest = return $ show i++rest

instance (Display l inc a,Display l inc b) => Display l inc (a,b) where
	displaysPrec proxyL proxyInc (x,y) rest = do
		sy <- displaysPrec proxyL proxyInc y (')':rest)
		sx <- displaysPrec proxyL proxyInc x (',':sy)
		return $ '(':sx

instance (Display l inc a) => Display l inc (Maybe a) where
	displaysPrec proxyL proxyInc Nothing rest = return $ "Nothing" ++ rest
	displaysPrec proxyL proxyInc (Just x) rest = do
		sx <- displaysPrec proxyL proxyInc x (')':rest)
		return $ "(Just "++sx

instance (Typeable a,Display l inc a) => Display l inc [a] where
	displaysPrec proxyL proxyInc xs rest = case cast xs :: Maybe String of
		Just str -> return $ str ++ rest
		Nothing -> liftM ('[':) $ displayList proxyL proxyInc xs rest

displayList :: Display l inc a => Proxy l -> Proxy inc -> [a] -> DisplayS l inc
displayList proxyL proxyInc [] rest = return $ ']':rest
displayList proxyL proxyInc [x] rest = displaysPrec proxyL proxyInc x rest
displayList proxyL proxyInc (x:xs) rest = do
	sxs <- displayList proxyL proxyInc xs rest
	displaysPrec proxyL proxyInc x $ ',':sxs
	
-- default instance for arbitrary types
instance (Layer l inc,MData (DisplayDict l inc) (l inc) a) => Display l inc a where
	displaysPrec l inc v rest = do
		ms <- gmapQ (displayProxy l inc) (\v -> return $ mshowChar ' ' <=< displayDict dict l inc v) v
		str <- liftM showConstr $ toConstr (displayProxy l inc) v
		let e = mshowChar '(' <=< mshowString str <=< (foldr (<=<) return ms) <=< mshowChar ')'
		e rest

displayProxy :: Proxy l -> Proxy inc -> Proxy (DisplayDict l inc)
displayProxy l inc = Proxy

type MShowS m = String -> m String

mshowChar :: Monad m => Char -> MShowS m
mshowChar c s = return (c:s)
	
mshow :: (Show a,Monad m) => a -> MShowS m
mshow x s = return $ show x ++ s

mshowString :: Monad m => String -> MShowS m
mshowString c s = return (c++s)

-- | An incremental version of @NFData@ that only forces dirty thunks
class (Layer l inc) => NFDataInc l inc a where
	nfDataInc :: Proxy l -> Proxy inc -> a -> l inc ()

instance (NFDataInc l inc a) => Sat (NFDataIncDict l inc a) where
	dict = NFDataIncDict { nfDataIncDict = nfDataInc }

data NFDataIncDict (l :: * -> * -> *) inc a = NFDataIncDict { nfDataIncDict :: Proxy l -> Proxy inc -> a -> l inc () }

instance Layer l inc => NFDataInc l inc Int where
	nfDataInc l inc = return . rnf
	{-# INLINE nfDataInc #-}

instance Layer l inc => NFDataInc l inc String where
	nfDataInc l inc = return . rnf
	{-# INLINE nfDataInc #-}

rnfInc :: NFDataInc l inc a => a -> l inc ()
rnfInc = nfDataInc Proxy Proxy

nfDataIncProxy :: Proxy l -> Proxy inc -> Proxy (NFDataIncDict l inc)
nfDataIncProxy l inc = Proxy

-- default instance for arbitrary types
instance (Layer l inc,MData (NFDataIncDict l inc) (l inc) a) => NFDataInc l inc a where
	nfDataInc l inc v = gmapQr (nfDataIncProxy l inc) (\() () -> return ()) () (nfDataIncDict dict l inc) v




