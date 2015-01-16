{-# LANGUAGE OverlappingInstances, UndecidableInstances, FlexibleContexts, FlexibleInstances, KindSignatures, MultiParamTypeClasses #-}

module Control.Monad.Incremental.Display where

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
import System.Mem.WeakKey

class (Layer l inc r m) => Display l inc r m a where
	displaysPrec :: Proxy l -> Proxy inc -> Proxy r -> Proxy m -> a -> DisplayS l inc r m

data DisplayDict l inc r m a = DisplayDict { displayDict :: Proxy l -> Proxy inc -> Proxy r -> Proxy m -> a -> DisplayS l inc r m }

instance (Display l inc r m a) => Sat (DisplayDict l inc r m a) where
	dict = DisplayDict { displayDict = displaysPrec }

displayAs :: (MonadIO m,Layer l inc r m,Display l inc r m a) => String -> a -> l inc r m ()
displayAs str x = (displaysPrec Proxy Proxy Proxy Proxy x "") >>= inL . liftIO . putStrLn . (str++)

display :: (MonadIO m,Layer l inc r m,Display l inc r m a) => a -> l inc r m ()
display x = (displaysPrec Proxy Proxy Proxy Proxy x "") >>= inL . liftIO . putStrLn

showInc :: (Layer l inc r m,Display l inc r m a) => a -> l inc r m String
showInc x = displaysPrec Proxy Proxy Proxy Proxy x ""

type DisplayS l inc (r :: * -> *) (m :: * -> *) = String -> l inc r m String

instance Display Inside inc r m a => Display Inside inc r m (Inside inc r m a) where
	displaysPrec proxyL proxyInc proxyR proxyM m rest = m >>= flip (displaysPrec proxyL proxyInc proxyR proxyM) rest
instance Display Outside inc r m a => Display Outside inc r m (Outside inc r m a) where
	displaysPrec proxyL proxyInc proxyR proxyM m rest = m >>= flip (displaysPrec proxyL proxyInc proxyR proxyM) rest
instance Display Outside inc r m a => Display Outside inc r m (Inside inc r m a) where
	displaysPrec proxyL proxyInc proxyR proxyM m rest = inside m >>= flip (displaysPrec proxyL proxyInc proxyR proxyM) rest

instance Layer l inc r m => Display l inc r m Char where
	displaysPrec proxyL proxyInc proxyR proxyM i rest = return $ show i++rest

instance Layer l inc r m => Display l inc r m Bool where
	displaysPrec proxyL proxyInc proxyR proxyM i rest = return $ show i++rest

instance Layer l inc r m => Display l inc r m Float where
	displaysPrec proxyL proxyInc proxyR proxyM i rest = return $ show i++rest

instance Layer l inc r m => Display l inc r m Double where
	displaysPrec proxyL proxyInc proxyR proxyM i rest = return $ show i++rest

instance Layer l inc r m => Display l inc r m Int where
	displaysPrec proxyL proxyInc proxyR proxyM i rest = return $ show i++rest

instance Layer l inc r m => Display l inc r m Integer where
	displaysPrec proxyL proxyInc proxyR proxyM i rest = return $ show i++rest

instance (Display l inc r m a,Display l inc r m b) => Display l inc r m (a,b) where
	displaysPrec proxyL proxyInc proxyR proxyM (x,y) rest = do
		sy <- displaysPrec proxyL proxyInc proxyR proxyM y (')':rest)
		sx <- displaysPrec proxyL proxyInc proxyR proxyM x (',':sy)
		return $ '(':sx

instance (Display l inc r m a) => Display l inc r m (Maybe a) where
	displaysPrec proxyL proxyInc proxyR proxyM Nothing rest = return $ "Nothing" ++ rest
	displaysPrec proxyL proxyInc proxyR proxyM (Just x) rest = do
		sx <- displaysPrec proxyL proxyInc proxyR proxyM x (')':rest)
		return $ "(Just "++sx

instance (Typeable a,Display l inc r m a) => Display l inc r m [a] where
	displaysPrec proxyL proxyInc proxyR proxyM xs rest = case cast xs :: Maybe String of
		Just str -> return $ str ++ rest
		Nothing -> liftM ('[':) $ displayList proxyL proxyInc proxyR proxyM xs rest

displayList :: Display l inc r m a => Proxy l -> Proxy inc -> Proxy r -> Proxy m -> [a] -> DisplayS l inc r m
displayList proxyL proxyInc proxyR proxyM [] rest = return $ ']':rest
displayList proxyL proxyInc proxyR proxyM [x] rest = displaysPrec proxyL proxyInc proxyR proxyM x rest
displayList proxyL proxyInc proxyR proxyM (x:xs) rest = do
	sxs <- displayList proxyL proxyInc proxyR proxyM xs rest
	displaysPrec proxyL proxyInc proxyR proxyM x $ ',':sxs
	
-- default instance for arbitrary types
instance (Layer l inc r m,MData (DisplayDict l inc r m) (l inc r m) a) => Display l inc r m a where
	displaysPrec l inc r m v rest = do
		ms <- gmapQ (displayProxy l inc r m) (\v -> return $ mshowChar ' ' <=< displayDict dict l inc r m v) v
		str <- liftM showConstr $ toConstr (displayProxy l inc r m) v
		let e = mshowChar '(' <=< mshowString str <=< (foldr (<=<) return ms) <=< mshowChar ')'
		e rest

displayProxy :: Proxy l -> Proxy inc -> Proxy r -> Proxy m -> Proxy (DisplayDict l inc r m)
displayProxy l inc r m = Proxy

type MShowS m = String -> m String

mshowChar :: Monad m => Char -> MShowS m
mshowChar c s = return (c:s)
	
mshow :: (Show a,Monad m) => a -> MShowS m
mshow x s = return $ show x ++ s

mshowString :: Monad m => String -> MShowS m
mshowString c s = return (c++s)

-- | An incremental version of @NFData@ that only forces dirty thunks
class (Layer l inc r m) => NFDataInc l inc r m a where
	rnfInc :: Proxy l -> Proxy inc -> Proxy r -> Proxy m -> a -> l inc r m ()

instance (NFDataInc l inc r m a) => Sat (NFDataIncDict l inc r m a) where
	dict = NFDataIncDict { rnfIncDict = rnfInc }

data NFDataIncDict (l :: * -> (* -> *) -> (* -> *) -> * -> *) inc r m a = NFDataIncDict { rnfIncDict :: Proxy l -> Proxy inc -> Proxy r -> Proxy m -> a -> l inc r m () }

instance Layer l inc r m => NFDataInc l inc r m Int where
	rnfInc l inc r m = return . rnf
	{-# INLINE rnfInc #-}

instance Layer l inc r m => NFDataInc l inc r m String where
	rnfInc l inc r m = return . rnf
	{-# INLINE rnfInc #-}

nfDataIncProxy :: Proxy l -> Proxy inc -> Proxy r -> Proxy m -> Proxy (NFDataIncDict l inc r m)
nfDataIncProxy l inc r m = Proxy

-- default instance for arbitrary types
instance (Layer l inc r m,MData (NFDataIncDict l inc r m) (l inc r m) a) => NFDataInc l inc r m a where
	rnfInc l inc r m v = gmapQr (nfDataIncProxy l inc r m) (\() () -> return ()) () (rnfIncDict dict l inc r m) v




