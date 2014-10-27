{-# LANGUAGE UndecidableInstances, FlexibleContexts, FlexibleInstances, KindSignatures, MultiParamTypeClasses #-}

module Control.Monad.Incremental.Display where

import Control.Monad.Incremental
import Control.Monad.IO.Class

--import Blaze.ByteString.Builder
--import Data.ByteString.Lazy.Char8 (ByteString)
--import qualified Data.ByteString.Lazy.Char8 as B
--import Blaze.ByteString.Builder.Internal
--import Blaze.Text.Int
--import Blaze.ByteString.Builder.Int

import Data.Monoid
import Data.Typeable
import Control.DeepSeq
import Control.Monad.Trans
import Control.Monad

-- XXX: this should/can be made more generic to allow seamless displaying of mixed outer/inner references/thunks
-- a generic but very slow version of display
--displayGeneric :: (MData NoCtx (Inside inc r m) a,MonadIO m,Layer l inc r m) => a -> l inc r m ()
--displayGeneric x = inside $ gshow x >>= inL . liftIO . putStrLn

displayAs :: (MonadIO m,Layer l inc r m,Display l inc r m a) => String -> a -> l inc r m ()
displayAs str x = (displaysPrec x "") >>= inL . liftIO . putStrLn . (str++)

display :: (MonadIO m,Layer l inc r m,Display l inc r m a) => a -> l inc r m ()
display x = (displaysPrec x "") >>= inL . liftIO . putStrLn

type DisplayS l inc (r :: * -> *) (m :: * -> *) = String -> l inc r m String

class (Layer l inc r m) => Display l inc r m a where
	displaysPrec :: a -> DisplayS l inc r m

instance Display Inside inc r m a => Display Inside inc r m (Inside inc r m a) where
		displaysPrec m rest = m >>= flip displaysPrec rest
instance Display Outside inc r m a => Display Outside inc r m (Outside inc r m a) where
	displaysPrec m rest = m >>= flip displaysPrec rest
instance Display Outside inc r m a => Display Outside inc r m (Inside inc r m a) where
	displaysPrec m rest = inside m >>= flip displaysPrec rest

instance Layer l inc r m => Display l inc r m Char where
	displaysPrec i rest = return $ show i++rest

instance Layer l inc r m => Display l inc r m Bool where
	displaysPrec i rest = return $ show i++rest

instance Layer l inc r m => Display l inc r m Float where
	displaysPrec i rest = return $ show i++rest

instance Layer l inc r m => Display l inc r m Double where
	displaysPrec i rest = return $ show i++rest

instance Layer l inc r m => Display l inc r m Int where
	displaysPrec i rest = return $ show i++rest

instance Layer l inc r m => Display l inc r m Integer where
	displaysPrec i rest = return $ show i++rest

instance (Display l inc r m a,Display l inc r m b) => Display l inc r m (a,b) where
	displaysPrec (x,y) rest = do
		sy <- displaysPrec y (')':rest)
		sx <- displaysPrec x (',':sy)
		return $ '(':sx

instance (Display l inc r m a) => Display l inc r m (Maybe a) where
	displaysPrec Nothing rest = return $ "Nothing" ++ rest
	displaysPrec (Just x) rest = do
		sx <- displaysPrec x (')':rest)
		return $ "(Just "++sx

instance (Typeable a,Display l inc r m a) => Display l inc r m [a] where
	displaysPrec xs rest = case cast xs :: Maybe String of
		Just str -> return $ str ++ rest
		Nothing -> liftM ('[':) $ displayList xs rest

displayList :: Display l inc r m a => [a] -> DisplayS l inc r m
displayList [] rest = return $ ']':rest
displayList [x] rest = displaysPrec x rest
displayList (x:xs) rest = do
	sxs <- displayList xs rest
	displaysPrec x $ ',':sxs
	
--class Layer l inc r m => Serialize l inc r m a where
--	serialize :: Proxy (l inc r m) -> a -> Builder
--
--instance (Layer l inc r m) => Serialize l inc r m Int where
--	serialize _ = \i -> integral i
--	{-# INLINE serialize #-}
--
--displayBlaze :: (Layer l inc r m,Serialize l inc r m a,InIO (l inc r m)) => a -> l inc r m ()
--displayBlaze = displayBlaze' Proxy
--
--{-# INLINE displayBlaze' #-}
--displayBlaze' :: (Layer l inc r m,Serialize l inc r m a,InIO (l inc r m)) => Proxy (l inc r m) -> a -> l inc r m ()
--displayBlaze' = \proxy a -> runInIO $ B.putStrLn $ toLazyByteString $ serialize proxy a

-- | An incremental version of @NFData@ that only forces dirty thunks
class Layer l inc r m => NFDataInc l inc r m a where
	rnfInc :: a -> l inc r m ()

instance Layer l inc r m => NFDataInc l inc r m Int where
	rnfInc = return . rnf
	{-# INLINE rnfInc #-}






