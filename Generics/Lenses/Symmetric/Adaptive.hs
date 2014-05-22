{-# LANGUAGE FlexibleContexts, RankNTypes, DeriveGeneric, ScopedTypeVariables, ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances, DoAndIfThenElse, UndecidableInstances, FunctionalDependencies, KindSignatures, DataKinds, GADTs #-}

module Generics.Lenses.Symmetric.Adaptive where

import Generics.Lenses.Symmetric.Language
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State (State(..),StateT(..),MonadState)
import qualified Control.Monad.State as State
import Control.Monad.Reader (Reader(..),ReaderT(..),MonadReader)
import qualified Control.Monad.Reader as Reader
import GHC.Generics
import Control.Monad.Adaptive
import GHC.TypeLits
import Control.Monad.Adaptive.Ref
import Data.List as List
import Data.Maybe
import qualified Control.Monad.Adaptive.CircularList as CL
import qualified Control.Monad.Adaptive.OrderedList as OL
import Control.Monad.Trans.Identity
import Data.IORef

import Debug.Trace

instance Ref m ShowRef => Box ShowRef m where
	box = newRef
	unbox = readRef
	inbox r f = readRef r >>= f >>= writeRef r >> return r

instance Ref m IORef => Box IORef m where
	box = newRef
	unbox = readRef
	inbox r f = readRef r >>= f >>= writeRef r >> return r

type ReadCompl m r a = (Changeable m r a -> Changeable m r (Modifiable m r a))

readSym :: (Show a,Eq a,Ref m r,Box k m,Box k (Changeable m r)) => SymLens (Changeable m r) k (ReadCompl m r a) (Modifiable m r a) a
readSym = SymLens missing' putr' putl' where
	missing' = do
		let fmx = newModC
		box fmx
	putr' mx' = do
		v <- lift $ readRef (value mx')
		trace ("readingS "++show (refId $ value mx') ++ " " ++ show v) $ liftStateTLens unboxLns $ do
			x' <- lift $ readModC mx'
			State.put $ (changeModC mx')
			return x'
	putl' x' = liftStateTLens unboxLns $ do
		fmx <- State.get
		mx' <- lift $ fmx $ return x'
		State.put $ (changeModC mx')
		return mx'

writeSym :: (Show a,Eq a,Ref m r,Box k m,Box k (Changeable m r)) => SymLens (Changeable m r) k (ReadCompl m r a) a (Modifiable m r a)
writeSym = dualSym readSym

--type ModCompl m r k a b c = (k (ReadCompl m r a),k (k c,k (ReadCompl m r b)))

-- this works, but we are losing the locality of updates
--modSym :: (Show a,Show b,Show c,Eq a,Eq b,Ref m r,Box k m,Box k (Changeable m r)) => SymLens (Changeable m r) k c a b -> SymLens (Changeable m r) k (ModCompl m r k a b c) (Modifiable m r a) (Modifiable m r b)
--modSym l = readSym <.> l <.> writeSym 

type ModCompl m r k a b c =
   ( Changeable m r (a,k c) -> Changeable m r (Modifiable m r a,k c)
   , Changeable m r (b,k c) -> Changeable m r (Modifiable m r b,k c)
   , k c)

modSym :: (Show a,Show b,Show c,Eq a,Eq b,Ref m r,Box k m,Box k (Changeable m r)) => SymLens (Changeable m r) k c a b -> SymLens (Changeable m r) k (ModCompl m r k a b c) (Modifiable m r a) (Modifiable m r b)
modSym l = SymLens missing' putr' putl' where
	missing' = missing l >>= \kc -> box (newModWithC,newModWithC,kc)
	putr' mx' = liftStateTLens unboxLns $ do
		(fmx,fmy,kc) <- State.get
		(my',kc') <- lift $ fmy $ readMod mx' >>= \x -> State.runStateT (putr l x) kc
		State.put (changeModWithC mx',changeModWithC my',kc')
		return my'
	putl' my' = liftStateTLens unboxLns $ do
		(fmx,fmy,kc) <- State.get
		(mx',kc') <- lift $ fmx $ readMod my' >>= \y -> State.runStateT (putl l y) kc
		State.put (changeModWithC mx',changeModWithC my',kc')
		return mx'
















