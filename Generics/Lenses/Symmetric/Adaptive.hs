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
import Control.Monad.Adaptive.DataTypes
import GHC.TypeLits
import Control.Monad.Ref
import Data.List as List
import Data.Maybe
import qualified Control.Monad.Adaptive.CircularList as CL
import qualified Control.Monad.Adaptive.OrderedList as OL
import Control.Monad.Trans.Identity

import Debug.Trace
import Control.Monad.Box

--instance Box r m => Box r (Changeable m r) where
--	box v = inM $ box v
--	unbox k = inM $ unbox k
--	inbox k f = inM $ inbox k f

type ReadCompl m r a = (Changeable m r a -> Changeable m r (Modifiable m r a))

readSym :: (Eq a,Ref m r,Box k m) => SymLens (Changeable m r) k (ReadCompl m r a) (Modifiable m r a) a
readSym = SymLens missing' putr' putl' where
	missing' = do
		let fmx = newMod
		inM $ box fmx
	putr' mx' = do
		v <- lift $ readRef (value mx')
		trace ("readingS "++show (refId $ value mx') ++ " ") $ liftStateTLens unboxLns $ do
			x' <- lift $ readModC mx'
			State.put $ (changeModC mx')
			return x'
	putl' x' = liftStateTLens unboxLns $ do
		fmx <- State.get
		mx' <- lift $ fmx $ return x'
		State.put $ (changeModC mx')
		return mx'

writeSym :: (Eq a,Ref m r,Box k m) => SymLens (Changeable m r) k (ReadCompl m r a) a (Modifiable m r a)
writeSym = dualSym readSym

--type ModCompl m r k a b c = (k (ReadCompl m r a),k (k c,k (ReadCompl m r b)))

-- this works, but we are losing the locality of updates
--modSym :: (Show a,Show b,Show c,Eq a,Eq b,Ref m r,Box k m,Box k (Changeable m r)) => SymLens (Changeable m r) k c a b -> SymLens (Changeable m r) k (ModCompl m r k a b c) (Modifiable m r a) (Modifiable m r b)
--modSym l = readSym <.> l <.> writeSym 

type ModCompl m r a b c =
   ( Changeable m r a -> Changeable m r (Modifiable m r a)
   , Changeable m r b -> Changeable m r (Modifiable m r b)
   , r c)

modSym :: (Eq a,Eq b,Ref m r,Box r m,Box r (Changeable m r)) => SymLens (Changeable m r) r c a b -> SymLens (Changeable m r) r (ModCompl m r a b c) (Modifiable m r a) (Modifiable m r b)
modSym l = SymLens missing' putr' putl' where
	missing' = missing l >>= \kc -> box (newMod,newMod,kc)
	putr' mx' = liftStateTLens unboxLns $ do
		(fmx,fmy,kc) <- State.get
		my' <- lift $ fmy $ readMod mx' >>= \x -> State.evalStateT (putr l x) kc
--		(my',kc') <- lift $ fmy $ readMod mx' >>= \x -> State.runStateT (putr l x) kc
		State.put (changeMod mx',changeMod my',kc)
		return my'
	putl' my' = liftStateTLens unboxLns $ do
		(fmx,fmy,kc) <- State.get
		mx' <- lift $ fmx $ readMod my' >>= \y -> State.evalStateT (putl l y) kc
--		(mx',kc') <- lift $ fmx $ readMod my' >>= \y -> State.runStateT (putl l y) kc
		State.put (changeMod mx',changeMod my',kc)
		return mx'
















