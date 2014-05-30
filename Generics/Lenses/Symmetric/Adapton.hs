{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Generics.Lenses.Symmetric.Adapton where

import Control.Monad.BiAdapton
import Control.Monad.BiAdapton.DataTypes
import Generics.Lenses.Symmetric.Language hiding (get)
import Data.IntMap (IntMap(..))
import Data.IntMap as IntMap
import Control.Monad.Trans.Class
import Data.Typeable
import Control.Monad.State (State(..),MonadState(..),StateT(..))
import qualified Control.Monad.State as State
import System.Mem.MemoTable
import Control.Monad.Ref
import Control.Monad.Box

type ReadCompl m r a = Inner m r a -> Inner m r (U m r a)

readSym :: (Box k m,Memo a,Typeable a,Layer Inner m r) => SymLens (Inner m r) k (ReadCompl m r a) (U m r a) a
readSym = SymLens missing' putr' putl' where
	missing' = inL $ box $ thunk Backward
	putr' t = liftStateTLens unboxLns $ do
		x <- lift (force Forward t)
		State.put $ rethunk Backward t
		return x
	putl' x = liftStateTLens unboxLns $ do
		ftx <- State.get
		t <- lift $ ftx (return x)
		State.put $ rethunk Backward t
		return t

writeSym :: (Box k m,Memo a,Typeable a,Layer Inner m r) => SymLens (Inner m r) k (ReadCompl m r a) a (U m r a)
writeSym = SymLens missing' putr' putl' where
	missing' = inL $ box $ thunk Forward
	putr' x = liftStateTLens unboxLns $ do
		ftx <- State.get
		t <- lift $ ftx (return x)
		State.put $ rethunk Forward t
		return t
	putl' t = liftStateTLens unboxLns $ do
		x <- lift (force Backward t)
		State.put $ rethunk Forward t
		return x

type ModCompl m r a b c =
	( Inner m r a -> Inner m r (U m r a)
	, Inner m r b -> Inner m r (U m r b)
	, r c)

modSym :: (Box r (Inner m r),Box r m,Typeable a,Typeable b,Memo a,Memo b,Memo c,Typeable c,Layer Inner m r) => SymLens (Inner m r) r c a b -> SymLens (Inner m r) r (ModCompl m r a b c) (U m r a) (U m r b)
modSym l = SymLens missing' putr' putl' where
	missing' = missing l >>= \kc -> box (thunk Backward,thunk Forward,kc)
	putr' tx = liftStateTLens unboxLns $ do
		(ftx,fty,kc) <- State.get
		ty' <- lift $ fty $ force Forward tx >>= \x -> State.evalStateT (putr l x) kc
--		(ty',kc') <- lift $ fty $ force Forward tx >>= \x -> State.runStateT (putr l x) kc
		State.put (rethunk Backward tx,rethunk Forward ty',kc)
		return ty'
	putl' ty = liftStateTLens unboxLns $ do
		(ftx,fty,kc) <- State.get
		tx' <- lift $ ftx $ force Backward ty >>= \y -> State.evalStateT (putl l y) kc
--		(tx',kc') <- lift $ ftx $ force Backward ty >>= \y -> State.runStateT (putl l y) kc
		State.put (rethunk Backward tx',rethunk Forward ty,kc)
		return tx'


--inLSym :: (Monad (l m r),Ref m r,InL l) => SymLens m k c a b -> SymLens (l m r) k c a b
--inLSym l = effectSym inL (const inL) l

--inLLns :: (Monad (l m r),Ref m r,InL l) => Lens m s v -> Lens (l m r) s v
--inLLns = undefined














