{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
-- A class for monads with mutable references

module Control.Monad.Ref where

import Control.Monad.ST.Lazy
import Data.IORef
import Data.STRef.Lazy
import Control.Monad.State (State(..),StateT(..))
import Control.Monad.State as State
import Data.Typeable

import System.IO.Unsafe

import Debug.Trace

-- | Reference equality class
class EqRef r where
	eqRef :: r a -> r a -> Bool
	refId :: r a -> Id
	refId r = error "no reference ids for this monad"

-- | References @r@ under monad @m@
class (EqRef r, Monad m) => Ref m r where
	newRef :: a -> m (r a) -- you should avoid using this function, as it does not handle unique ids. new refs should always be created under Inner or Outer instead
	readRef  :: r a -> m a
	writeRef :: r a -> a -> m ()

class Ref m r => UniqueRef m r where
	-- | creates a reference with a unique id, what is useful for implementation purposes
	newUniqueRef :: a -> m (r a)

data IdRef r a = IdRef Id (r a) deriving Typeable

instance EqRef r => EqRef (IdRef r) where
	eqRef (IdRef i1 r1) (IdRef i2 r2) = eqRef r1 r2
	refId (IdRef i r) = i

instance EqRef (STRef s) where
	eqRef r1 r2 = r1 == r2
instance EqRef (IORef) where
	eqRef r1 r2 = r1 == r2

--
--instance Ref m r => Ref m (IdRef r) where
--	newRef v = newRef v >>= return . IdRef (-1)
--	readRef (IdRef i r) = readRef r
--	writeRef (IdRef i r) v = writeRef r v

instance Ref IO IORef where
	newRef = newIORef
	readRef = readIORef
	writeRef = writeIORef

instance Ref (ST s) (STRef s) where
	newRef = newSTRef
	readRef = readSTRef
	writeRef = writeSTRef

mapRefM :: Ref m r => (a -> m a) -> r a -> m (r a)
mapRefM f r = readRef r >>= f >>= writeRef r >> return r

mapRef :: Ref m r => (a -> a) -> r a -> m ()
mapRef f r = readRef r >>= writeRef r . f

type Id = Int
	
	
	
