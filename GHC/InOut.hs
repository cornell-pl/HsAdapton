{-# LANGUAGE TypeFamilies, UndecidableInstances, FlexibleInstances, FlexibleContexts, TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.InOut
-- Copyright   :  (C) 2013 Hugo Pacheco
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Hugo Pacheco <hpacheco@nii.ac.jp>
-- Stability   :  provisional
--
-- Generic sums of products representation for algebraic data types
-- 
--
--
----------------------------------------------------------------------------
module GHC.InOut where

import GHC.Generics

class (Generic a,ToFromRep (Rep a)) => InOut a where
	inn :: F a -> a
	out :: a -> F a
	
type family Flatten (f :: * -> *) :: *
type F a = Flatten (Rep a)

class ToFromRep (f :: * -> *) where
	fromRep :: f x -> Flatten f
	toRep :: Flatten f -> f x

type instance Flatten U1 = ()
type instance Flatten (K1 i c) = c
type instance Flatten (M1 i c f) = Flatten f
type instance Flatten (f :+: g) = Either (Flatten f) (Flatten g)
type instance Flatten (f :*: g) = (Flatten f,Flatten g)

instance ToFromRep U1 where
	fromRep U1 = ()
	toRep () = U1
instance ToFromRep (K1 i c) where
	fromRep (K1 c) = c
	toRep c = K1 c
instance ToFromRep f => ToFromRep (M1 i c f) where
	fromRep (M1 f) = fromRep f
	toRep x = M1 $ toRep x
instance (ToFromRep f,ToFromRep g) => ToFromRep (f :+: g) where
	fromRep (L1 f) = Left (fromRep f)
	fromRep (R1 g) = Right (fromRep g)
	toRep (Left x) = L1 (toRep x)
	toRep (Right y) = R1 (toRep y)
instance (ToFromRep f,ToFromRep g) => ToFromRep (f :*: g) where
	fromRep (f :*: g) = (fromRep f,fromRep g)
	toRep (f,g) = (toRep f :*: toRep g)

instance (Generic a,ToFromRep (Rep a)) => InOut a where
	inn = to . toRep
	out = fromRep . from

--instance InOut [a] where
--	inn s = either (\() -> []) (\(x,xs) -> x:xs) s
--	out l = case l of { [] -> Left (); (x:xs) -> Right (x,xs) }
	
