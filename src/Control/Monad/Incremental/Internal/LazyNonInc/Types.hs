{-# LANGUAGE ScopedTypeVariables, KindSignatures, DeriveDataTypeable, StandaloneDeriving #-}

module Control.Monad.Incremental.Internal.LazyNonInc.Types where

import Data.Unique
import Data.Typeable
import Data.DeepTypeable
import Language.Haskell.TH.Syntax
import Data.IORef

-- output modifiables that are always recomputed from scratch
newtype LazyNonIncU (l :: * -> * -> *) inc a = LazyNonIncU (
		IORef (l inc a)
	)

deriving instance Typeable LazyNonIncU

instance DeepTypeable LazyNonIncU where
	typeTree _ = MkTypeTree (mkName "Control.Monad.Incremental.LazyNonInc.Types.LazyNonIncU") [] []

instance (DeepTypeable l,DeepTypeable inc,DeepTypeable a) => DeepTypeable (LazyNonIncU l inc a) where
	typeTree (_ :: Proxy (LazyNonIncU l inc a)) = MkTypeTree (mkName "Control.Monad.Incremental.LazyNonInc.Types.LazyNonIncU") args [MkConTree (mkName "Control.Monad.Incremental.LazyNonInc.ref") [typeTree (Proxy::Proxy a)]]
		where args = [typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy a)]

-- lazy input modifiables with sharing
newtype LazyNonIncL (l :: * -> * -> *) inc a = LazyNonIncL (
		IORef (l inc a)
	)

deriving instance Typeable LazyNonIncL

instance DeepTypeable LazyNonIncL where
	typeTree _ = MkTypeTree (mkName "Control.Monad.Incremental.LazyNonInc.Types.LazyNonIncL") [] []


instance (DeepTypeable l,DeepTypeable inc,DeepTypeable a) => DeepTypeable (LazyNonIncL l inc a) where
	typeTree (_ :: Proxy (LazyNonIncL l inc a)) = MkTypeTree (mkName "Control.Monad.Incremental.LazyNonInc.Types.LazyNonIncL") args [MkConTree (mkName "Control.Monad.Incremental.LazyNonInc.ref") [typeTree (Proxy::Proxy a)]]
		where args = [typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy a)]

-- strict input modifiables
newtype LazyNonIncM (l :: * -> * -> *) inc a = LazyNonIncM (
		IORef a
	)

deriving instance Typeable LazyNonIncM

instance DeepTypeable LazyNonIncM where
	typeTree _ = MkTypeTree (mkName "Control.Monad.Incremental.LazyNonInc.Types.LazyNonIncM") [] []

instance (DeepTypeable l,DeepTypeable inc,DeepTypeable a) => DeepTypeable (LazyNonIncM l inc a) where
	typeTree (_ :: Proxy (LazyNonIncM l inc a)) = MkTypeTree (mkName "Control.Monad.Incremental.LazyNonInc.Types.LazyNonIncM") args [MkConTree (mkName "Control.Monad.Incremental.LazyNonInc.ref") [typeTree (Proxy::Proxy a)]]
		where args = [typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy a)]

instance Eq (LazyNonIncU l inc a) where
	t1 == t2 = error "no equality"
instance Eq (LazyNonIncL l inc a) where
	t1 == t2 = error "no equality"
instance Eq (LazyNonIncM l inc a) where
	t1 == t2 = error "no equality"


