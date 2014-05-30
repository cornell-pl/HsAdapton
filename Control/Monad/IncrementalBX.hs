{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, TypeOperators, UndecidableInstances, GADTs, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, DataKinds, TypeFamilies, KindSignatures, ConstraintKinds #-}

module Control.Monad.IncrementalBX where

import GHC.Exts hiding (lazy)
import Generics.Lenses.Symmetric.Language
import Control.Monad.BiAdapton as BiAdapton
import Control.Monad.BiAdapton.DataTypes
import Control.Monad.Adaptive as Adaptive
import Control.Monad.Adaptive.DataTypes
import Data.Typeable
import System.Mem.MemoTable
import Control.Monad.Ref
import Generics.Lenses.Symmetric.Adapton as SymAdapton
import Generics.Lenses.Symmetric.Adaptive as SymAdaptive
import Control.Monad.Trans
import GHC.Generics
import Control.Monad.Box
import Data.Proxy

data Inc = Eager | Lazy

deriving instance Typeable Lazy
deriving instance Typeable Eager

eager = Proxy :: Proxy Eager
lazy = Proxy :: Proxy Lazy

class (Box r (Inside inc m r),Box r m,Ref m r,Monad (Outside inc m r),Monad (Inside inc m r),MonadIO m) => IncrementalBX (inc :: Inc) m r where
	
	-- * Inside/outside world
	type Inside inc :: (* -> *) -> (* -> *) -> * -> *
	type Outside inc :: (* -> *) -> (* -> *) -> * -> *
	
	-- * Modifiable data type
	type M inc :: (* -> *) -> (* -> *) -> * -> *
	
	-- * Necessary constraints on values
	type K inc m r a :: Constraint
	
	-- * Outside world operations
	run :: Proxy inc -> Outside inc m r a -> m a
	inside :: Proxy inc -> Inside inc m r a -> Outside inc m r a
	get :: (K inc m r a) => Proxy inc -> M inc m r a -> Outside inc m r a
	change :: (K inc m r a) => Proxy inc -> M inc m r a -> a -> Outside inc m r ()
	new :: (K inc m r a) => Proxy inc -> a -> Outside inc m r (M inc m r a)
	display :: (Display inc m r a) => Proxy inc -> a -> Outside inc m r ()
	displayAs :: (Display inc m r a) => Proxy inc -> String -> a -> Outside inc m r ()
	
	-- * Symmetric lens complement types
	type ReadCpl inc m r a :: *
	type ModCpl inc m r a b c :: *
	
	-- * Inner world symmetric lens combinators
	readSym :: (K inc m r a) => Proxy inc -> SymLens (Inside inc m r) r (ReadCpl inc m r a) (M inc m r a) a
	writeSym :: (K inc m r a) => Proxy inc -> SymLens (Inside inc m r) r (ReadCpl inc m r a) a (M inc m r a)
	modSym :: (K inc m r a,K inc m r b,K inc m r c) => Proxy inc -> SymLens (Inside inc m r) r c a b -> SymLens (Inside inc m r) r (ModCpl inc m r a b c) (M inc m r a) (M inc m r b)

instance (MonadIO m,Ref m r,Box r m,Box r (Changeable m r)) => IncrementalBX Eager m r where
	type Inside Eager = Changeable
	type Outside Eager = Adaptive
	
	type M Eager = Modifiable
	
	type K Eager m r a = (Eq a)
	
	run l = Adaptive.run
	inside l = inCh
	get l = readMod
	change l m v = Adaptive.change m v >> Adaptive.propagate -- we run propagate after each change, to avoid having a specific primitive
	new l = newMod . return
	display l = displayAdaptive
	displayAs l = displayAdaptiveAs
	
	type ReadCpl Eager m r a = SymAdaptive.ReadCompl m r a
	type ModCpl Eager m r a b c = SymAdaptive.ModCompl m r a b c
	
	readSym l = SymAdaptive.readSym
	writeSym l = SymAdaptive.writeSym
	modSym l = SymAdaptive.modSym
	
instance (Typeable m,Typeable r,MonadIO m,Ref m r,Box r m,Box r (Inner m r)) => IncrementalBX Lazy m r where
	type Inside Lazy = Inner
	type Outside Lazy = Outer

	type M Lazy = U
	
	type K Lazy m r a = (Typeable m,Typeable r,Typeable a,Memo a)

	run l = BiAdapton.run
	inside l = BiAdapton.inner
	get l = BiAdapton.get
	change l = set
	new l = ref
	display l = displayOuter
	displayAs l = displayOuterAs
	
	type ReadCpl Lazy m r a = SymAdapton.ReadCompl m r a
	type ModCpl Lazy m r a b c = SymAdapton.ModCompl m r a b c
	
	readSym l = SymAdapton.readSym
	writeSym l = SymAdapton.writeSym
	modSym l = SymAdapton.modSym

displayAdaptive :: (Ref m r,MonadIO m, Display Eager m r a) => a -> Outside Eager m r ()
displayAdaptive x = inCh $ showL eager x >>= \str -> inM $ liftIO $ print str

displayAdaptiveAs :: (Ref m r,MonadIO m, Display Eager m r a) => String -> a -> Outside Eager m r ()
displayAdaptiveAs tag x = inCh $ showL eager x >>= \str -> inM $ liftIO $ print $ tag ++ ": " ++ str

displayOuter :: (Typeable m,Typeable r,Ref m r,MonadIO m, Display Lazy m r a) => a -> Outside Lazy m r ()
displayOuter x = inner $ showL lazy x >>= \str -> inL $ liftIO $ print str

displayOuterAs :: (Typeable m,Typeable r,Ref m r,MonadIO m, Display Lazy m r a) => String -> a -> Outside Lazy m r ()
displayOuterAs tag x = inner $ showL lazy x >>= \str -> inL $ liftIO $ print $ tag ++ ": " ++ str

class Display inc m r a where
	showL :: MonadIO m => Proxy inc -> a -> Inside inc m r String

instance (Display Eager m r a,K Eager m r a,Ref m r,MonadIO m) => Display Eager m r (Modifiable m r a) where
	showL l t = Adaptive.readMod t >>= showL l >>= \str -> return $ "<m" ++ show (refId $ value t) ++ " " ++ str ++ ">"

instance (Display Lazy m r a,K Lazy m r a,Ref m r,MonadIO m) => Display Lazy m r (U m r a) where
	showL l t = BiAdapton.get t >>= showL l >>= \str -> return $ "<u" ++ show (idU t) ++ " " ++ str ++ ">"

instance Monad (Inside inc m r) => Display inc m r (U1 p) where
	showL l U1 = return ""
instance (Monad (Inside inc m r),Display inc m r c) => Display inc m r (K1 i c p) where
	showL l (K1 c) = showL l c
instance (Monad (Inside inc m r),Display inc m r (f p),Constructor c) => Display inc m r (M1 C c f p) where
	showL l m1 = do
		str <- showL l (unM1 m1)
		return $ "(" ++ conName m1 ++ " " ++ str ++ ")"
instance (Monad (Inside inc m r),Display inc m r (f p)) => Display inc m r (M1 D c f p) where
	showL l  m1 = showL l (unM1 m1)
instance (Monad (Inside inc m r),Display inc m r (f p)) => Display inc m r (M1 S c f p) where
	showL l m1 = showL l (unM1 m1)
instance (Monad (Inside inc m r),Display inc m r (f p),Display inc m r (g p)) => Display inc m r ((f :+: g) p) where
	showL l (L1 x) = showL l x
	showL l (R1 x) = showL l x
instance (Monad (Inside inc m r),Display inc m r (f p),Display inc m r (g p)) => Display inc m r ((f :*: g) p) where
	showL l (x :*: y) = do
		str1 <- showL l x
		str2 <- showL l y
		return $ str1 ++ " " ++ str2
instance Monad (Inside inc m r) => Display inc m r Int where
	showL l i = return $ show i
instance Monad (Inside inc m r) => Display inc m r Char where
	showL l c = return $ show c

instance (Monad (Inside inc m r),Display inc m r a,Display inc m r b) => Display inc m r (a,b) where
	showL l (a,b) = do
		str1 <- showL l a
		str2 <- showL l b
		return $ "(" ++ str1 ++ "," ++ str2 ++ ")"
instance (Monad (Inside inc m r),Display inc m r a,Display inc m r b) => Display inc m r (Either a b) where
	showL l (Left a) = do
		str1 <- showL l a
		return $ "(Left " ++ str1 ++ ")"
	showL l (Right b) = do
		str2 <- showL l b
		return $ "(Right " ++ str2 ++ ")"
	




