{-# LANGUAGE UndecidableInstances, ConstraintKinds, DataKinds, QuasiQuotes, TemplateHaskell, DeriveGeneric, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, KindSignatures, ScopedTypeVariables, GADTs, BangPatterns, DeriveDataTypeable, StandaloneDeriving, FlexibleContexts #-}

module Main where

import qualified Control.Monad.BiAdapton as BiAdapton
import qualified Control.Monad.BiAdapton.DataTypes as BiAdaptonD
import Generics.Lenses.Symmetric.Language hiding (get)
import Data.IntMap.Lazy (IntMap(..))
import Data.IntMap.Lazy as IntMap
import Data.Map.Lazy (Map(..))
import Data.Map.Lazy as Map
import Control.Monad.Trans.Class
import Data.Typeable
import Control.Monad.State (State,MonadState,StateT)
import qualified Control.Monad.State as State
import System.Mem.MemoTable
import Data.STRef.Lazy
import Control.Monad.Trans
import Control.Monad.ST.Lazy
import Control.Monad.ST.Lazy.Unsafe
import qualified Generics.Lenses.Symmetric.Adaptive as SymAdaptive

import qualified Control.Monad.ST as StrictS
import qualified Control.Monad.ST.Unsafe as StrictSU
import qualified Data.STRef.Strict as Strict
import Control.Monad
import System.IO.Unsafe
import Unsafe.Coerce
import GHC.Generics
import Data.Generics.TH
import System.Mem.StableName
import Control.Monad.IncrementalBX
import Control.Monad.Box
import Data.IORef
import Control.Monad.Ref
import Control.Monad.ST.Lazy

-- * fst lens

type EXFstCpl (inc :: Inc) m r = ModCpl inc m r (Int,Char) Int (Int -> Char)

fstModSym :: (K inc m r (Int, Char),K inc m r Int,K inc m r (Int -> Char),IncrementalBX inc m r) => Proxy inc -> SymLens (Inside inc m r) r (EXFstCpl inc m r) (M inc m r (Int,Char)) (M inc m r Int)
fstModSym inc = modSym inc $ fstSym (return $ const 'x')

-- * fst <.> cond lens

type EXFstECpl (inc :: Inc) m r = ModCpl inc m r (Int,Char) (Either Int Int) (r (Int -> Char),r (Int -> Bool))

fstEModSym :: (K inc m r (Int, Char),K inc m r (Either Int Int),K inc m r (r (Int -> Char), r (Int -> Bool)),IncrementalBX inc m r) => Proxy inc -> SymLens (Inside inc m r) r (EXFstECpl inc m r) (M inc m r (Int,Char)) (M inc m r (Either Int Int))
fstEModSym inc = modSym inc $ fstSym (return $ const 'x') <.> condSym (return $ const True)

exFstELazy :: IO ()
exFstELazy = stToIO $ run lazy $ inside lazy (missing (fstEModSym lazy)) >>= State.evalStateT exFstELazy'

exFstELazy' :: StateT ((STRef RealWorld) (EXFstECpl Lazy (ST RealWorld) (STRef RealWorld))) (Outside Lazy (ST RealWorld) (STRef RealWorld)) ()
exFstELazy' = do
	tx <- lift $ new lazy $ (1::Int,'a')
	lift $ liftIO $ putStrLn "source"
--	lift $ display lazy tx
	ty <- State.mapStateT (inside lazy) $ putr (fstEModSym lazy) tx
	lift $ liftIO $ putStrLn "view"
--	lift $ display lazy ty
	
	tx <- State.mapStateT (inside lazy) $ putl (fstEModSym lazy) ty
	
	lift $ liftIO $ putStrLn "source0"
--	lift $ display lazy tx
	
--	lift $ BiAdaptonD.inL $ BiAdapton.printDependentsU BiAdaptonD.Forward "" tx
--	lift $ BiAdaptonD.inL $ BiAdapton.printDependentsU BiAdaptonD.Backward "" ty
	
	lift $ liftIO $ putStrLn "changing view"
	lift $ change lazy ty (Right 2)
--	lift $ display lazy ty
--	lift $ display lazy tx
	
--	lift $ BiAdaptonD.inL $ BiAdapton.printDependentsU BiAdaptonD.Forward "" tx
--	lift $ BiAdaptonD.inL $ BiAdapton.printDependentsU BiAdaptonD.Backward "" ty
	
	lift $ liftIO $ putStrLn "displaying new source"
--	lift $ display lazy tx
	
	lift $ liftIO $ putStrLn "changing source"
	lift $ change lazy tx (3,'c')
	lift $ liftIO $ putStrLn "changing source"
	lift $ change lazy tx (4,'d')
	lift $ liftIO $ putStrLn "new source 2"
	lift $ display lazy tx
	lift $ liftIO $ putStrLn "new view 2"
	lift $ display lazy ty
	
	return ()

-- * filter lens

type ComplMod2 inc m r a b = ModCpl inc m r (ListM' inc m r (Either a b)) (ListM' inc m r a) (ComplMod2' inc m r a b)
data ComplMod2' inc (m :: * -> *) r a b = CM2 (r ( r (Either (r ()) (r (ComplMod2'' inc m r a b))), (r (ListM' inc m r a -> Bool)) )  ) deriving (Generic,Typeable)
type ComplMod2'' inc (m :: * -> *) r a b = Either (r (ComplMod2 inc m r a b)) (r (r (r (ReadCpl inc m r (ListM' inc m r (Either a b))),r (ComplMod2' inc m r a b)),r b))

type EqComplMod2' m r a b = Eq (r (r (Either (r ()) (r (Either (r (SymAdaptive.ModCompl m r (ListM' Eager m r (Either a b)) (ListM' Eager m r a) (ComplMod2' Eager m r a b))) (r (r (r (SymAdaptive.ReadCompl m r (ListM' Eager m r (Either a b))), r (ComplMod2' Eager m r a b)), r b))))), r (ListM' Eager m r a -> Bool)))

deriving instance EqComplMod2' m r a b => Eq (ComplMod2' Eager m r a b)

instance (Typeable m,Typeable r,Typeable a,Typeable b) => Memo (ComplMod2' Lazy m r a b) where
	memoKey = MemoKey . unsafePerformIO . makeStableName

filterlModSym2 :: (K inc m r (ListM' inc m r a),K inc m r (ComplMod2' inc m r a b),K inc m r (ListM' inc m r (Either a b)),IncrementalBX inc m r) => Proxy inc -> b -> SymLens (Inside inc m r) r (ComplMod2 inc m r a b) (ListM inc m r (Either a b)) (ListM inc m r a)
filterlModSym2 inc def = modSym inc $ filterlModSym2' inc def

filterlModSym2' :: (K inc m r (ListM' inc m r a),K inc m r (ComplMod2' inc m r a b),K inc m r (ListM' inc m r (Either a b)),IncrementalBX inc m r) => Proxy inc -> b -> SymLens (Inside inc m r) r (ComplMod2' inc m r a b) (ListM' inc m r (Either a b)) (ListM' inc m r a)
filterlModSym2' inc def = liftSym (unboxLns >.> outLns) $ outSym <!.> (idSym <!+> distlSym) <!.> f <.> (coassoclSym <!.> (innSym <!+> idSym) <!.> switchSym (return $ const True))
	where f = idSym <++> (idSym <!*> (filterlModSym2 inc def) <++> (idSym <!*> (readSym inc <.> (filterlModSym2' inc def))) <.> sndSym' (return def))

exFilterLazy :: IO ()
exFilterLazy = stToIO $ run lazy $ inside lazy (missing (filterlModSym2 lazy 0)) >>= State.evalStateT exFilterLazy'

exFilterLazy' :: StateT ((STRef RealWorld) (ComplMod2 Lazy (ST RealWorld) (STRef RealWorld) Int Int)) (Outside Lazy (ST RealWorld) (STRef RealWorld)) ()
exFilterLazy' = do
	tx <- lift $ toListM lazy ([Right 1,Left 2] :: [Either Int Int])
--	lift $ displayAs lazy "source" tx
	ty <- State.mapStateT (inside lazy) $ putr (filterlModSym2 lazy 0) tx
--	lift $ displayAs lazy "view" ty
	
	tx <- State.mapStateT (inside lazy) $ putl (filterlModSym2 lazy 0) ty
	
--	lift $ displayAs lazy "source0" tx
	
	lift $ printListDependentsU BiAdaptonD.Forward tx
	lift $ printListDependentsU BiAdaptonD.Backward ty
	
	lift $ liftIO $ putStrLn "#########changing source"
	tailx <- lift $ tailOfListM lazy tx
	newtailx <- lift $ toListM' lazy [Right 3,Left 4,Left 6]
	lift $ change lazy tailx newtailx
--	lift $ displayAs lazy "new source" tx
--	lift $ displayAs lazy "new view" ty
	
--	lift $ printListDependentsU BiAdaptonD.Forward tx
--	lift $ printListDependentsU BiAdaptonD.Backward ty

	lift $ liftIO $ putStrLn "#########changing view"
	lasty <- lift $ lastOfListM lazy ty
	newlasty <- lift $ toListM' lazy [8,10]
	lift $ liftIO $ putStrLn "now"
	lift $ change lazy lasty newlasty
--	lift $ displayAs lazy "new view 2" ty
	lift $ displayAs lazy "new source 2" tx
	
	return ()

exFilterEager :: IO ()
exFilterEager = run eager $ inside eager (missing (filterlModSym2 eager (-1))) >>= State.evalStateT exFilterEager'

exFilterEager' :: StateT (IORef (ComplMod2 Eager IO IORef Int Int)) (Outside Eager IO IORef) ()
exFilterEager' = do
	s <- lift $ toListM eager [Right 1,Left 2]
	lift $ displayAs eager "source" s
	v <- State.mapStateT (inside eager) $ putr (filterlModSym2 eager (-1)) s
	lift $ displayAs eager "view" v
	
--	lift $ inM $ lift $ putStrLn "s0...."
--	lift $ listMods s
--	lift $ inM $ State.lift $ putStrLn "v0...."
--	lift $ listMods v

--	r <- State.get
--	c <- lift $ readRef r
--	lift $ inM $ lift $ putStrLn $ "complement: " ++ (show c)

	lift $ liftIO $ putStrLn "going backward..."
	s <- State.mapStateT (inside eager) $ putl (filterlModSym2 eager (-1)) v
	
--	lift $ inM $ lift $ putStrLn "s...."
--	lift $ listMods s
--	lift $ inM $ State.lift $ putStrLn "v...."
--	lift $ listMods v

	tailv <- lift $ tailOfListM eager v
	v' <- lift $ toListM' eager [6]
	lift $ liftIO $ putStrLn "############### Changing view tail ###############"
	lift $ change eager tailv v'
--	lift $ inM $ State.lift $ putStrLn "new source...."
--	lift $ listMods s
--	lift $ inM $ State.lift $ putStrLn "new view...."
--	lift $ listMods v
	
--	tailv <- lift $ tailOfListMod v
--	tailv' <- lift $ toListMod' [8,10]
--	lift $ inM $ State.lift $ putStrLn "Changing view tail"
--	lift $ inCh $ change tailv tailv'
--	lift $ propagate
--	lift $ inM $ State.lift $ putStrLn "new source2...."
----	lift $ listMods s
--	lift $ inM $ State.lift $ putStrLn "new view2...."
----	lift $ listMods v
--
--	tails <- lift $ tailOfListMod s
--	tails' <- lift $ toListMod' [Left 14,Right 15,Left 16]
--	lift $ inM $ State.lift $ putStrLn "Changing source tail"
--	lift $ change tails tails'
--	lift $ propagate
--	lift $ inM $ State.lift $ putStrLn "new2 source...."
----	lift $ listMods s
--	lift $ inM $ State.lift $ putStrLn "new2 view...."
----	lift $ listMods v

--	tails <- lift $ matchListMod (Left 2) s
--	tails' <- lift $ toListMod' [Left 10]
--	lift $ inM $ State.lift $ putStrLn "Changing source tail"
--	lift $ inChMb (sing :: Sing "get") $ change tails tails'
--	lift $ propagate
--	lift $ inM $ State.lift $ putStrLn "new2 source...."
--	lift $ listMods s
--	lift $ inM $ State.lift $ putStrLn "new2 view...."
--	lift $ listMods v
--
--	tailv <- lift $ secondOfListMod v
--	tailv' <- lift $ toListMod' [6]
--	lift $ inM $ State.lift $ putStrLn "Changing view tail"
--	lift $ inChMb (sing :: Sing "put") $ change tailv tailv'
--	lift $ propagate
--	lift $ inM $ State.lift $ putStrLn "new source...."
----	lift $ listMods s
--	lift $ inM $ State.lift $ putStrLn "new view...."
----	lift $ listMods v

--	tails <- lift $ tailOfListMod s
--	tails' <- lift $ toListMod' [Left 14]
--	lift $ inM $ State.lift $ putStrLn "Changing source tail"
--	lift $ inChMb (sing :: Sing "get") $ change tails tails'
--	lift $ propagate
--	lift $ inM $ State.lift $ putStrLn "new2 source...."
--	lift $ listMods s
--	lift $ inM $ State.lift $ putStrLn "new2 view...."
--	lift $ listMods v

	return ()

-- * Lists with modifiable tails

data ListM' inc m r a = NilM | ConsM a (ListM inc m r a) deriving (Generic,Typeable)
type ListM inc m r a = M inc m r (ListM' inc m r a)
	
deriving instance (Eq a,Ref m r) => Eq (ListM' Eager m r a)

instance (Typeable inc,Typeable m,Typeable r,Typeable a) => Memo (ListM' inc m r a) where
	memoKey = MemoKey . unsafePerformIO . makeStableName

instance (Monad (Inside inc m r),Typeable a,Display inc m r a,Display inc m r (M inc m r (ListM' inc m r a))) => Display inc m r (ListM' inc m r a) where
	showL inc = showL inc . from

toListM :: (K inc m r (ListM' inc m r a),Typeable a,IncrementalBX inc m r) => Proxy inc -> [a] -> Outside inc m r (ListM inc m r a)
toListM inc xs = toListM' inc xs >>= new inc
	
toListM' :: (K inc m r (ListM' inc m r a),Typeable a,IncrementalBX inc m r) => Proxy inc -> [a] -> Outside inc m r (ListM' inc m r a)
toListM' inc [] = return NilM
toListM' inc (x:xs) = do
	mxs <- toListM inc xs
	return $ ConsM x mxs

tailOfListM :: (K inc m r (ListM' inc m r a),Typeable a,IncrementalBX inc m r) => Proxy inc -> ListM inc m r a -> Outside inc m r (ListM inc m r a)
tailOfListM inc mxs = get inc mxs >>= \xs -> case xs of
	NilM -> return mxs
	ConsM x mxs' -> tailOfListM inc mxs'

lastOfListM :: (K inc m r (ListM' inc m r a),Typeable a,IncrementalBX inc m r) => Proxy inc -> ListM inc m r a -> Outside inc m r (ListM inc m r a)
lastOfListM inc mxs = get inc mxs >>= \xs -> case xs of
	ConsM x mxs' -> get inc mxs' >>= \xs' -> case xs' of
		NilM -> return mxs
		otherwise -> lastOfListM inc mxs'

deriving instance Typeable ST

instance MonadIO (ST RealWorld) where
	liftIO = unsafeIOToST

printListDependentsU :: (MonadIO m,Memo a,Typeable a,BiAdapton.Layer BiAdaptonD.Outer m r) => BiAdaptonD.Direction -> ListM Lazy m r a -> BiAdaptonD.Outer m r ()
printListDependentsU dir mxs = do
	BiAdaptonD.inL $ BiAdapton.printDependentsU dir "ROOT" mxs
	BiAdapton.get mxs >>= \xs -> case xs of
		NilM -> return ()
		ConsM x mxs' -> printListDependentsU dir mxs'
