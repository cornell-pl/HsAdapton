{-# LANGUAGE KindSignatures, ExistentialQuantification, DataKinds, FlexibleInstances, DeriveGeneric, FlexibleContexts #-}

module Main where

import Generics.Lenses.Symmetric.Language
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State (State(..),StateT(..),MonadState)
import qualified Control.Monad.State as State
import GHC.Generics
import Generics.Lenses.Symmetric.Adaptive
import Control.Monad.Adaptive
import Control.Monad.Adaptive.Ref
import GHC.TypeLits
import Control.Monad.Adaptive.CircularList hiding (get)
import Control.Monad.Adaptive.OrderedList hiding (inM,run)

import Debug.Trace
import System.IO.Unsafe
import Data.Maybe
import Control.Monad.Identity
import Data.IORef

import Control.Monad.Adaptive.Ref

instance Show (IORef a) where
	show r = "ioref"

instance Show (a -> b) where
	show x = "f"

data MapCpl k c = MapCpl (k (Either (k ()) (k (k c,k (MapCpl k c))))) deriving Generic

mapSym :: Box k m => SymLens m k c a b -> SymLens m k (MapCpl k c) [a] [b]
mapSym l = liftSym (unboxLns >.> outLns) $ outSym <!.> (idSym <++> l <*> mapSym l) <.!> innSym


----composersSym :: Monad m => SymLens (ComposerState m) [ComposerCpl] [ComposerYY] [ComposerC]
----composersSym = mapSym composerSym
--
--
--
--composerSym :: Monad m => SymLens (ComposerState m) k ComposerCpl ComposerYY ComposerC
--composerSym = fstEqSym f <.> addSndEqSym g where
--	f :: ComposerState m (Name -> ComposerState m (Year,Year))
--	f = undefined --State.get >>= \m -> return (fst . m)
--	g :: ComposerState m (Name -> ComposerState m Country)
--	g = undefined
--
--type ComposerState = StateT (Name -> ((Year,Year),Country))
--type ComposerCpl = (Maybe (Name,(Year,Year)),Maybe (Name,Country))
--type ComposerYY = (Name,(Year,Year))
--type ComposerC = (Name,Country)
--type Name = String
--type Year = Int
--type Country = String

--
--data MapCpl m a b c = MapCpl (ModCompl m ShowRef (ListMod' m a) (ListMod' m b) (Either () (c,MapCpl m a b c))) deriving (Show,Generic)
--
--mapModSym :: (Show c,Show a,Show b,Eq a,Eq b,Ref m ShowRef,Monad m) => SymLens (Changeable m ShowRef) c a b -> SymLens (Changeable m ShowRef) (MapCpl m a b c) (ListMod m a) (ListMod m b)
--mapModSym l = liftSym outLns $ modSym $ outSym <!.> (idSym <++> l <*> mapModSym l) <.!> innSym
--
--data Stream a = SCons a (Stream a) deriving (Show,Generic)
--
--mapSym' :: Monad m => SymLens m c a b -> SymLens m (Stream c) [a] [b]
--mapSym' l = liftSym outLns $ outSym <!.> (idSym <!+> l <*> mapSym' l) <.!> innSym
--
--data Compl a b = Compl (Either () (Compl a b,([a] -> b)),[a] -> Bool) deriving (Show,Generic)
--
--filterSym :: (Show a,Enum a,Monad m) => SymLens m (Compl a a) [Either a a] [a]
--filterSym = liftSym outLns $ (outSym <!.> f <.!> coassoclSym <.!> (innSym <!+> idSym)) <.> switchSym (return h)
--	where f = idSym <++> ((idSym <!*> filterSym) <.!> distlSym) <.> (idSym <!+> sndSym (return g))
--	      g (x:xs) = pred x
--	      h [] = True
--	      h (x:xs) = trace (show x) $ True
--
--ex :: IO ()
--ex = missing filterSym >>= State.evalStateT ex'
--
--ex' :: StateT (Compl Int Int) IO ()
--ex' = do
----	State.get >>= lift . print
--	lift $ putStrLn "forward:"
--	v <- putr filterSym [Right 1,Left 2,Right 3,Left 4]
--	lift $ print v
----	State.get >>= lift . print
--	lift $ putStrLn "backward:"
--	s <- putl filterSym [0,2]
--	lift $ print s
----	State.get >>= lift . print
--	return ()
--
--type ComplMod m a b = ModCompl m ShowRef (ListMod' m (Either a b)) (ListMod' m a) (ComplMod' m a b) --((ListMod m (Either a b),ListMod m a),ComplMod' m r a b) 
--type ComplMod'' m a b = (ReadCompl m ShowRef (ListMod' m (Either a b)),ComplMod' m a b) 
--data ComplMod' m a b = CM (Either () (Either (ComplMod m a b) (ComplMod'' m a b,ListMod' m a -> b)),ListMod' m a -> Bool) deriving (Show,Generic)
--
--filterlModSym :: (Show a,Show b,Eq a,Eq b,Monad m,Ref m ShowRef) => SymLens (Changeable m ShowRef) (ComplMod m a b) (ListMod m (Either a b)) (ListMod m a)
--filterlModSym = modSym $ filterlModSym'
--
--filterlModSym' :: (Show a,Show b,Eq a,Eq b,Monad m,Ref m ShowRef) => SymLens (Changeable m ShowRef) (ComplMod' m a b) (ListMod' m (Either a b)) (ListMod' m a)
--filterlModSym' = liftSym outLns $ outSym <!.> (idSym <!+> distlSym) <!.> f <.> (coassoclSym <!.> (innSym <!+> idSym) <!.> switchSym (return $ const True))
--	where f = idSym <++> (idSym <!*> filterlModSym <++> (idSym <!*> (readSym <.> filterlModSym')) <.> sndSym (return undefined))
--
type ComplMod2 m a b = ModCompl m ShowRef MyBox (ListMod' m (Either a b)) (ListMod' m a) (ComplMod2' m a b) 
data ComplMod2' (m :: * -> *) a b = CM2 (MyBox ( MyBox (Either (MyBox ()) (MyBox (ComplMod2'' m a b))), (MyBox (ListMod' m a -> Bool)) )  ) deriving (Show,Generic)
type ComplMod2'' (m :: * -> *) a b = Either (MyBox (ComplMod2 m a b)) (MyBox (MyBox (MyBox (ReadCompl m ShowRef (ListMod' m (Either a b))),MyBox (ComplMod2' m a b)),MyBox b))

type MyBox = ShowRef

filterlModSym2 :: (Show a,Show b,Eq a,Eq b,Monad m,Ref m ShowRef) => b -> SymLens (Changeable m ShowRef) MyBox (ComplMod2 m a b) (ListMod m (Either a b)) (ListMod m a)
filterlModSym2 def = modSym $ filterlModSym2' def

filterlModSym2' :: (Show a,Show b,Eq a,Eq b,Monad m,Ref m ShowRef) => b -> SymLens (Changeable m ShowRef) MyBox (ComplMod2' m a b) (ListMod' m (Either a b)) (ListMod' m a)
filterlModSym2' def = liftSym (unboxLns >.> outLns) $ outSym <!.> (idSym <!+> distlSym) <!.> f <.> (coassoclSym <!.> (innSym <!+> idSym) <!.> switchSym (return $ const True))
	where f = idSym <++> (idSym <!*> (filterlModSym2 def) <++> (idSym <!*> (readSym <.> (filterlModSym2' def))) <.> sndSym' (return def))

exMod :: IO ()
exMod = State.evalStateT (run $ inCh (missing (filterlModSym2 (-1))) >>= State.evalStateT exMod') 0

--exMod' :: StateT (FilterCpl (StateT Int IO) ShowRef Int) (Adaptive (StateT Int IO) ShowRef) ()
exMod' :: StateT (MyBox (ComplMod2 (StateT Int IO) Int Int)) (Adaptive (StateT Int IO) ShowRef) ()
exMod' = do
	s <- lift $ toListMod [Right 1,Left 2]
	lift $ printListMod "source" s
	v <- State.mapStateT (inCh) $ putr (filterlModSym2 (-1)) s
	lift $ printListMod "view" v
	
	lift $ inM $ lift $ putStrLn "s0...."
	lift $ listMods s
	lift $ inM $ State.lift $ putStrLn "v0...."
	lift $ listMods v

--	r <- State.get
--	c <- lift $ readRef r
--	lift $ inM $ lift $ putStrLn $ "complement: " ++ (show c)

	lift $ inM $ State.lift $ putStrLn "going backward..."
	s <- State.mapStateT (inCh) $ putl (filterlModSym2 (-1)) v
	
	lift $ inM $ lift $ putStrLn "s...."
	lift $ listMods s
	lift $ inM $ State.lift $ putStrLn "v...."
	lift $ listMods v

	tailv <- lift $ tailOfListMod v
	v' <- lift $ toListMod' [6]
	lift $ inM $ State.lift $ putStrLn "############### Changing view tail ###############"
	lift $ change tailv v'
	lift $ propagate
	lift $ inM $ State.lift $ putStrLn "new source...."
	lift $ listMods s
	lift $ inM $ State.lift $ putStrLn "new view...."
	lift $ listMods v
	
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

filterMod2 :: (Show a,Eq a,Ref m ShowRef,Monad m) => ListMod m (Either a b) -> Changeable m ShowRef (ListMod m a)
filterMod2 mxs = newMod $ readMod mxs >>= filterMod2'

filterMod2' :: (Show a,Eq a,Ref m ShowRef,Monad m) => ListMod' m (Either a b) -> Changeable m ShowRef (ListMod' m a)
filterMod2' xs = case xs of
	ConsMod (Left x) mxs' -> liftM (ConsMod x) $ filterMod2 mxs'
	ConsMod (Right x) mxs' -> readMod mxs' >>= \xs' -> filterMod2' xs'
	NilMod -> return NilMod

ex22 :: IO ()
ex22 = State.evalStateT (run exR22) 0

exR22 :: Adaptive (StateT Int IO) ShowRef ()
exR22 = do
	s <- toListMod [Right 1,Left 2]
	printListMod "source" s
	v <- inCh $ filterMod2 s
	printListMod "view" v
	
	inM $ lift $ putStrLn "s...."
	listMods s
	inM $ State.lift $ putStrLn "v...."
	listMods v
--	
--	tails <- secondOfListMod s
--	tails' <- toListMod' [Right 5,Left 6]
--	inChMb (sing :: Sing "get") $ change tails tails'
--	propagate
--	
--	inM $ lift $ putStrLn "s2...."
--	listMods s
--	inM $ State.lift $ putStrLn "v2...."
--	listMods v
--	

type EXC m = ModCompl m ShowRef ShowRef Int (Either Int Int) (Int -> Bool)

readModEx :: (Monad m,Ref m ShowRef) => SymLens (Changeable m ShowRef) ShowRef (EXC m) (Modifiable m ShowRef Int) (Modifiable m ShowRef (Either Int Int))
readModEx = modSym $ condSym (return $ const True)

exRead :: IO ()
exRead = State.evalStateT (run $ inCh (missing (readModEx)) >>= State.evalStateT exRead') 0

exRead' :: StateT (ShowRef (EXC (StateT Int IO))) (Adaptive (StateT Int IO) ShowRef) ()
exRead' = do
	s <- lift $ toMod 1
	lift $ printM "source" s
	v <- State.mapStateT (inCh) $ putr (readModEx) s
	lift $ printM "view" v

	s <- State.mapStateT (inCh) $ putl (readModEx) v
	
	lift $ inM $ State.lift $ putStrLn "Changing view"
	lift $ change v (Right 2)
	lift $ propagate
	
	return ()

----type DummyCpl m r =	((Modifiable m r (Modifiable m r (Int, Char)),
----	                           Modifiable m r Int),
----	                          r (ModCompl m r (Int, Char) Int (Int -> Char), ReadCompl m r Int))
--
----dummySym :: Ref m ShowRef => SymLens (Changeable m ShowRef) (DummyCpl m ShowRef) (Modifiable m ShowRef (Modifiable m ShowRef (Int,Char))) (Modifiable m --ShowRef Int)
----dummySym = modSym (modSym (fstSym $ return $ const ' ') <.> readSym)
--
----exMod2 :: IO ()
----exMod2 = State.evalStateT (run $ inChL (sing :: Sing "*") (missing (dummySym)) >>= State.evalStateT exMod2') 0
----
----exMod2' :: StateT (DummyCpl (StateT Int IO) ShowRef) (Adaptive (StateT Int IO) ShowRef) ()
----exMod2' = do
----	s <- lift $ doubleMod (1,'a')
----	lift $ printDoubleMod "source" s
----	v <- State.mapStateT (inCh . runChangeable (sing :: Sing "get")) $ putr (dummySym) s
----	lift $ printMod "view" v
----
----	s <- State.mapStateT (inCh . runChangeable (sing :: Sing "put")) $ putl (dummySym) v
----	
----	lift $ inM $ lift $ putStrLn "s...."
----	lift $ doubleMods s
----	lift $ inM $ State.lift $ putStrLn "v...."
----	lift $ doubleMods' v
----
----	tails <- lift $ innerMod s
----	tails' <- lift $ return (2,'b')
----	lift $ inM $ State.lift $ putStrLn "Changing source"
----	lift $ inChMb (sing :: Sing "get") $ change tails tails'
----	lift $ propagate
----	lift $ inM $ State.lift $ putStrLn "new2 source...."
----	lift $ doubleMods s
----	lift $ inM $ State.lift $ putStrLn "new2 view...."
----	lift $ doubleMods' v
----	
----	lift $ inM $ State.lift $ putStrLn "Changing view"
----	lift $ inChMb (sing :: Sing "put") $ change v 3
----	lift $ propagate
----	lift $ inM $ State.lift $ putStrLn "new3 source...."
----	lift $ doubleMods s
----	lift $ inM $ State.lift $ putStrLn "new3 view...."
----	lift $ doubleMods' v

----type SumCpl m r a = ModCompl m r (ListMod m a) (Modifiable m r a) (SumCpl' m r a)
----data SumCpl' m r a = SumCpl' (Either a (a,(ReadCompl m r (ListMod m a),SumCpl' m r a))) (a -> Bool)
----
----sumSym :: (Eq a,Monad m,Ref m r) => SymLens (Changeable m r) (SumCpl m r a) (ListMod m a) (Modifiable m r a)
----sumSym = modSym sumSym'
----
----sumSym' :: (Eq a,Monad m,Ref m r) => SymLens (Changeable m r) (SumCpl' m r a) (ListMod' m a) a
----sumSym' = undefined --modSym sumSym''
--
----sumSym'' :: (Eq a,Monad m,Ref m r) => SymLens (StateT a (Changeable m r)) (SumCpl' m r a) (ListMod' m a) a
----sumSym'' = liftSym outLns $ outSym <!.> (getStateSym <++> (modifyStateWithSym (+) <*> (readSym <.> sumSym'')) <.!> remfstOneSym) <.> switchSym (return (==0))
--
--data Cp a b = CpNil | CpFun (Cp a b) (((a,[a]),(b,[b])) -> Bool) deriving (Show,Generic)
--	
--partitionSym :: (Monad m) => SymLens m (Cp a b) [Either a b] ([a],[b])
--partitionSym = liftSym outLns $ outSym <!.> (addfstOneSym <++> ((idSym <!*> partitionSym) <.!> distlSym) <.> f) <.!> coassoclSym <.!> undistsSym <.!> (innSym <!*> innSym)
--	where f = (assoclSym <!.> (idSym <!*> outSym) <!.> distrSym <!+> subrSym <!.> (outSym <!*> idSym) <.!> distlSym) <!.> coassocrSym <!.> (idSym <!+> g) <.!> cosubrSym
--	      g = cosubrSym <!.> (idSym <!+> switchSym (return $ const False))
--
--data Cpl a b = CplNil (() -> [b]) | CplCons (a -> b,Cpl a b) ((a,[a]) -> Bool) deriving (Show,Generic)
--
--fstListSym :: (Monad m,Enum a) => SymLens m (Cpl a a) ([a],[a]) [a]
--fstListSym = liftSym outLns $ (outSym <!*> idSym) <!.> distlSym <!.> (idSym <!+> (idSym <!*> outSym) <!.> distrSym) <!.> (f <++> g) <.!> innSym
--	where f = fstSym (return $ const [])
--	      g = (remsndOneSym <!+> distpSym <!.> (fstSym (return pred) <*> fstListSym)) <.> switchSym (return $ const False)
--
--ex1 :: IO ()
--ex1 = missing fstListSym >>= State.evalStateT ex1'
--
--ex1' :: StateT (Cpl Int Int) IO ()
--ex1' = do
--	State.get >>= lift . print
--	lift $ putStrLn "forward:"
--	v <- putr fstListSym ([2,4],[1,3])
--	lift $ print v
--	State.get >>= lift . print
--	lift $ putStrLn "backward:"
--	s <- putl fstListSym [2,4,6,8]
--	lift $ print s
--	State.get >>= lift . print
--	return ()
--
--type FC a = ([a -> Bool],(Cp a a,[a] -> [a]))
--
--filterSym2 :: (Integral a,Enum a,Monad m) => SymLens m (FC a) [a] [a]
--filterSym2 = mapSym (condSym $ return even) <.> partitionSym <.> fstSym (return $ const [])
--
--type FC3 a = ([a -> Bool],(Cp a a,Cpl a a))
--
--filterSym3 :: (Integral a,Enum a,Monad m) => SymLens m (FC3 a) [a] [a]
--filterSym3 = mapSym (condSym $ return even) <.> partitionSym <.> fstListSym
--
--ex2 :: IO ()
--ex2 = missing filterSym2 >>= State.evalStateT ex2'
--
--ex2' :: StateT (FC Int) IO ()
--ex2' = do
--	State.get >>= lift . print
--	lift $ putStrLn "forward:"
--	v <- putr filterSym2 [1,2,3,4]
--	lift $ print v
--	State.get >>= lift . print
--	lift $ putStrLn "backward:"
--	s <- putl filterSym2 [2,4,6,8]
--	lift $ print s
--	State.get >>= lift . print
--	return ()
--
--doubleMods :: (NewMod n (StateT Int IO) ShowRef,Monad (n (StateT Int IO) ShowRef),InM n,Show a)
--         => Modifiable (StateT Int IO) ShowRef (Modifiable (StateT Int IO) ShowRef a) -> n (StateT Int IO) ShowRef ()
--doubleMods mxs = readMod mxs >>= \xs -> do
--	printdMod mxs
--	doubleMods' xs
--
--printdMod :: (InM n,NewMod n (StateT Int IO) ShowRef,Show a)
--             => Modifiable (StateT Int IO) ShowRef (Modifiable (StateT Int IO) ShowRef a) -> n (StateT Int IO) ShowRef ()
--printdMod mxs@(Node r _ mes) = do
--	inM $ State.lift $ putStr $ show r
--	inM $ State.lift $ putStr "  "
--	l <- chIn $ fromDoubleMod mxs
--	inM $ State.lift $ putStrLn $ show l
--	inM $ readRef mes >>= edgesTimes >>= State.lift . putStrLn
--	
--printsMod :: (InM n,NewMod n (StateT Int IO) ShowRef,Show a)
--             => Modifiable (StateT Int IO) ShowRef a -> n (StateT Int IO) ShowRef ()
--printsMod mxs@(Node r _ mes) = do
--	inM $ State.lift $ putStr $ show r
--	inM $ State.lift $ putStr "  "
--	l <- chIn $ fromMod mxs
--	inM $ State.lift $ putStrLn $ show l
--	inM $ readRef mes >>= edgesTimes >>= State.lift . putStrLn
--
--doubleMods' :: (NewMod n (StateT Int IO) ShowRef,Monad (n (StateT Int IO) ShowRef),InM n,Show a)
--          => Modifiable (StateT Int IO) ShowRef a -> n (StateT Int IO) ShowRef ()
--doubleMods' m = readMod m >>= \x -> do
--	printsMod m
--
---- modifiable list operations

instance (Ref IO r,Show a,RefId r) => Show (Modifiable m r a) where
	show (Node r wr es _) = "<mod " ++ show (refId r) ++ " " ++ unsafePerformIO (liftM (show) $ readRef r) ++ ">"

doubleMod :: (Ref IO r,Show a,NewMod m' m r,Eq a) => a -> m' m r (Modifiable m r (Modifiable m r a))
doubleMod x = newMod $ inAd $ newMod $ inAd $ return x

fromDoubleMod :: (Ref m r) => Modifiable m r (Modifiable m r a) -> Changeable m r a
fromDoubleMod mmx = readMod mmx >>= \mx -> readMod mx

toMod :: (Show a,NewMod m' m r,Eq a) => a -> m' m r (Modifiable m r a)
toMod x = newMod $ inAd $ return x

fromMod :: (Ref m r) => (Modifiable m r a) -> Changeable m r a
fromMod mx = readMod mx

printMod str mx = newMod $ fromMod mx >>= inM . State.lift . putStrLn . ((str++": ")++) . (++"\n") . show

printDoubleMod str mmx = newMod $ fromDoubleMod mmx >>= inM . State.lift . putStrLn . ((str++": ")++) . (++"\n") . show
	

data ListMod' m a = NilMod | ConsMod a (ListMod m a) deriving (Show,Eq,Generic)
type ListMod m a = Modifiable m ShowRef (ListMod' m a)

toListMod' :: (Show a,NewMod m' m ShowRef,Eq a) => [a] -> m' m ShowRef (ListMod' m a)
toListMod' [] = return NilMod
toListMod' (x:xs) = return (ConsMod x) `ap` toListMod xs

toListMod :: (Show a,NewMod m' m ShowRef,Eq a) => [a] -> m' m ShowRef (ListMod m a)
toListMod = newMod . inAd . toListMod'

fromListMod :: (Ref m ShowRef) => ListMod m a -> Changeable m ShowRef [a]
fromListMod mxs = readMod mxs >>= fromListMod'

fromListMod' :: (Ref m ShowRef) => ListMod' m a -> Changeable m ShowRef [a]
fromListMod' NilMod = return []
fromListMod' (ConsMod x mxs) = return (:) `ap` (return x) `ap` fromListMod mxs

printM :: (Show a,NewMod n (StateT Int IO) ShowRef) => String -> Modifiable (StateT Int IO) ShowRef a -> n (StateT Int IO) ShowRef (Modifiable (StateT Int IO) ShowRef ())
printM str m = newMod $ readModC m >>= \x -> inM $ State.lift $ putStrLn $ str ++ ": " ++ show x

printV :: (Show a,NewMod n (StateT Int IO) ShowRef) => a -> n (StateT Int IO) ShowRef (Modifiable (StateT Int IO) ShowRef ())
printV x = newMod $ (inM $ State.lift $ putStrLn $ show x :: Changeable (StateT Int IO) ShowRef ())

printListMod :: (Show a, NewMod n (StateT Int IO) ShowRef) => String -> ListMod (StateT Int IO) a -> n (StateT Int IO) ShowRef (Modifiable (StateT Int IO) ShowRef ())
printListMod str l = newMod $ fromListMod l >>= inM . State.lift . putStrLn . ((str++": ")++) . (++"\n") . show

printListMod' :: (Show a, NewMod n (StateT Int IO) ShowRef) => String -> ListMod' (StateT Int IO) a -> n (StateT Int IO) ShowRef (Modifiable (StateT Int IO) ShowRef ())
printListMod' str l = newMod $ fromListMod' l >>= inM . State.lift . putStrLn . ((str++": ")++) . (++"\n") . show

edgesTimes :: (InOL n,InM n,Ref m r,EqRef r,NewMod n m r) => [Recomp m r] -> n m r String
edgesTimes [] = return ""
edgesTimes (e:es) = do
	str <- edgeTimes e
	str' <- edgesTimes es
	return $ str ++ " ,\n" ++ str'

edgeTimes :: (InOL n,InM n,Ref m r,EqRef r,NewMod n m r) => Recomp m r -> n m r String
edgeTimes (Edge ori dest reader timespan) = do
	isSplicedOut <- inOL (isSplicedOut (start timespan))
	(_,starttime,_) <- inM $ val (start timespan)
	(_,stoptime,_) <- inM $ val (stop timespan)
	if isSplicedOut
		then trace ("deleted "++show ori ++"@"++ show starttime++" "++ show (dest)++"@"++ show stoptime) $ return ""
		else return $ show ori ++ "@"++show starttime ++ " --> " ++ show ( dest) ++ "@"++ show stoptime

printlistMod :: (InM n,NewMod n (StateT Int IO) ShowRef,Show a)
             => ListMod (StateT Int IO) a -> n (StateT Int IO) ShowRef ()
printlistMod mxs@(Node r _ mes _) = do
	inM $ State.lift $ putStr $ show r
	inM $ State.lift $ putStr "  "
	l <- chIn $ fromListMod mxs
	inM $ State.lift $ putStrLn $ show l
	inM (readRef mes) >>= edgesTimes >>= inM . State.lift . putStrLn

listMods :: (NewMod n (StateT Int IO) ShowRef,Monad (n (StateT Int IO) ShowRef),InM n,Show a)
         => ListMod (StateT Int IO) a -> n (StateT Int IO) ShowRef ()
listMods mxs = readMod mxs >>= \xs -> do
	printlistMod mxs
	listMods' xs

listMods' :: (NewMod n (StateT Int IO) ShowRef,Monad (n (StateT Int IO) ShowRef),InM n,Show a)
          => ListMod' (StateT Int IO) a -> n (StateT Int IO) ShowRef ()
listMods' NilMod = return ()
listMods' (ConsMod x mxs) = do
	listMods mxs

tailOfListMod :: (Ref m ShowRef,NewMod n m ShowRef) => ListMod m a -> n m ShowRef (ListMod m a)
tailOfListMod mxs = readMod mxs >>= \xs -> case xs of
	NilMod -> return mxs
	ConsMod _ mxs' -> tailOfListMod mxs'

lastOfListMod :: (Ref m ShowRef,NewMod n m ShowRef) => ListMod m a -> n m ShowRef (ListMod m a)
lastOfListMod mxs = readMod mxs >>= \xs -> case xs of
	NilMod -> return mxs
	ConsMod _ mxs' -> readMod mxs' >>= \xs' -> case xs' of
		NilMod -> return mxs
		ConsMod _ mxs'' -> lastOfListMod mxs'

innerMod :: (Eq a,Ref m r,NewMod n m r) => Modifiable m r (Modifiable m r a) -> n m r (Modifiable m r a)
innerMod mx = readMod mx

matchListMod :: (Eq a,Ref m ShowRef,NewMod n m ShowRef) => a -> ListMod m a -> n m ShowRef (ListMod m a)
matchListMod i mxs = readMod mxs >>= \xs -> case xs of
	NilMod -> error "matchListMod"
	ConsMod x mxs' -> if x == i
		then return mxs
		else matchListMod i mxs'

secondOfListMod :: (Ref m ShowRef,NewMod n m ShowRef) => ListMod m a -> n m ShowRef (ListMod m a)
secondOfListMod mxs = readMod mxs >>= \xs -> case xs of
	NilMod -> error ""
	ConsMod _ mxs' -> return mxs'