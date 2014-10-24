{-# LANGUAGE TypeFamilies, BangPatterns, Rank2Types, GADTs, DeriveDataTypeable, AllowAmbiguousTypes, MultiParamTypeClasses, TemplateHaskell, ScopedTypeVariables, KindSignatures, ConstraintKinds, FlexibleContexts, StandaloneDeriving, FlexibleInstances, UndecidableInstances #-}

module Main where

import System.Random
import Control.Monad.Incremental hiding (LiftInc(..))
import Control.Monad.Incremental.Draw
import Control.Monad.Incremental.Display
import Control.Monad.Incremental.LazyNonInc
import Control.Monad.Incremental.Adapton hiding (MData)
import qualified Control.Monad.Incremental.Adapton as Adapton
import Data.Proxy
import System.Mem.MemoTable (Memo(..))
import Control.Monad.IO.Class
import Control.Monad
import Data.IORef
import Data.DeriveTH
import Data.WithClass.Derive.MData
import Data.WithClass.MData
import Data.WithClass.MGenerics.Aliases
import Control.Concurrent
import Prelude hiding (mod,const)
import qualified Prelude
import Control.Monad.Trans
import Data.Unique
import System.Mem.WeakRef as WeakRef
import System.Mem.Weak as Weak
import System.Mem.WeakTable as WeakTable

import Test.QuickCheck.Gen
import Test.QuickCheck
import System.IO.Unsafe
import Data.Unique
import System.Mem.StableName
import System.TimeIt

--import Blaze.ByteString.Builder
--import qualified Data.ByteString.Lazy.Char8 as B
--import Blaze.ByteString.Builder.Internal
--import Blaze.ByteString.Builder.Char8
import Data.Monoid
import Data.Hashable
import System.Mem
import Control.Monad.Incremental.Generics
import Control.Monad.Incremental.List hiding (JoinListMod'(..))
import Control.Monad.Incremental.Tree

--import Criterion.Main hiding (run)
--import Criterion.Types

import Debug

-- * Generic modifiable lists and list operations

-- creates an input list from a regular list
toListRefInside :: (
	Eq a,Eq (ListMod mod Inside inc r m a),Input mod Inside inc r m,Layer l inc r m) => [a] -> l inc r m (ListMod mod Inside inc r m a)
toListRefInside xs = inside $ toListRef xs

toListRef :: (
	Eq a,Eq (ListMod mod l inc r m a),Input mod l inc r m) => [a] -> l inc r m (ListMod mod l inc r m a)
toListRef [] = ref NilMod
toListRef (x:xs) = do
	mxs <- toListRef xs
	ref (ConsMod x mxs)

toTreeRef :: (
	Eq a,Eq (TreeMod mod l inc r m a),Input mod l inc r m) => Tree a -> l inc r m (TreeMod mod l inc r m a)
toTreeRef Empty = ref EmptyMod
toTreeRef (Bin x l r) = do
	ml <- toTreeRef l
	mr <- toTreeRef r
	ref (BinMod x ml mr)
	
toListModInside :: (
	Eq a,Eq (ListMod mod Inside inc r m a),Input mod Inside inc r m,Layer l inc r m) => [a] -> l inc r m (ListMod mod Inside inc r m a)
toListModInside xs = inside $ toListMod xs

toListMod :: (
	Eq a,Eq (ListMod mod l inc r m a),Input mod l inc r m) => [a] -> l inc r m (ListMod mod l inc r m a)
toListMod [] = ref NilMod
toListMod (x:xs) = mod $ do
	mxs <- toListMod xs
	return $ ConsMod x mxs

modifyListModAt :: (Eq a,Eq (ListMod mod l inc r m a),Layer Outside inc r m,Input mod l inc r m)
	=> ListMod mod l inc r m a -> Int -> (ListMod' mod l inc r m a -> l inc r m (ListMod' mod l inc r m a)) -> Outside inc r m ()
modifyListModAt mxs 0 f = modify mxs f
modifyListModAt mxs i f = getOutside mxs >>= \xs -> case xs of
	ConsMod x mxs -> modifyListModAt mxs (pred i) f
	NilMod -> error "position not found"

setListModAt :: (Eq a,Eq (ListMod mod l inc r m a),Input mod l inc r m,Layer Outside inc r m)
	=> ListMod mod l inc r m a -> Int -> (ListMod' mod l inc r m a -> Outside inc r m (ListMod' mod l inc r m a)) -> Outside inc r m ()
setListModAt mxs 0 f = getOutside mxs >>= f >>= set mxs
setListModAt mxs i f = getOutside mxs >>= \xs -> case xs of
	ConsMod x mxs -> setListModAt mxs (pred i) f
	NilMod -> error "position not found"

setListModHeadAt :: (Eq a,Eq (ListMod mod l inc r m a),Input mod l inc r m,Layer Outside inc r m)
	=> ListMod mod l inc r m a -> Int -> a -> Outside inc r m ()
setListModHeadAt mxs i x' = setListModAt mxs i $ \xs -> case xs of
	ConsMod x mxs -> return $ ConsMod x' mxs
	NilMod -> return NilMod

deleteListModAt :: (Eq a,Eq (ListMod mod l inc r m a),Input mod l inc r m,Layer Outside inc r m) => Int -> ListMod mod l inc r m a -> Outside inc r m ()
deleteListModAt i mxs = setListModAt mxs i $ \xs -> case xs of
	ConsMod x mxs' -> getOutside mxs'
	NilMod -> error "shouldn't be empty"

insertListModAt :: (
	Eq a,Eq (ListMod mod l inc r m a),Input mod l inc r m,Layer Outside inc r m) => Int -> a -> ListMod mod l inc r m a -> Outside inc r m ()
insertListModAt i x mxs = setListModAt mxs i $ \xs -> do
	mxs' <- refOutside xs
	return $ ConsMod x mxs'

-- * Incremental length
	
updown1Inc :: (Eq (ListMod mod l inc r m a),MonadIO m,Output mod l inc r m,Eq a,Ord a,Memo (ListMod mod l inc r m a))
	=> mod l inc r m Bool -> ListMod mod l inc r m a -> l inc r m (ListMod mod l inc r m a)
updown1Inc mb mxs = do
	let up = quicksortInc compare
	let down = quicksortInc (\x y -> compare y x)
	thunk $ force mb >>= \b -> (if b then up mxs else down mxs) >>= force

updown2Inc :: (Eq (ListMod mod l inc r m a),MonadIO m,Output mod l inc r m,Eq a,Ord a,Memo (ListMod mod l inc r m a))
	=> mod l inc r m Bool -> ListMod mod l inc r m a -> l inc r m (ListMod mod l inc r m a)
updown2Inc mb mxs = do
	let up = quicksortInc compare mxs
	let down = quicksortInc (\x y -> compare y x) mxs
	thunk $ force mb >>= \b -> (if b then up else down) >>= force

--main = testFilterMemoMU

testFilterIncMU = runOuter $ testFilterIncMU' >> mergePDFsInto "filter.pdf"
testFilterIncMU' :: Outer IORef IO ()
testFilterIncMU' = do
	s :: ListM Inside IORef IO Int <- toListModInside [1,2,3,4]
	t :: ListU Inside IORef IO Int <- inside $ filterInc even s
	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
	display t
	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
	setListModHeadAt s 2 6
	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
	display t
	inL $ threadDelay 2000000 -- wait some time to make it more likely for the GC to purge unecessary data
	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
	return ()
	
testFilterMemoMU = runOuter $ testFilterMemoMU' >> mergePDFsInto "filter.pdf"
testFilterMemoMU' :: Outer IORef IO ()
testFilterMemoMU' = do
	s :: ListM Inside IORef IO Int <- toListModInside [1,2,3,4]
	t :: ListU Inside IORef IO Int <- inside $ filterInc even s
	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
	!() <- rnfInc t
	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
	setListModHeadAt s 2 6
--	s' :: ListM Inside IORef IO Int <- toListModInside [6,7]
--	modifyListModAt s 2 $ const $ get s'
	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
	!() <- display t
	inL $ threadDelay 3000000 -- wait some time to make it more likely for the GC to purge unecessary data
	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
	return ()

testFilterMemoLU = runOuter $ testFilterMemoLU' >> mergePDFsInto "filter.pdf"
testFilterMemoLU' :: Outer IORef IO ()
testFilterMemoLU' = do
	s :: ListL Inside IORef IO Int <- toListModInside [1,2,3,4]
	t :: ListU Inside IORef IO Int <- inside $ filterInc even s
	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
	display t
	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
--	setListModHeadAt s 2 6
	s' :: ListL Inside IORef IO Int <- toListModInside [6,7]
	modifyListModAt s 2 $ Prelude.const $ get s'
	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
	display t
	inL $ threadDelay 3000000 -- wait some time to make it more likely for the GC to purge unecessary data
	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
	return ()
	
testFilterLazyNonIncLU = runLazyNonIncOuter $ testFilterLazyNonIncLU'
testFilterLazyNonIncLU' :: Outside LazyNonInc IORef IO ()
testFilterLazyNonIncLU' = do
	s :: ListLazyNonIncL Inside IORef IO Int <- toListModInside [1,2,3,4]
	t :: ListLazyNonIncU Inside IORef IO Int <- inside $ filterInc even s
	display t
--	setListModHeadAt s 2 6
	s' :: ListLazyNonIncL Inside IORef IO Int <- toListModInside [6,7]
	modifyListModAt s 2 $ Prelude.const $ get s'
	display t
	return ()
	
--main = testFilterMemoMU
--main = testFilterMemoLU
--main = testFilterLazyNonIncLU

-- use case for our inner reference creation extension for which reference reuse should not happen for consistency with the non-incremental semantics
ex = runOuter ex'
ex' :: Outer IORef IO ()
ex' = do
	t :: U Inside Adapton IORef IO (L Inside Adapton IORef IO Int) <- inside $ thunkU $ modL $ return 5
	m <- inside $ force t
	inside $ display m
	setL m 6
	m' <- inside $ force t
	inside $ display m'

exNonInc = runLazyNonIncOuter exNonInc'
exNonInc' :: Outside LazyNonInc IORef IO ()
exNonInc' = do
	t :: LazyNonIncU Inside LazyNonInc IORef IO (LazyNonIncL Inside LazyNonInc IORef IO Int) <- inside $ thunkLazyNonIncU $ modLazyNonIncL $ return 5
	m <- inside $ force t
	inside $ display m
	setLazyNonIncL m 6
	m' <- inside $ force t
	inside $ display m'

proxyIORef = Proxy :: Proxy IORef
proxyIO = Proxy :: Proxy IO

data TreeChange a where
	TreeChg :: (forall mod l inc r m . (
			Eq (TreeMod mod l inc r m a),Input mod l inc r m,Layer l inc r m,Layer Outside inc r m
		)
		=> TreeMod mod l inc r m a -> Outside inc r m ()) -> TreeChange a

data Tree a = Empty | Bin a (Tree a) (Tree a) deriving (Show,Eq)

genTree :: Int -> Int -> IO (Tree Int,[TreeChange Int])
genTree depth runs = do
	tree <- genTree' depth
	chgs <- genTreeChgs runs depth
	return (tree,chgs)

genTreeChgs :: Int -> Int -> IO [TreeChange Int]
genTreeChgs 0 depth = return []
genTreeChgs n depth = do
	chg <- genTreeChg depth
	chgs <- genTreeChgs (n-1) depth
	return $ chg:chgs

genTreeChg :: Int -> IO (TreeChange Int)
genTreeChg 0 = return $ TreeChg $ \mt -> modify mt $ \EmptyMod -> return EmptyMod
genTreeChg depth = do
	j::Int <- generate $ frequency [(2,return 0),(10,choose (1,2))]
	case j of
		0 -> do
			y <- generate $ choose (minBound,maxBound)
			return $ TreeChg $ \mt -> modify mt $ \t -> case t of
				BinMod x l r -> return $ BinMod y l r
				EmptyMod -> return EmptyMod
		1 -> do
			TreeChg chg <- genTreeChg (depth-1)
			return $ TreeChg $ \mt -> getOutside mt >>= \t -> case t of
				BinMod x ml mr -> chg ml
				EmptyMod -> set mt EmptyMod
		2 -> do
			TreeChg chg <- genTreeChg (depth-1)
			return $ TreeChg $ \mt -> getOutside mt >>= \t -> case t of
				BinMod x ml mr -> chg mr
				EmptyMod -> set mt EmptyMod

genTree' :: (Random a,Bounded a) => Int -> IO (Tree a)
genTree' 0 = return Empty
genTree' depth = do
	x <- generate $ choose (minBound,maxBound)
	l <- genTree' (depth-1)
	r <- genTree' (depth-1)
	return $ Bin x l r

-- generates a list and a list of random positions in the list
genList :: Int -> Int -> IO ([Int],[Int])
genList i runs = do
	let max = i
	xs <- generate $ vectorOf max $ choose (minBound,maxBound)
	print xs
	positions <- generate $ vectorOf runs $ choose (0,max-1)
	print positions
	return (xs,positions)

data ListChange a where
	ListChg :: (forall mod l inc r m . (
			Eq (ListMod mod l inc r m a),Input mod l inc r m,Layer l inc r m,Layer Outside inc r m
		)
		=> ListMod mod l inc r m a -> Outside inc r m ()) -> ListChange a

genListPairs :: Int -> Int -> IO ([Int],[ListChange Int])
genListPairs i runs = do
	let max = i
	xs <- generate $ vectorOf max $ choose (minBound,maxBound)
	positionsDelete <- generate $ vectorOf runs $ choose (0,max-1)
	positionsInsert <- generate $ vectorOf runs $ choose (0,max-2)
	let merge [] [] = return []
	    merge (x:xs) (y:ys) = do
		tail <- merge xs ys
		v <- generate $ choose (minBound::Int,maxBound)
		return $ ListChg (deleteListModAt x) : ListChg (insertListModAt y v) : tail
	positions <- merge positionsDelete positionsInsert
	print xs
	return (xs,positions)
	
genProblem :: Int -> Int -> IO ([Int],[ListChange Int])
genProblem i runs = do
	let max = i
	let xs = [1,2,4,5,6,8]
	positionsDelete <- generate $ vectorOf (runs-1) $ choose (0,6-1)
	positionsInsert <- generate $ vectorOf runs $ choose (0,6-2)
	let merge [] [] = []
	    merge (x:xs) (y:ys) = ListChg (deleteListModAt x) : ListChg (insertListModAt y y) : merge xs ys
	let positions = merge (2:positionsDelete) positionsInsert
	return (xs,positions)

testM :: (ListM Inside IORef IO Int -> Inner IORef IO (ListU Inside IORef IO Int)) -> ([Int],[ListChange Int]) -> Int -> Outer IORef IO (Double,Double,Double)
testM f (xs,chgs) runs = do
	(scratch,(s,t)) <- timeOuterT $ do
		s :: ListM Inside IORef IO Int <- toListRefInside xs
		t :: ListU Inside IORef IO Int <- inside $ f s
--		drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
		!() <- rnfInc t
--		drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
		return (s,t)
	(dirty,propagate) <- testM' s t chgs
	return (scratch,dirty / toEnum runs,propagate / toEnum runs)

testM' :: ListM Inside IORef IO Int -> ListU Inside IORef IO Int -> [ListChange Int] -> Outer IORef IO (Double,Double)
testM' s t [] = return (0,0)
testM' s t (chg:chgs) = do
	(dirty,()) <- timeOuterT $ applyListChange chg s
--	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
	(propagate,()) <- timeOuterT $ rnfInc t
--	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
	(dirty',propagate') <- testM' s t chgs
	return (dirty+dirty',propagate+propagate')

testL :: (ListL Inside IORef IO Int -> Inner IORef IO (ListU Inside IORef IO Int)) -> ([Int],[ListChange Int]) -> Int -> Outer IORef IO (Double,Double)
testL f (xs,chgs) runs = do
	(scratch,(s,t)) <- timeOuterT $ do
		s :: ListL Inside IORef IO Int <- toListModInside xs
		t :: ListU Inside IORef IO Int <- inside $ f s
--		drawPDF proxyIORef proxyIO (Merge s t)
		!() <- rnfInc t
--		drawPDF proxyIORef proxyIO (Merge s t)
		return (s,t)
	(cycles,()) <- timeOuterT $ testL' s t chgs
	return (scratch,cycles / toEnum runs)

testL' :: ListL Inside IORef IO Int -> ListU Inside IORef IO Int -> [ListChange Int] -> Outer IORef IO ()
testL' s t [] = return ()
testL' s t (chg:chgs) = do
	applyListChange chg s
--	drawPDF proxyIORef proxyIO (Merge s t)
	!() <- rnfInc t
--	drawPDF proxyIORef proxyIO (Merge s t)
	testL' s t chgs

testNonIncL :: (ListLazyNonIncL Inside IORef IO Int -> Inside LazyNonInc IORef IO (ListLazyNonIncU Inside IORef IO Int)) -> ([Int],[ListChange Int]) -> Int -> Outside LazyNonInc IORef IO (Double,Double)
testNonIncL f (xs,chgs) runs = do
	(scratch,(s,t)) <- timeLazyNonIncOuterT $ do
		s :: ListLazyNonIncL Inside IORef IO Int <- toListModInside xs
		t :: ListLazyNonIncU Inside IORef IO Int <- inside $ f s
		!() <- rnfInc t
		return (s,t)
	(cycles,()) <- timeLazyNonIncOuterT $ testNonIncL' s t chgs
	return (scratch,cycles / toEnum runs)

testNonIncL' :: ListLazyNonIncL Inside IORef IO Int -> ListLazyNonIncU Inside IORef IO Int -> [ListChange Int] -> Outside LazyNonInc IORef IO ()
testNonIncL' s t [] = return ()
testNonIncL' s t (chg:chgs) = do
	applyListChange chg s
	!() <- rnfInc t
	testNonIncL' s t chgs
	
testNonIncM :: (ListLazyNonIncM Inside IORef IO Int -> Inside LazyNonInc IORef IO (ListLazyNonIncU Inside IORef IO Int)) -> ([Int],[ListChange Int]) -> Int -> Outside LazyNonInc IORef IO (Double,Double,Double)
testNonIncM f (xs,chgs) runs = do
	(scratch,(s,t)) <- timeLazyNonIncOuterT $ do
		s :: ListLazyNonIncM Inside IORef IO Int <- toListRefInside xs
		t :: ListLazyNonIncU Inside IORef IO Int <- inside $ f s
		!() <- rnfInc t
		return (s,t)
	(dirty,propagate) <- testNonIncM' s t chgs
	return (scratch,dirty / toEnum runs,propagate / toEnum runs)

testNonIncM' :: ListLazyNonIncM Inside IORef IO Int -> ListLazyNonIncU Inside IORef IO Int -> [ListChange Int] -> Outside LazyNonInc IORef IO (Double,Double)
testNonIncM' s t [] = return (0,0)
testNonIncM' s t (chg:chgs) = do
	(dirty,()) <- timeLazyNonIncOuterT $ applyListChange chg s
	(propagate,()) <- timeLazyNonIncOuterT $ rnfInc t
	(dirty',propagate') <- testNonIncM' s t chgs
	return (dirty+dirty',propagate+propagate')

--testListU :: (ListU Inside IORef IO Int -> Inner IORef IO (ListU Inside IORef IO Int)) -> ([Int],[ListChange Int]) -> Int -> Outer IORef IO (Double,Double)
--testListU f (xs,chgs) runs = do
--	(scratch,(s,t)) <- timeOuterT $ do
--		s :: ListU Inside IORef IO Int <- toListRefInside xs
--		t :: ListU Inside IORef IO Int <- inside $ f s
--		drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
--		!() <- rnfInc t
--		drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
--		return (s,t)
--	(cycles,()) <- timeOuterT $ testListU' s t chgs
--	return (scratch,cycles / toEnum runs)
--
--testListU' :: ListU Inside IORef IO Int -> ListU Inside IORef IO Int -> [ListChange Int] -> Outer IORef IO ()
--testListU' s t [] = return ()
--testListU' s t (chg:chgs) = do
--	applyListChange chg s
--	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
--	!() <- rnfInc t
--	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
--	testListU' s t chgs

--testU :: (ListU Inside IORef IO Int -> Inner IORef IO (U Inside Adapton IORef IO Int)) -> ([Int],[ListChange Int]) -> Int -> Outer IORef IO (Double,Double,Double)
--testU f (xs,chgs) runs = do
--	(scratch,(s,t)) <- timeOuterT $ do
--		s :: ListU Inside IORef IO Int <- toListRefInside xs
--		t :: U Inside Adapton IORef IO Int <- inside $ f s
----		drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
--		!() <- rnfInc t
----		drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
--		return (s,t)
--	(dirty,propagate) <- testU' s t chgs
--	return (scratch,dirty / toEnum runs, propagate / toEnum runs)
--
--testU' :: ListU Inside IORef IO Int -> U Inside Adapton IORef IO Int -> [ListChange Int] -> Outer IORef IO (Double,Double)
--testU' s t [] = return (0,0)
--testU' s t (chg:chgs) = do
--	(dirty,()) <- timeOuterT $ applyListChange chg s
----	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
--	(propagate,()) <- timeOuterT $ rnfInc t
----	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
--	(dirty',propagate') <- testU' s t chgs
--	return (dirty+dirty',propagate+propagate')
	
testTreeM :: (Eq b,MData (DrawDict Adapton IORef IO) (Outer IORef IO) b,NFDataInc Outside Adapton IORef IO b) => (TreeM Inside IORef IO Int -> Inner IORef IO (U Inside Adapton IORef IO b)) -> (Tree Int,[TreeChange Int]) -> Int -> Outer IORef IO (Double,Double)
testTreeM f (xs,chgs) runs = do
	(scratch,(s,t)) <- timeOuterT $ do
		s :: TreeM Inside IORef IO Int <- inside $ toTreeRef xs
		t :: U Inside Adapton IORef IO b <- inside $ f s
		drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
		!() <- rnfInc t
		drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
		return (s,t)
	(cycles,()) <- timeOuterT $ testTreeM' s t chgs
	return (scratch,cycles / toEnum runs)

testTreeM' :: (Eq b,MData (DrawDict Adapton IORef IO) (Outer IORef IO) b,NFDataInc Outside Adapton IORef IO b) => TreeM Inside IORef IO Int -> U Inside Adapton IORef IO b -> [TreeChange Int] -> Outer IORef IO ()
testTreeM' s t [] = return ()
testTreeM' s t (chg:chgs) = do
	applyTreeChange chg s
	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
	!() <- rnfInc t
	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
	testTreeM' s t chgs

testNonIncTreeM :: (TreeLazyNonIncM Inside IORef IO Int -> Inside LazyNonInc IORef IO (LazyNonIncU Inside LazyNonInc IORef IO Int)) -> (Tree Int,[TreeChange Int]) -> Int -> Outside LazyNonInc IORef IO (Double,Double)
testNonIncTreeM f (xs,chgs) runs = do
	(scratch,(s,t)) <- timeLazyNonIncOuterT $ do
		s :: TreeLazyNonIncM Inside IORef IO Int <- inside $ toTreeRef xs
		t :: LazyNonIncU Inside LazyNonInc IORef IO Int <- inside $ f s
--		drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
		!() <- rnfInc t
--		drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
		return (s,t)
	(cycles,()) <- timeLazyNonIncOuterT $ testNonIncTreeM' s t chgs
	return (scratch,cycles / toEnum runs)

testNonIncTreeM' :: TreeLazyNonIncM Inside IORef IO Int -> LazyNonIncU Inside LazyNonInc IORef IO Int -> [TreeChange Int] -> Outside LazyNonInc IORef IO ()
testNonIncTreeM' s t [] = return ()
testNonIncTreeM' s t (chg:chgs) = do
	applyTreeChange chg s
--	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
	!() <- rnfInc t
--	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
	testNonIncTreeM' s t chgs

testNonIncU :: (ListLazyNonIncU Inside IORef IO Int -> Inside LazyNonInc IORef IO (LazyNonIncU Inside LazyNonInc IORef IO Int)) -> ([Int],[ListChange Int]) -> Int -> Outside LazyNonInc IORef IO (Double,Double,Double)
testNonIncU f (xs,chgs) runs = do
	(scratch,(s,t)) <- timeLazyNonIncOuterT $ do
		s :: ListLazyNonIncU Inside IORef IO Int <- toListRefInside xs
		t :: LazyNonIncU Inside LazyNonInc IORef IO Int <- inside $ f s
		!() <- rnfInc t
		return (s,t)
	(dirty,propagate) <- testNonIncU' s t chgs
	return (scratch,dirty / toEnum runs,propagate / toEnum runs)

testNonIncU' :: ListLazyNonIncU Inside IORef IO Int -> LazyNonIncU Inside LazyNonInc IORef IO Int -> [ListChange Int] -> Outside LazyNonInc IORef IO (Double,Double)
testNonIncU' s t [] = return (0,0)
testNonIncU' s t (chg:chgs) = do
	(dirty,()) <- timeLazyNonIncOuterT $ applyListChange chg s
	(propagate,()) <- timeLazyNonIncOuterT $ rnfInc t
	(dirty',propagate') <- testNonIncU' s t chgs
	return (dirty+dirty',propagate+propagate')


applyListChange :: (Eq (ListMod mod l inc r m a),Input mod l inc r m,Layer l inc r m,Layer Outside inc r m)
	=> ListChange a -> ListMod mod l inc r m a -> Outside inc r m ()
applyListChange (ListChg f) xs = f xs

applyTreeChange :: (Eq (TreeMod mod l inc r m a),Input mod l inc r m,Layer l inc r m,Layer Outside inc r m)
	=> TreeChange a -> TreeMod mod l inc r m a -> Outside inc r m ()
applyTreeChange (TreeChg f) xs = f xs

--main = do
--	let size = 10
--	let runs = 5
--	let p = even
--	gen <- genListPairs size runs
----	res1 <- runOuter $ testL (filterInc p) gen (runs * 2)
--	res2 <- runOuter $ testM (filterInc p) gen (runs * 2) >> mergePDFsInto "filterInc.pdf"
----	res3 <- runLazyNonIncOuter $ testNonIncL (filterInc p) gen (runs * 2)
----	res4 <- runLazyNonIncOuter $ testNonIncM (filterInc p) gen (runs * 2)
----	print res1
--	print res2
----	print res3
----	print res4

--main = do
--	let size = 10^5
--	let runs = 100
--	let f x y = return $ x + y
--	gen <- genListPairs size runs
----	gen <- return ([5,2,6,1,3],ListChg (deleteListModAt 2):ListChg (insertListModAt 3 (-1)):[])
----	res1 <- runOuter $ testL (filterInc p) gen (runs * 2)
--	res2 <- runOuter $ testU (fold1Inc f) gen (runs * 2) -- >> mergePDFsInto "filterInc.pdf"
----	res3 <- runLazyNonIncOuter $ testNonIncL (filterInc2 p) gen (runs * 2)
--	res4 <- runLazyNonIncOuter $ testNonIncU (fold1Inc f) gen (runs * 2)
----	print res1
--	print res2
----	print res3
--	print res4

--main = do
--	let size = 10^2
--	let runs = 100
--	gen <- genListPairs size runs
----	res1 <- runOuter $ testL (filterInc p) gen (runs * 2)
--	res2 <- runOuter $ testM (mapInc return >=> quicksortInc2 compare) gen (runs * 2) -- >> mergePDFsInto "filterInc.pdf"
----	res3 <- runLazyNonIncOuter $ testNonIncL (filterInc2 p) gen (runs * 2)
--	res4 <- runLazyNonIncOuter $ testNonIncM (mapInc return >=> quicksortInc2 compare) gen (runs * 2)
----	print res1
--	print res2
----	print res3
--	print res4



main = runOuter $ do
	(mxs :: ListMod L Inside Adapton IORef IO (String,Int)) <- inside $ toListRef [("a",5),("b",4),("c",3)]
--	(mys :: ListMod M Inside Adapton IORef IO (String,Int)) <- inside $ toListRef [("c",3),("b",3),("d",2)]
--	(uxs :: ListMod U Inside Adapton IORef IO (String,Int)) <- inside $ mapInc return mxs
--	(uys :: ListMod U Inside Adapton IORef IO (String,Int)) <- inside $ mapInc return mys
	
	inL $ liftIO $ putStrLn "\n\n"
	
	(txs :: ListMod L Inside Adapton IORef IO (String,Int)) <- inside $ copyInc proxyNoCtx mxs
	
	inL $ liftIO $ putStrLn "\n\n"
	
	drawToPDF proxyAdapton proxyIORef proxyIO (Merge mxs txs) "copy.pdf"
	
	modifyListModAt mxs 1 $ \(ConsMod y mys') -> return $ ConsMod ("c",5) mys'
	
--	display txs
	
--	drawToPDF proxyAdapton proxyIORef proxyIO (Merge mxs txs) "copy.pdf"
	
--	(thunk :: ListMod U Inside Adapton IORef IO (String,Int)) <- inside $ mergeMapInc (\(kx,vx) (ky,vy) -> compare vy vx) (\x y -> return $ x + y) uxs uys
--	display thunk
	
--	modifyListModAt mxs 1 $ \(ConsMod y mys') -> return $ ConsMod ("c",5) mys'
--	modifyListModAt mxs 2 $ \(ConsMod y mys') -> return $ ConsMod ("b",4) mys'
--	
--	inL $ liftIO $ putStrLn "\n\n"
--
--	display thunk

--main = do
--	let size = 1
--	let runs = 5
----	let f x y = return $ x + y
--	gen@(tree,_) <- genTree size runs
--	putStrLn $ show tree
----	gen <- return ([5,2,6,1,3],ListChg (deleteListModAt 2):ListChg (insertListModAt 3 (-1)):[])
----	res1 <- runOuter $ testL (filterInc p) gen (runs * 2)
----	res2 <- runOuter $ testTreeM (foldTreeInc 0 f) gen runs >> mergePDFsInto "filterInc.pdf"
----	res3 <- runLazyNonIncOuter $ testNonIncL (filterInc2 p) gen (runs * 2)
----	res4 <- runLazyNonIncOuter $ testNonIncTreeM (foldTreeInc 0 f) gen runs
--	res5 <- runOuter $ testTreeM (listifyTree) gen runs >> mergePDFsInto "filterInc.pdf"
----	print res1
----	print res2
----	print res3
----	print res4
--	print res5

--main = do
--	res <- runLazyNonIncOuter $ testFactNonInc 10 250
--	print res

--myConfig = defaultConfig { resamples = Last (Just 0) } 

--main = do
--	let size = 6
--	let runs = 250
--	genList size runs >>= \gen -> 
--		defaultMainWith defaultConfig
--		[	bgroup "filter"
--				[	bench "incL scratch" $ whnfIO $ runOuter $ testL (filterMemoU even) gen 0
--				,	bench "incL" $ whnfIO $ runOuter $ testL (filterMemoU even) gen runs
--				,	bench "incM scratch" $ whnfIO $ runOuter $ testM (filterMemoU even) gen 0
--				,	bench "incM" $ whnfIO $ runOuter $ testM (filterMemoU even) gen runs
--				,	bench "nonincL scratch" $ whnfIO $ runLazyNonIncOuter $ testNonIncL (filterInc2 even) gen 0
--				,	bench "nonincL" $ whnfIO $ runLazyNonIncOuter $ testNonIncL (filterInc2 even) gen runs
--				,	bench "nonincM scratch" $ whnfIO $ runLazyNonIncOuter $ testNonIncM (filterInc2 even) gen 0
--				,	bench "nonincM" $ whnfIO $ runLazyNonIncOuter $ testNonIncM (filterInc2 even) gen runs
--       			]
--		]
	
timeOuterT :: Outer r IO a -> Outer r IO (Double,a)
timeOuterT (Outer m) = Outer $ timeItT m

timeLazyNonIncOuterT :: Outside LazyNonInc r IO a -> Outside LazyNonInc r IO (Double,a)
timeLazyNonIncOuterT (LazyNonIncOuter m) = LazyNonIncOuter $ timeItT m

sumTree :: (MData (MemoCtx NoCtx) (Inside inc r m) (TreeMod mod Inside inc r m Int),Output U Inside inc r m,MonadIO m) => TreeMod mod Inside inc r m Int -> Inside inc r m (U Inside inc r m Int)
sumTree = gsumInc

listifyTree :: (MData (MemoCtx NoCtx) (Inside inc r m) (TreeMod mod Inside inc r m Int),Output U Inside inc r m,MonadIO m) => TreeMod mod Inside inc r m Int -> Inside inc r m (JoinListMod U Inside inc r m Int)
listifyTree = unGenericQMemo $ listifyInc proxyNoCtx (\(i::Int) -> return True)

