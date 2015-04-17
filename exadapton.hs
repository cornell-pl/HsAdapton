{-# LANGUAGE TypeFamilies, BangPatterns, Rank2Types, GADTs, DeriveDataTypeable, AllowAmbiguousTypes, MultiParamTypeClasses, TemplateHaskell, ScopedTypeVariables, KindSignatures, ConstraintKinds, FlexibleContexts, StandaloneDeriving, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.DeepTypeable
import Data.WithClass.MData
import Data.DeriveTH
import Data.WithClass.Derive.MData
import Data.WithClass.Derive.DeepTypeable
import Language.Haskell.TH.Syntax

import System.Random
import System.IO
import Control.Monad.Incremental hiding (LiftInc(..))
import Control.Monad.Incremental.Draw
import Control.Monad.Incremental.Display
import Control.Monad.Incremental.LazyNonInc
import Control.Monad.Incremental.List (JoinListMod')
import Control.Monad.Incremental.Adapton hiding (MData)
import qualified Control.Monad.Incremental.Adapton as Adapton
import Data.Proxy
import System.Mem.MemoTable
import Data.Memo
import Data.Derive.Memo
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
 as WeakKey
import System.Mem.Weak as Weak
 as WeakTable

import Test.QuickCheck.Gen
import Test.QuickCheck
import System.IO.Unsafe
import Data.Unique
import System.Mem.StableName.Exts
import Control.Exception
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

--import Debug

-- * Generic modifiable lists and list operations

-- creates an input list from a regular list
toListRefInside :: (
	IncK inc (ListMod' mod Inside inc r m a),Input mod Inside inc r m,Layer l inc r m) => [a] -> l inc r m (ListMod mod Inside inc r m a)
toListRefInside xs = inside $ toListRef xs

toListRef :: (
	IncK inc (ListMod' mod l inc r m a),Input mod l inc r m) => [a] -> l inc r m (ListMod mod l inc r m a)
toListRef [] = ref NilMod
toListRef (x:xs) = do
	mxs <- toListRef xs
	ref (ConsMod x mxs)

toTreeRef :: (
	IncK inc (TreeMod' mod l inc r m a),Input mod l inc r m) => Tree a -> l inc r m (TreeMod mod l inc r m a)
toTreeRef Empty = ref EmptyMod
toTreeRef (Bin x l r) = do
	ml <- toTreeRef l
	mr <- toTreeRef r
	ref (BinMod x ml mr)
	
toListModInside :: (IncK inc (ListMod' mod Inside inc r m a),
	Input mod Inside inc r m,Layer l inc r m) => [a] -> l inc r m (ListMod mod Inside inc r m a)
toListModInside xs = inside $ toListMod xs

--main = testFilterMemoMU

testFilterIncMU = runOuter $ testFilterIncMU' >> mergeGraphsInto "filter.pdf"
testFilterIncMU' :: Outer IORef IO ()
testFilterIncMU' = do
	s :: ListM Inside IORef IO Int <- toListModInside [1,2,3,4]
	t :: ListU Inside IORef IO Int <- inside $ filterInc (return . even) s
	drawPDF "" proxyAdapton proxyIORef proxyIO (Merge s t)
	display t
	drawPDF "" proxyAdapton proxyIORef proxyIO (Merge s t)
	setListModHeadAt s 2 6
	drawPDF "" proxyAdapton proxyIORef proxyIO (Merge s t)
	display t
	inL $ threadDelay 2000000 -- wait some time to make it more likely for the GC to purge unecessary data
	drawPDF "" proxyAdapton proxyIORef proxyIO (Merge s t)
	return ()
	
testFilterMemoMU = runOuter $ testFilterMemoMU' >> mergeGraphsInto "filter.pdf"
testFilterMemoMU' :: Outer IORef IO ()
testFilterMemoMU' = do
	s :: ListM Inside IORef IO Int <- toListModInside [1,2,3,4]
	t :: ListU Inside IORef IO Int <- inside $ filterInc (return . even) s
	drawPDF "" proxyAdapton proxyIORef proxyIO (Merge s t)
	!() <- nfDataInc Proxy Proxy Proxy Proxy t
	drawPDF "" proxyAdapton proxyIORef proxyIO (Merge s t)
	setListModHeadAt s 2 6
--	s' :: ListM Inside IORef IO Int <- toListModInside [6,7]
--	modifyListModAt s 2 $ const $ get s'
	drawPDF "" proxyAdapton proxyIORef proxyIO (Merge s t)
	!() <- display t
	inL $ threadDelay 3000000 -- wait some time to make it more likely for the GC to purge unecessary data
	drawPDF "" proxyAdapton proxyIORef proxyIO (Merge s t)
	return ()

testFilterMemoLU = runOuter $ testFilterMemoLU' >> mergeGraphsInto "filter.pdf"
testFilterMemoLU' :: Outer IORef IO ()
testFilterMemoLU' = do
	s :: ListL Inside IORef IO Int <- toListModInside [1,2,3,4]
	t :: ListU Inside IORef IO Int <- inside $ filterInc (return . even) s
	drawPDF "" proxyAdapton proxyIORef proxyIO (Merge s t)
	display t
	drawPDF "" proxyAdapton proxyIORef proxyIO (Merge s t)
--	setListModHeadAt s 2 6
	s' :: ListL Inside IORef IO Int <- toListModInside [6,7]
	modifyListModAt s 2 $ Prelude.const $ get s'
	drawPDF "" proxyAdapton proxyIORef proxyIO (Merge s t)
	display t
	inL $ threadDelay 3000000 -- wait some time to make it more likely for the GC to purge unecessary data
	drawPDF "" proxyAdapton proxyIORef proxyIO (Merge s t)
	return ()
	
testFilterLazyNonIncLU = runLazyNonIncOuter $ testFilterLazyNonIncLU'
testFilterLazyNonIncLU' :: Outside LazyNonInc IORef IO ()
testFilterLazyNonIncLU' = do
	s :: ListLazyNonIncL Inside IORef IO Int <- toListModInside [1,2,3,4]
	t :: ListLazyNonIncU Inside IORef IO Int <- inside $ filterInc (return . even) s
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

data TreeChange a where
	TreeChg :: (forall mod l inc r m . (
			IncK inc (TreeMod' mod l inc r m a),Input mod l inc r m,Layer l inc r m,Layer Outside inc r m
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
			y::Int <- generate $ choose (minBound,maxBound)
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



genListPairs :: Int -> Int -> IO ([Int],[ListChange Int])
genListPairs i runs = do
	let max = i
	xs <- generate $ vectorOf max $ choose (minBound,maxBound)
	positionsDelete <- generate $ vectorOf runs $ choose (0,max-1)
	positionsInsert <- generate $ vectorOf runs $ choose (0,max-2)	    
	positions <- merge positionsDelete positionsInsert
	print xs
	return (xs,positions)
  where
	merge :: [Int] -> [Int] -> IO [ListChange Int]
	merge [] [] = return []
	merge (x:xs) (y:ys) = do
		tail <- merge xs ys
		v <- generate $ choose (minBound::Int,maxBound)
		return $ ListChg (deleteListModAt x) : ListChg (insertListModAt y v) : tail
	
genProblem :: Int -> Int -> IO ([Int],[ListChange Int])
genProblem i runs = do
	let max = i
	let xs = [1,2,4,5,6,8]
	positionsDelete <- generate $ vectorOf (runs-1) $ choose (0,6-1)
	positionsInsert <- generate $ vectorOf runs $ choose (0,6-2)
	let positions = merge (2:positionsDelete) positionsInsert
	return (xs,positions)
  where
	merge :: [Int] -> [Int] -> [ListChange Int]
	merge [] [] = []
	merge (x:xs) (y:ys) = ListChg (deleteListModAt x) : ListChg (insertListModAt y y) : merge xs ys

testM :: (ListM Inside IORef IO Int -> Inner IORef IO (ListU Inside IORef IO Int)) -> ([Int],[ListChange Int]) -> Int -> Outer IORef IO (Double,Double,Double)
testM f (xs,chgs) runs = do
	(scratch,(s,t)) <- timeOuterT $ do
		s :: ListM Inside IORef IO Int <- toListRefInside xs
		t :: ListU Inside IORef IO Int <- inside $ f s
--		drawPDF "" proxyAdapton proxyIORef proxyIO (Merge s t)
		!() <- nfDataInc Proxy Proxy Proxy Proxy t
--		drawPDF "" proxyAdapton proxyIORef proxyIO (Merge s t)
		return (s,t)
	(dirty,propagate) <- testM' s t chgs
	return (scratch,dirty / toEnum runs,propagate / toEnum runs)

testM' :: ListM Inside IORef IO Int -> ListU Inside IORef IO Int -> [ListChange Int] -> Outer IORef IO (Double,Double)
testM' s t [] = return (0,0)
testM' s t (chg:chgs) = do
	(dirty,()) <- timeOuterT $ applyListChange chg s
--	drawPDF "" proxyAdapton proxyIORef proxyIO (Merge s t)
	(propagate,()) <- timeOuterT $ nfDataInc Proxy Proxy Proxy Proxy t
--	drawPDF "" proxyAdapton proxyIORef proxyIO (Merge s t)
	(dirty',propagate') <- testM' s t chgs
	return (dirty+dirty',propagate+propagate')

testL :: (ListL Inside IORef IO Int -> Inner IORef IO (ListU Inside IORef IO Int)) -> ([Int],[ListChange Int]) -> Int -> Outer IORef IO (Double,Double)
testL f (xs,chgs) runs = do
	(scratch,(s,t)) <- timeOuterT $ do
		s :: ListL Inside IORef IO Int <- toListModInside xs
		t :: ListU Inside IORef IO Int <- inside $ f s
--		drawPDF proxyIORef proxyIO (Merge s t)
		!() <- nfDataInc Proxy Proxy Proxy Proxy t
--		drawPDF proxyIORef proxyIO (Merge s t)
		return (s,t)
	(cycles,()) <- timeOuterT $ testL' s t chgs
	return (scratch,cycles / toEnum runs)

testL' :: ListL Inside IORef IO Int -> ListU Inside IORef IO Int -> [ListChange Int] -> Outer IORef IO ()
testL' s t [] = return ()
testL' s t (chg:chgs) = do
	applyListChange chg s
--	drawPDF proxyIORef proxyIO (Merge s t)
	!() <- nfDataInc Proxy Proxy Proxy Proxy t
--	drawPDF proxyIORef proxyIO (Merge s t)
	testL' s t chgs

testNonIncL :: (ListLazyNonIncL Inside IORef IO Int -> Inside LazyNonInc IORef IO (ListLazyNonIncU Inside IORef IO Int)) -> ([Int],[ListChange Int]) -> Int -> Outside LazyNonInc IORef IO (Double,Double)
testNonIncL f (xs,chgs) runs = do
	(scratch,(s,t)) <- timeLazyNonIncOuterT $ do
		s :: ListLazyNonIncL Inside IORef IO Int <- toListModInside xs
		t :: ListLazyNonIncU Inside IORef IO Int <- inside $ f s
		!() <- nfDataInc Proxy Proxy Proxy Proxy t
		return (s,t)
	(cycles,()) <- timeLazyNonIncOuterT $ testNonIncL' s t chgs
	return (scratch,cycles / toEnum runs)

testNonIncL' :: ListLazyNonIncL Inside IORef IO Int -> ListLazyNonIncU Inside IORef IO Int -> [ListChange Int] -> Outside LazyNonInc IORef IO ()
testNonIncL' s t [] = return ()
testNonIncL' s t (chg:chgs) = do
	applyListChange chg s
	!() <- nfDataInc Proxy Proxy Proxy Proxy t
	testNonIncL' s t chgs
	
testNonIncM :: (ListLazyNonIncM Inside IORef IO Int -> Inside LazyNonInc IORef IO (ListLazyNonIncU Inside IORef IO Int)) -> ([Int],[ListChange Int]) -> Int -> Outside LazyNonInc IORef IO (Double,Double,Double)
testNonIncM f (xs,chgs) runs = do
	(scratch,(s,t)) <- timeLazyNonIncOuterT $ do
		s :: ListLazyNonIncM Inside IORef IO Int <- toListRefInside xs
		t :: ListLazyNonIncU Inside IORef IO Int <- inside $ f s
		!() <- nfDataInc Proxy Proxy Proxy Proxy t
		return (s,t)
	(dirty,propagate) <- testNonIncM' s t chgs
	return (scratch,dirty / toEnum runs,propagate / toEnum runs)

testNonIncM' :: ListLazyNonIncM Inside IORef IO Int -> ListLazyNonIncU Inside IORef IO Int -> [ListChange Int] -> Outside LazyNonInc IORef IO (Double,Double)
testNonIncM' s t [] = return (0,0)
testNonIncM' s t (chg:chgs) = do
	(dirty,()) <- timeLazyNonIncOuterT $ applyListChange chg s
	(propagate,()) <- timeLazyNonIncOuterT $ nfDataInc Proxy Proxy Proxy Proxy t
	(dirty',propagate') <- testNonIncM' s t chgs
	return (dirty+dirty',propagate+propagate')

--testListU :: (ListU Inside IORef IO Int -> Inner IORef IO (ListU Inside IORef IO Int)) -> ([Int],[ListChange Int]) -> Int -> Outer IORef IO (Double,Double)
--testListU f (xs,chgs) runs = do
--	(scratch,(s,t)) <- timeOuterT $ do
--		s :: ListU Inside IORef IO Int <- toListRefInside xs
--		t :: ListU Inside IORef IO Int <- inside $ f s
--		drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
--		!() <- nfDataInc t
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
--	!() <- nfDataInc t
--	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
--	testListU' s t chgs

--testU :: (ListU Inside IORef IO Int -> Inner IORef IO (U Inside Adapton IORef IO Int)) -> ([Int],[ListChange Int]) -> Int -> Outer IORef IO (Double,Double,Double)
--testU f (xs,chgs) runs = do
--	(scratch,(s,t)) <- timeOuterT $ do
--		s :: ListU Inside IORef IO Int <- toListRefInside xs
--		t :: U Inside Adapton IORef IO Int <- inside $ f s
----		drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
--		!() <- nfDataInc t
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
--	(propagate,()) <- timeOuterT $ nfDataInc t
----	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
--	(dirty',propagate') <- testU' s t chgs
--	return (dirty+dirty',propagate+propagate')
	
testTreeM :: (Eq b,MData (DrawDict Adapton IORef IO) (Outer IORef IO) b,NFDataInc Outside Adapton IORef IO b) => (TreeM Inside IORef IO Int -> Inner IORef IO (U Inside Adapton IORef IO b)) -> (Tree Int,[TreeChange Int]) -> Int -> Outer IORef IO (Double,Double)
testTreeM f (xs,chgs) runs = do
	(scratch,(s,t)) <- timeOuterT $ do
		s :: TreeM Inside IORef IO Int <- inside $ toTreeRef xs
		t :: U Inside Adapton IORef IO b <- inside $ f s
		drawPDF "" proxyAdapton proxyIORef proxyIO (Merge s t)
		!() <- nfDataInc Proxy Proxy Proxy Proxy t
		drawPDF "" proxyAdapton proxyIORef proxyIO (Merge s t)
		return (s,t)
	(cycles,()) <- timeOuterT $ testTreeM' s t chgs
	return (scratch,cycles / toEnum runs)

testTreeM' :: (Eq b,MData (DrawDict Adapton IORef IO) (Outer IORef IO) b,NFDataInc Outside Adapton IORef IO b) => TreeM Inside IORef IO Int -> U Inside Adapton IORef IO b -> [TreeChange Int] -> Outer IORef IO ()
testTreeM' s t [] = return ()
testTreeM' s t (chg:chgs) = do
	applyTreeChange chg s
	drawPDF "" proxyAdapton proxyIORef proxyIO (Merge s t)
	!() <- nfDataInc Proxy Proxy Proxy Proxy t
	drawPDF "" proxyAdapton proxyIORef proxyIO (Merge s t)
	testTreeM' s t chgs

testNonIncTreeM :: (TreeLazyNonIncM Inside IORef IO Int -> Inside LazyNonInc IORef IO (LazyNonIncU Inside LazyNonInc IORef IO Int)) -> (Tree Int,[TreeChange Int]) -> Int -> Outside LazyNonInc IORef IO (Double,Double)
testNonIncTreeM f (xs,chgs) runs = do
	(scratch,(s,t)) <- timeLazyNonIncOuterT $ do
		s :: TreeLazyNonIncM Inside IORef IO Int <- inside $ toTreeRef xs
		t :: LazyNonIncU Inside LazyNonInc IORef IO Int <- inside $ f s
--		drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
		!() <- nfDataInc Proxy Proxy Proxy Proxy t
--		drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
		return (s,t)
	(cycles,()) <- timeLazyNonIncOuterT $ testNonIncTreeM' s t chgs
	return (scratch,cycles / toEnum runs)

testNonIncTreeM' :: TreeLazyNonIncM Inside IORef IO Int -> LazyNonIncU Inside LazyNonInc IORef IO Int -> [TreeChange Int] -> Outside LazyNonInc IORef IO ()
testNonIncTreeM' s t [] = return ()
testNonIncTreeM' s t (chg:chgs) = do
	applyTreeChange chg s
--	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
	!() <- nfDataInc Proxy Proxy Proxy Proxy t
--	drawPDF proxyAdapton proxyIORef proxyIO (Merge s t)
	testNonIncTreeM' s t chgs

--testNonIncU :: (ListLazyNonIncU Inside IORef IO Int -> Inside LazyNonInc IORef IO (LazyNonIncU Inside LazyNonInc IORef IO Int)) -> ([Int],[ListChange Int]) -> Int -> Outside LazyNonInc IORef IO (Double,Double,Double)
--testNonIncU f (xs,chgs) runs = do
--	(scratch,(s,t)) <- timeLazyNonIncOuterT $ do
--		s :: ListLazyNonIncU Inside IORef IO Int <- toListRefInside xs
--		t :: LazyNonIncU Inside LazyNonInc IORef IO Int <- inside $ f s
--		!() <- nfDataInc t
--		return (s,t)
--	(dirty,propagate) <- testNonIncU' s t chgs
--	return (scratch,dirty / toEnum runs,propagate / toEnum runs)
--
--testNonIncU' :: ListLazyNonIncU Inside IORef IO Int -> LazyNonIncU Inside LazyNonInc IORef IO Int -> [ListChange Int] -> Outside LazyNonInc IORef IO (Double,Double)
--testNonIncU' s t [] = return (0,0)
--testNonIncU' s t (chg:chgs) = do
--	(dirty,()) <- timeLazyNonIncOuterT $ applyListChange chg s
--	(propagate,()) <- timeLazyNonIncOuterT $ nfDataInc t
--	(dirty',propagate') <- testNonIncU' s t chgs
--	return (dirty+dirty',propagate+propagate')




applyTreeChange :: (IncK inc (TreeMod' mod l inc r m a),Input mod l inc r m,Layer l inc r m,Layer Outside inc r m)
	=> TreeChange a -> TreeMod mod l inc r m a -> Outside inc r m ()
applyTreeChange (TreeChg f) xs = f xs

--main = do
--	let size = 10
--	let runs = 5
--	let p = even
--	gen <- genListPairs size runs
----	res1 <- runOuter $ testL (filterInc p) gen (runs * 2)
--	res2 <- runOuter $ testM (filterInc p) gen (runs * 2) >> mergeGraphsInto "filterInc.pdf"
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
--	res2 <- runOuter $ testU (fold1Inc f) gen (runs * 2) -- >> mergeGraphsInto "filterInc.pdf"
----	res3 <- runLazyNonIncOuter $ testNonIncL (filterInc2 p) gen (runs * 2)
--	res4 <- runLazyNonIncOuter $ testNonIncU (fold1Inc f) gen (runs * 2)
----	print res1
--	print res2
----	print res3
--	print res4

main_A = do
	let size = 10^3
	let runs = 5
	gen <- genListPairs size runs
	let inc_func = mapInc return >=> quicksortInc (\x y -> return $ compare x y)
--	let inc_func = filterInc (return . even)
--	res1 <- runOuter $ testL inc_func gen (runs * 2)
	res2 <- runOuter $ testM inc_func gen (runs * 2) -- >> mergeGraphsInto "filterInc.pdf"
--	res3 <- runLazyNonIncOuter $ testNonIncL inc_func gen (runs * 2)
--	res4 <- runLazyNonIncOuter $ testNonIncM inc_func gen (runs * 2)
--	print res1
	print res2
--	print res3
--	print res4



--main = runOuter $ do
--	(mxs :: ListMod L Inside Adapton IORef IO (String,Int)) <- inside $ toListRef [("a",5),("b",4),("c",3)]
----	(mys :: ListMod M Inside Adapton IORef IO (String,Int)) <- inside $ toListRef [("c",3),("b",3),("d",2)]
----	(uxs :: ListMod U Inside Adapton IORef IO (String,Int)) <- inside $ mapInc return mxs
----	(uys :: ListMod U Inside Adapton IORef IO (String,Int)) <- inside $ mapInc return mys
--	
--	liftIO $ putStrLn "\n\n"
--	
--	(txs :: ListMod L Inside Adapton IORef IO (String,Int)) <- inside $ copyInc proxyNoCtx mxs
--	
--	liftIO $ putStrLn "\n\n"
--	
--	drawToPDF proxyAdapton proxyIORef proxyIO (Merge mxs txs) "copy.pdf"
--	
--	modifyListModAt mxs 1 $ \(ConsMod y mys') -> return $ ConsMod ("c",5) mys'
--	
--	display txs
	
--	drawToPDF proxyAdapton proxyIORef proxyIO (Merge mxs txs) "copy.pdf"
	
--	(thunk :: ListMod U Inside Adapton IORef IO (String,Int)) <- inside $ mergeMapInc (\(kx,vx) (ky,vy) -> compare vy vx) (\x y -> return $ x + y) uxs uys
--	display thunk
	
--	modifyListModAt mxs 1 $ \(ConsMod y mys') -> return $ ConsMod ("c",5) mys'
--	modifyListModAt mxs 2 $ \(ConsMod y mys') -> return $ ConsMod ("b",4) mys'
--	
--	liftIO $ putStrLn "\n\n"
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
----	res2 <- runOuter $ testTreeM (foldTreeInc 0 f) gen runs >> mergeGraphsInto "filterInc.pdf"
----	res3 <- runLazyNonIncOuter $ testNonIncL (filterInc2 p) gen (runs * 2)
----	res4 <- runLazyNonIncOuter $ testNonIncTreeM (foldTreeInc 0 f) gen runs
--	res5 <- runOuter $ testTreeM (listifyTree) gen runs >> mergeGraphsInto "filterInc.pdf"
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

sumTree :: (IncK inc Int,MData (MemoCtx NoCtx) (Inside inc r m) (TreeMod mod Inside inc r m Int),Output U Inside inc r m, m) => TreeMod mod Inside inc r m Int -> Inside inc r m (U Inside inc r m Int)
sumTree = gsumInc

listifyTree :: (IncK inc (JoinListMod' U Inside inc r m Int),MData (MemoCtx NoCtx) (Inside inc r m) (TreeMod mod Inside inc r m Int),Output U Inside inc r m, m) => TreeMod mod Inside inc r m Int -> Inside inc r m (JoinListMod U Inside inc r m Int)
listifyTree = listifyInc proxyNoCtx (\(i::Int) -> return True)


-- * Customer example

topkInc :: (IncK inc (ListMod' thunk l inc r m a),IncK inc (ListMod' mod l inc r m a),Eq a,Eq (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Hashable (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Eq (ListMod mod l inc r m a),Eq (ListMod thunk l inc r m a), m,Memo a,Memo (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Memo (ListMod mod l inc r m a),Thunk mod l inc r m,Memo (ListMod thunk l inc r m a),Output thunk l inc r m) =>
	(a -> a -> l inc r m Ordering) -> Int -> ListMod mod l inc r m a -> l inc r m (ListMod thunk l inc r m a)
topkInc cmp i = memo $ \_ mxs -> (mapInc return >=> quicksortInc (flip cmp) >=> takeInc' i) mxs >>= force

leastkIncM :: (IncK inc (ListMod' thunk l inc r m a),IncK inc (ListMod' mod l inc r m a),Eq a,Eq (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Hashable (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Eq (ListMod mod l inc r m a),Eq (ListMod thunk l inc r m a), m,Memo a,Memo (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Memo (ListMod mod l inc r m a),Thunk mod l inc r m,Memo (ListMod thunk l inc r m a),Output thunk l inc r m,OrdM (l inc r m) a) =>
	Int -> ListMod mod l inc r m a -> l inc r m (ListMod thunk l inc r m a)
leastkIncM i =
	let sort = quicksortIncM
	in memo $ \_ mxs -> (mapInc return >=> sort >=> takeInc' i) mxs >>= force

leastkInc :: (IncK inc (ListMod' thunk l inc r m a),IncK inc (ListMod' mod l inc r m a),Eq (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Hashable (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Eq (ListMod mod l inc r m a),Eq (ListMod thunk l inc r m a),Memo a,Memo (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Memo (ListMod mod l inc r m a),Thunk mod l inc r m,Memo (ListMod thunk l inc r m a),Output thunk l inc r m,Eq a) =>
	(a -> a -> l inc r m Ordering) -> Int -> ListMod mod l inc r m a -> l inc r m (ListMod thunk l inc r m a)
leastkInc cmp i = memo $ \_ mxs -> (mapInc return >=> quicksortInc cmp >=> takeInc' i) mxs >>= force

leastkIncAs :: (Memo name,IncK inc (ListMod' thunk l inc r m a),IncK inc (ListMod' mod l inc r m a),Eq (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Hashable (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Eq (ListMod mod l inc r m a),Eq (ListMod thunk l inc r m a),Memo a,Memo (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Memo (ListMod mod l inc r m a),Thunk mod l inc r m,Memo (ListMod thunk l inc r m a),Output thunk l inc r m,Eq a) =>
	name -> (a -> a -> l inc r m Ordering) -> Int -> ListMod mod l inc r m a -> l inc r m (ListMod thunk l inc r m a)
leastkIncAs name cmp i = memo $ \_ mxs -> (mapInc return >=> quicksortIncAs name cmp >=> takeInc' i) mxs >>= force


type Warehouse = ListM Inside IORef IO Item
data Item = Item { itemName :: String, itemPrice :: M Inside Adapton IORef IO Int, itemInflation :: Int, itemQuantity :: M Inside Adapton IORef IO Int }
	deriving (Typeable,Eq)

data Customer = Customer { customerName :: String, customerBalance :: M Inside Adapton IORef IO Int, customerPurchases :: ListM Inside IORef IO (String,Int) }
	deriving (Typeable,Eq)

instance OrdM (Inside Adapton IORef IO) Item where
	compareM i1 i2 = do
		p1 <- get $ itemPrice i1 
		p2 <- get $ itemPrice i2 
		return $ compare p1 p2

data LeastItem = LeastItem deriving (Typeable,Eq)
instance Hashable LeastItem where
	hash _ = 0
	hashWithSalt salt _ = salt
$(deriveMemoId ''LeastItem)

leastItem = inside . leastkIncAs LeastItem compareM 1
-- finds the cheapest item
cheapestItem :: String -> Warehouse -> ListU Inside IORef IO Item -> Outside Adapton IORef IO Item
cheapestItem msg warehouse leastItem = do
	let wrongQuantity item = do
		str <- showInc item
		error $ "WRONG ITEM " ++ str ++ " " ++ msg
--	showInc warehouse >>= inL . liftIO . putStrLn . ("warehouse: "++)
--	showInc mxs >>= inL . liftIO . putStrLn . ("cheapestItem: "++)
	xs <- forceOutside leastItem
	case xs of
		NilMod -> error $ "NO ITEMS LEFT " ++ msg
		ConsMod item _ -> do
			quantity <- getOutside $ itemQuantity item
			when (quantity <= 0) $ wrongQuantity item
			return item

deleteItem :: Warehouse -> Item -> Outside Adapton IORef IO ()
deleteItem warehouse item = do
	getOutside warehouse >>= \xs -> case xs of
		ConsMod item' warehouse' -> if itemName item == itemName item'
			then getOutside warehouse' >>= set warehouse
			else deleteItem warehouse' item
		NilMod -> error $ "ITEM NOT FOUND "

-- buy an item for a customer and increase the item's price
buyCheapestItem :: String -> Warehouse -> ListU Inside IORef IO Item -> Customer -> Outside Adapton IORef IO Item
buyCheapestItem msg warehouse leastItem customer = do
	item <- cheapestItem msg warehouse leastItem
	let wrongQuantity = do
		str1 <- inside $ showInc customer
		str2 <- inside $ showInc item
		error $ str1 ++ " CAN'T BUY " ++ str2 ++ " " ++ msg
	let noBalance = do
		str1 <- inside $ showInc customer
		str2 <- inside $ showInc item
		error $ str1 ++ " has insufficient funds for " ++ show str2 ++ " " ++ msg
	let buy = do
		balance <- getOutside (customerBalance customer)
		price <- getOutside (itemPrice item)
		-- check balance; if the check fails, it calls retry
		when (price > balance) $ noBalance
		-- inflate item's price
		set (itemPrice item) $ price + itemInflation item
		-- decrease item's quantity
		quantity <- getOutside (itemQuantity item)
		when (quantity == 1) $ deleteItem warehouse item
		when (quantity <= 0) $ wrongQuantity
		set (itemQuantity item) $ pred quantity
		-- decrease customer's balance
		set (customerBalance customer) $ balance - price
		-- add customer's purchase
		modify (customerPurchases customer) $ ref >=> return . ConsMod (itemName item,price)
		return item
	buy

customer_thread :: Int -> Warehouse -> ListU Inside IORef IO Item -> Customer -> Outside Adapton IORef IO ()
customer_thread 0 warehouse leastItem customer = return ()
customer_thread i warehouse leastItem customer = do
		choice <- liftIO $ generate $ choose (False,True)
		if choice
			then do -- list the cheapest item
				(time,item) <- timeOuterT $ do
					item <- cheapestItem ("customer " ++ customerName customer) warehouse leastItem
					str <- showInc item
--					let msg = "customer " ++ customerName customer ++ " found cheapest " ++ str ++ " "
--					drawPDF msg proxyAdapton proxyIORef proxyIO (Merge warehouse leastItem)
					return str
				liftIO $ putStrLn $ "customer " ++ customerName customer ++ " found cheapest " ++ item ++ " in " ++ show time ++ " " 
			else do -- buy the cheapest item
				(time,item) <- timeOuterT $ do
					item <- buyCheapestItem ("customer " ++ customerName customer) warehouse leastItem customer
					str <- showInc item
--					let msg = "customer " ++ customerName customer ++ " bought cheapest " ++ str ++ " "
--					drawPDF msg proxyAdapton proxyIORef proxyIO (Merge warehouse leastItem)
					return str
				liftIO $ putStrLn $ "customer " ++ customerName customer ++ " bought cheapest " ++ item ++ " in " ++ show time ++ " " 
		customer_thread (pred i) warehouse leastItem customer

main = main_Customers
main_Customers = {-flip finally (mergeGraphsInto' "ex.pdf") $ -} do
	let numItems = 10^4
	
	hSetBuffering stdout NoBuffering
	
	runIncremental $ do
		(warehouse,customer) <- genDB numItems
		(thunk::ListU Inside IORef IO Item) <- leastItem warehouse
		customer_thread 20 warehouse thunk customer
	
genDB :: Int -> Outside Adapton IORef IO (Warehouse,Customer)
genDB numItems = do
	let itemIds = [1..numItems]	
	let genItem itemId = do
		let itemName = show itemId
		price <- liftIO $ generate $ choose (1,500)
		itemPrice <- inside $ ref price
		quantity <- liftIO $ generate $ choose (1,2)
		itemQuantity <- inside $ ref quantity
		let itemInflation = price `div` quantity
		return $ Item itemName itemPrice itemInflation itemQuantity
	warehouse <- inside . toListRef =<< mapM genItem itemIds
	let genCustomer customerId = do
		let customerName = show customerId
		customerBalance <- inside . ref =<< inL (liftIO $ generate $ choose (0,maxBound))
		customerPurchases <- inside $ ref NilMod
		return $ Customer customerName customerBalance customerPurchases
	customers <- genCustomer 1
	-- memoize cheapest item query
--	cheapestItem warehouse >>= inside . nfDataInc
	return (warehouse,customers)
	
instance (Display l1 inc r m (M Inside Adapton IORef IO Int)) => Display l1 inc r m Item where
	displaysPrec proxyL proxyInc proxyR proxyM (Item itemName itemPrice itemInflation itemQuantity) r = do
		sq <- displaysPrec proxyL proxyInc proxyR proxyM itemQuantity (')':r)
		si <- displaysPrec proxyL proxyInc proxyR proxyM itemInflation (' ':sq)
		sp <- displaysPrec proxyL proxyInc proxyR proxyM itemPrice (' ':si)
		sn <- displaysPrec proxyL proxyInc proxyR proxyM itemName (' ':sp)
		return $ "(Item " ++ sn

instance (Display l1 inc r m (M Inside Adapton IORef IO Int),Display l1 inc r m (ListM Inside IORef IO (String, Int))) => Display l1 inc r m Customer where
	displaysPrec proxyL proxyInc proxyR proxyM (Customer customerName customerBalance customerPurchases) r = do
		sp <- displaysPrec proxyL proxyInc proxyR proxyM customerPurchases (')':r)
		sb <- displaysPrec proxyL proxyInc proxyR proxyM customerBalance (' ':sp)
		sn <- displaysPrec proxyL proxyInc proxyR proxyM customerName (' ':sb)
		return $ "(Customer " ++ sn

instance (NFDataInc l1 inc r m (M Inside Adapton IORef IO Int)) => NFDataInc l1 inc r m Item where
	nfDataInc proxyL proxyInc proxyR proxyM (Item itemName itemPrice itemInflation itemQuantity) = do
		n <- nfDataInc proxyL proxyInc proxyR proxyM itemName
		p <- nfDataInc proxyL proxyInc proxyR proxyM itemPrice
		i <- nfDataInc proxyL proxyInc proxyR proxyM itemInflation
		q <- nfDataInc proxyL proxyInc proxyR proxyM itemQuantity
		return $ n `seq` p `seq` i `seq` q

instance (NFDataInc l1 inc r m (M Inside Adapton IORef IO Int),NFDataInc l1 inc r m (ListM Inside IORef IO (String, Int))) => NFDataInc l1 inc r m Customer where
	nfDataInc proxyL proxyInc proxyR proxyM (Customer customerName customerBalance customerPurchases) = do
		n <- nfDataInc proxyL proxyInc proxyR proxyM customerName
		b <- nfDataInc proxyL proxyInc proxyR proxyM customerBalance
		p <- nfDataInc proxyL proxyInc proxyR proxyM customerPurchases
		return $ n `seq` b `seq` p
		
$(deriveMemo ''Item)
$(deriveMemo ''Customer)
$( derive makeMData ''Item )
$( derive makeMData ''Customer )
$( derive makeDeepTypeable ''Item )
$( derive makeDeepTypeable ''Customer )
