{-# LANGUAGE TypeFamilies, BangPatterns, Rank2Types, GADTs, DeriveDataTypeable, AllowAmbiguousTypes, MultiParamTypeClasses, TemplateHaskell, ScopedTypeVariables, KindSignatures, ConstraintKinds, FlexibleContexts, StandaloneDeriving, FlexibleInstances, UndecidableInstances #-}

module Main where

import qualified Examples.Customer as Customer

import Data.DeepTypeable
import Data.WithClass.MData
import Data.DeriveTH
import Data.WithClass.Derive.MData
import Data.WithClass.Derive.DeepTypeable
import Language.Haskell.TH.Syntax

import Control.Monad.Incremental.Benchmark
import System.Random
import System.IO
import Control.Monad.Incremental.LazyNonInc
import Control.Monad.Incremental.Adapton
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
import System.Mem.Weak.Exts as Weak

import Test.QuickCheck.Gen
import Test.QuickCheck
import System.IO.Unsafe
import Data.Unique
import System.Mem.StableName.Exts
import Control.Exception

import Data.Monoid
import Data.Hashable
import System.Mem
import Control.Monad.Incremental.Generics
import Control.Monad.Incremental.List
import Control.Monad.Incremental.Tree
import System.TimeIt

import Criterion.Main hiding (run)
import Criterion.Types
import System.Random.Shuffle

genListDeleteInsert :: (IncK inc (ListMod' mod Inside inc Int),Input mod Inside inc)
	=> IncParams inc -> Int -> Int -> IO (ListMod mod Inside inc Int,[ListMod mod Inside inc Int -> Outside inc ()])
genListDeleteInsert params size runs = do
	xs <- generate $ vectorOf size $ choose (minBound,maxBound)
	positionsDelete <- generate $ vectorOf runs $ choose (0,size-1)
	positionsInsert <- generate $ vectorOf runs $ choose (0,size-2)	    
	positions <- merge positionsDelete positionsInsert
	s <- toListModIO xs
	return (s,positions)
  where
	merge [] [] = return []
	merge (x:xs) (y:ys) = do
		tail <- merge xs ys
		v :: Int <- generate $ choose (minBound,maxBound)
		return $ (deleteListModAt x) : (insertListModAt y v) : tail

genListPairDeleteInsert :: (IncK inc (ListMod' mod Inside inc Int),Input mod Inside inc)
	=> IncParams inc -> Int -> Int -> IO ((ListMod mod Inside inc Int,ListMod mod Inside inc Int),[(ListMod mod Inside inc Int,ListMod mod Inside inc Int) -> Outside inc ()])
genListPairDeleteInsert params size runs = do
	(l,chgsl) <- genListDeleteInsert params (size `div` 2) (runs `div` 2)
	(r,chgsr) <- genListDeleteInsert params (size `div` 2) (runs `div` 2)
	chgs <- shuffleM $ (map (\f -> f . fst) chgsl) ++ (map (\f -> f . snd) chgsr)
	return ((l,r),chgs)

-- lazy

$(deriveName "FilterLazyLazyNonIncBench")
filterLazyLazyNonIncBench :: IncBenchmark LazyNonInc (ListMod LazyNonIncM Inside LazyNonInc Int) (ListMod LazyNonIncU Inside LazyNonInc Int) (Maybe Int)
filterLazyLazyNonIncBench = IncBenchmark "FilterLazyLazyNonIncBench" genListDeleteInsert (inside . filterIncAs FilterLazyLazyNonIncBench (return . even)) (inside . headMayInc)

$(deriveName "FilterLazyAdaptonBench")
filterLazyAdaptonBench :: IncBenchmark Adapton (ListMod M Inside Adapton Int) (ListMod U Inside Adapton Int) (Maybe Int)
filterLazyAdaptonBench = IncBenchmark "FilterLazyAdaptonBench" genListDeleteInsert (inside . filterIncAs FilterLazyAdaptonBench (return . even)) (inside . headMayInc)

$(deriveName "MapLazyLazyNonIncBench")
mapLazyLazyNonIncBench :: IncBenchmark LazyNonInc (ListMod LazyNonIncM Inside LazyNonInc Int) (ListMod LazyNonIncU Inside LazyNonInc Int) (Maybe Int)
mapLazyLazyNonIncBench = IncBenchmark "MapLazyLazyNonIncBench" genListDeleteInsert (inside . mapIncAs MapLazyLazyNonIncBench (return . succ)) (inside . headMayInc)

$(deriveName "MapLazyAdaptonBench")
mapLazyAdaptonBench :: IncBenchmark Adapton (ListMod M Inside Adapton Int) (ListMod U Inside Adapton Int) (Maybe Int)
mapLazyAdaptonBench = IncBenchmark "MapLazyAdaptonBench" genListDeleteInsert (inside . mapIncAs MapLazyAdaptonBench (return . succ)) (inside . headMayInc)

$(deriveName "QuicksortLazyLazyNonIncBench")
quicksortLazyLazyNonIncBench :: IncBenchmark LazyNonInc (ListMod LazyNonIncM Inside LazyNonInc Int) (ListMod LazyNonIncU Inside LazyNonInc Int) (Maybe Int)
quicksortLazyLazyNonIncBench = IncBenchmark "QuicksortLazyLazyNonIncBench" genListDeleteInsert (inside . quicksortIncAs' QuicksortLazyLazyNonIncBench  (\x y -> return $ compare x y)) (inside . headMayInc)

$(deriveName "QuicksortLazyAdaptonBench")
quicksortLazyAdaptonBench :: IncBenchmark Adapton (ListMod M Inside Adapton Int) (ListMod U Inside Adapton Int) (Maybe Int)
quicksortLazyAdaptonBench = IncBenchmark "QuicksortLazyAdaptonBench" genListDeleteInsert (inside . quicksortIncAs' QuicksortLazyAdaptonBench (\x y -> return $ compare x y)) (inside . headMayInc)

quicksortLazyAdaptonBench2 :: IncBenchmark Adapton (ListMod M Inside Adapton Int) (ListMod M Inside Adapton Int) (Maybe Int)
quicksortLazyAdaptonBench2 = IncBenchmark "QuicksortLazyAdaptonBench" genListDeleteInsert return $ inside . (quicksortIncAs' QuicksortLazyAdaptonBench (\x y -> return $ compare x y) >=> (headMayInc :: ListMod U Inside Adapton Int -> Inside Adapton (Maybe Int)))

$(deriveName "MergesortLazyLazyNonIncBench")
mergesortLazyLazyNonIncBench :: IncBenchmark LazyNonInc (ListMod LazyNonIncM Inside LazyNonInc Int) (ListMod LazyNonIncU Inside LazyNonInc Int) (Maybe Int)
mergesortLazyLazyNonIncBench = IncBenchmark "MergesortLazyLazyNonIncBench" genListDeleteInsert (inside . quicksortIncAs' MergesortLazyLazyNonIncBench  (\x y -> return $ compare x y)) (inside . headMayInc)

$(deriveName "MergesortLazyAdaptonBench")
mergesortLazyAdaptonBench :: IncBenchmark Adapton (ListMod M Inside Adapton Int) (ListMod U Inside Adapton Int) (Maybe Int)
mergesortLazyAdaptonBench = IncBenchmark "MergesortLazyAdaptonBench" genListDeleteInsert (inside . mergesortIncAs' MergesortLazyAdaptonBench (\x y -> return $ compare x y)) (inside . headMayInc)

-- batch

$(deriveName "FilterBatchLazyNonIncBench")
filterBatchLazyNonIncBench :: IncBenchmark LazyNonInc (ListMod LazyNonIncM Inside LazyNonInc Int) (ListMod LazyNonIncU Inside LazyNonInc Int) (ListMod LazyNonIncU Inside LazyNonInc Int)
filterBatchLazyNonIncBench = IncBenchmark "FilterBatchLazyNonIncBench" genListDeleteInsert (inside . filterIncAs FilterBatchLazyNonIncBench (return . even)) (inside . return)

$(deriveName "FilterBatchAdaptonBench")
filterBatchAdaptonBench :: IncBenchmark Adapton (ListMod M Inside Adapton Int) (ListMod U Inside Adapton Int) (ListMod U Inside Adapton Int)
filterBatchAdaptonBench = IncBenchmark "FilterBatchAdaptonBench" genListDeleteInsert (inside . filterIncAs FilterBatchAdaptonBench (return . even)) (inside . return)

$(deriveName "MapBatchLazyNonIncBench")
mapBatchLazyNonIncBench :: IncBenchmark LazyNonInc (ListMod LazyNonIncM Inside LazyNonInc Int) (ListMod LazyNonIncU Inside LazyNonInc Int) (ListMod LazyNonIncU Inside LazyNonInc Int)
mapBatchLazyNonIncBench = IncBenchmark "MapBatchLazyNonIncBench" genListDeleteInsert (inside . mapIncAs MapBatchLazyNonIncBench (return . succ)) (inside . return)

$(deriveName "MapBatchAdaptonBench")
mapBatchAdaptonBench :: IncBenchmark Adapton (ListMod M Inside Adapton Int) (ListMod U Inside Adapton Int) (ListMod U Inside Adapton Int)
mapBatchAdaptonBench = IncBenchmark "MapBatchAdaptonBench" genListDeleteInsert (inside . mapIncAs MapBatchAdaptonBench (return . succ)) (inside . return)

$(deriveName "FoldMinBatchLazyNonIncBench")
foldMinBatchLazyNonIncBench :: IncBenchmark LazyNonInc (ListMod LazyNonIncM Inside LazyNonInc Int) (LazyNonIncU Inside LazyNonInc Int) (LazyNonIncU Inside LazyNonInc Int)
foldMinBatchLazyNonIncBench = IncBenchmark "FoldMinBatchLazyNonIncBench" genListDeleteInsert (inside . fold1IncAs' FoldMinBatchLazyNonIncBench (\x y -> return $ min x y)) (inside . return)

$(deriveName "FoldMinBatchAdaptonBench")
foldMinBatchAdaptonBench :: IncBenchmark Adapton (ListMod M Inside Adapton Int) (U Inside Adapton Int) (U Inside Adapton Int)
foldMinBatchAdaptonBench = IncBenchmark "FoldMinBatchAdaptonBench" genListDeleteInsert (inside . fold1IncAs' FoldMinBatchAdaptonBench (\x y -> return $ min x y)) (inside . return)

$(deriveName "FoldSumBatchLazyNonIncBench")
foldSumBatchLazyNonIncBench :: IncBenchmark LazyNonInc (ListMod LazyNonIncM Inside LazyNonInc Int) (LazyNonIncU Inside LazyNonInc Int) (LazyNonIncU Inside LazyNonInc Int)
foldSumBatchLazyNonIncBench = IncBenchmark "FoldSumBatchLazyNonIncBench" genListDeleteInsert (inside . fold1IncAs' FoldSumBatchLazyNonIncBench (\x y -> return $ x + y)) (inside . return)

$(deriveName "FoldSumBatchAdaptonBench")
foldSumBatchAdaptonBench :: IncBenchmark Adapton (ListMod M Inside Adapton Int) (U Inside Adapton Int) (U Inside Adapton Int)
foldSumBatchAdaptonBench = IncBenchmark "FoldSumBatchAdaptonBench" genListDeleteInsert (inside . fold1IncAs' FoldSumBatchAdaptonBench (\x y -> return $ x + y)) (inside . return)


$(deriveName "MergeBatchAdaptonBench")
mergeBatchAdaptonBench :: IncBenchmark Adapton (ListMod M Inside Adapton Int,ListMod M Inside Adapton Int) (ListMod U Inside Adapton Int) (ListMod U Inside Adapton Int)
mergeBatchAdaptonBench = IncBenchmark "MergeBatchAdaptonBench" genListPairDeleteInsert (inside . uncurry (mergeIncAs' MergeBatchAdaptonBench (\x y -> return $ compare x y))) (inside . return)

mainBench = do
	-- lazy pattern
--	runIncBenchmark filterLazyLazyNonIncBench defaultIncParams (10^6) 0
--	runIncBenchmark filterLazyAdaptonBench (defaultIncParams { adaptonMemoSize = 10^6 }) (10^6) 250
--	runIncBenchmark mapLazyLazyNonIncBench defaultIncParams (10^6) 0
--	runIncBenchmark mapLazyAdaptonBench (defaultIncParams { adaptonMemoSize = 10^6 }) (10^6) 250
--	runIncBenchmark quicksortLazyLazyNonIncBench defaultIncParams (10^5) 0
--	runIncBenchmark2 quicksortLazyAdaptonBench (defaultIncParams { adaptonMemoSize = 10^5 }) (10^5) 20
--	runIncBenchmark mergesortLazyLazyNonIncBench defaultIncParams (10^5) 20
	runIncBenchmark mergesortLazyAdaptonBench (defaultIncParams { adaptonMemoSize = 10^4 }) (10^4) 250
	-- batch pattern
--	runIncBenchmark filterBatchLazyNonIncBench defaultIncParams (10^6) 0
--	runIncBenchmark filterBatchAdaptonBench (defaultIncParams { adaptonMemoSize = 10^6 }) (10^6) 250
--	runIncBenchmark mapBatchLazyNonIncBench defaultIncParams (10^6) 0
--	runIncBenchmark mapBatchAdaptonBench (defaultIncParams { adaptonMemoSize = 10^6 }) (10^6) 250
--	runIncBenchmark foldMinBatchLazyNonIncBench defaultIncParams (10^6) 0
--	runIncBenchmark foldMinBatchAdaptonBench (defaultIncParams { adaptonMemoSize = 10^6 }) (10^6) 250
--	runIncBenchmark foldSumBatchLazyNonIncBench defaultIncParams (10^6) 0
--	runIncBenchmark foldSumBatchAdaptonBench (defaultIncParams { adaptonMemoSize = 10^6 }) (10^6) 250

--main = Customer.main

main = mainBench















