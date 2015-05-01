{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Control.Concurrent.Transactional.Benchmark where

import Data.Proxy
import Control.Monad.Incremental
import Control.Concurrent.Transactional
import System.TimeIt
import Control.Monad.Incremental.Display
import Control.Concurrent.Async
import Data.List
import Debug
import System.IO
import Control.Concurrent.Chan
import System.Mem
import Control.Concurrent

-- | The type for incremental benchmarks
data TxBenchmark
	inc
	a -- source type
	b -- per-run argument
	t -- thread argument type
	= TxBenchmark
	{	txBenchName :: String -- benchmark name
	,	txBenchData :: IncParams inc -> Int -> Int -> IO (a,[b]) -- test data generation
	,	txBenchThreads :: IncParams inc -> Int -> a -> IO [t] -- create threads with thread-local arguments
	,	txBenchView :: IncParams inc -> a -> b -> t -> IO () -- individual thread-local computation
	}

splitListAt :: Int -> [a] -> [[a]]
splitListAt i xs = let (l,r) = splitAt i xs in case r of
	[] -> [l]
	r -> l : splitListAt i r

-- | Runs an incremental transactional benchmark
runTxBenchmark :: TxBenchmark inc a b t -> IncParams inc -> Int -> Int -> Int -> IO Double
runTxBenchmark bench (params :: IncParams inc) size runs threads = do
	let inc = Proxy :: Proxy inc
	
	writeChan debugChan $ "generating benchmark data " ++ show (txBenchName bench)
	(s,chgs) <- txBenchData bench params size (runs * threads)
	let l = length chgs
	let chgss = splitListAt (l `div` threads) chgs
	writeChan debugChan $ "generating benchmark threads " ++ show (txBenchName bench)
	ts <- txBenchThreads bench params threads s
	let incThreadLoop t chgs = case chgs of
		[] -> return ()
		(b:bs) -> do
			txBenchView bench params s b t
			incThreadLoop t bs
	let incThread t chgs = incThreadLoop t chgs

	let runThreads = do
		(time,_) <- timeItT $ mapConcurrently (uncurry incThread) $ zip ts chgss
		writeChan debugChan $ "total time: " ++ show time
		return time

	writeChan debugChan $ "running benchmark " ++ show (txBenchName bench)
	runThreads
	


