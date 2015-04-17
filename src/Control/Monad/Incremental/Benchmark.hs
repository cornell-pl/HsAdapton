{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Control.Monad.Incremental.Benchmark where

import Data.Proxy
import Control.Monad.Incremental
import System.TimeIt
import Control.Monad.Incremental.Display
import Control.Concurrent.Async
import Data.List
import Debug
import System.IO

-- | The type for incremental benchmarks
data IncBenchmark
	inc -- incremental class
	a -- source type
	b -- target type
	r -- result type
	= IncBenchmark
	{	incBenchName :: String -- benchmark name
	,	incBenchData :: IncParams inc -> Int -> Int -> IO (a,[a -> Outside inc ()]) -- test data generation
	,	incBenchComp :: a -> Outside inc b -- incremental computation
	,	incBenchView :: b -> Outside inc r -- evaluate the result
	}

-- | Runs an incremental benchmark
runIncBenchmark :: (NFDataInc Outside inc r)
	=> IncBenchmark inc a b r -> IncParams inc -> Int -> Int -> IO (Double,Double)
runIncBenchmark bench (params :: IncParams inc) size runs = do
	let inc = Proxy :: Proxy inc
	putStrLn $ "running benchmark " ++ show (incBenchName bench)
	(s,chgs) <- incBenchData bench params size runs
	let l = length chgs
	(scratchTime,t) <- timeItT $ runIncrementalWithParamsProxy inc params $ do
		t <- incBenchComp bench s
		incBenchView bench t >>= rnfInc
		return t
	putStrLn $ "scratch: " ++ show scratchTime
	let doChanges chgs = case chgs of
		[] -> return ()
		(chg:chgs) -> do
			chg s
			incBenchView bench t >>= rnfInc
	(incTime,()) <- timeItT $ runIncrementalWithParamsProxy inc params $ doChanges chgs
	putStrLn $ "inc * " ++ show l ++ ": " ++ show (incTime / toEnum l)
	return (scratchTime,incTime)

-- | evaluate each change separatedly
runIncBenchmark2 :: (NFDataInc Outside inc r)
	=> IncBenchmark inc a b r -> IncParams inc -> Int -> Int -> IO (Double,Double)
runIncBenchmark2 bench (params :: IncParams inc) size runs = do
	let inc = Proxy :: Proxy inc
	putStrLn $ "running benchmark " ++ show (incBenchName bench)
	(s,chgs) <- incBenchData bench params size runs
	let l = length chgs
	(scratchTime,t) <- timeItT $ runIncrementalWithParamsProxy inc params $ do
		t <- incBenchComp bench s
		incBenchView bench t >>= rnfInc
		return t
	putStrLn $ "scratch: " ++ show scratchTime
	let doChanges chgs = case chgs of
		[] -> return 0
		(chg:chgs) -> do
			(time,_) <- timeItT $ runIncrementalWithParamsProxy inc params $ chg s >> incBenchView bench t >>= rnfInc
			putStrLn $ "inc: " ++ show time
			times <- doChanges chgs
			return (times + times :: Double)
			
	incTime <- doChanges chgs
	putStrLn $ "inc * " ++ show l ++ ": " ++ show (incTime / toEnum l)
	return (scratchTime,incTime)


--main = do
--	-- lazy
--	let size = 10^5
--	let runs = 10
--	(s :: ListM Inside Int,chgs) <- genListDeleteInsert size runs
--	let sort = listMUInc >=> quicksortIncAs SortInt (\x y -> return $ compare x y) >=> takeInc' 1
--	t <- runIncremental $ inside $ sort s
--	defaultMainWith myConfig
--		[	bgroup "lazy"
--			[	bgroup "quicksort"
--				[	bench "Adapton Scratch" $ whnfIO $ runOuter $ timeOuter $ rnfInc t
--				,	bench ("Adapton Incremental " ++ show (runs*2)) $ whnfIO $ runOuter $ runListTest s t chgs
--				]
--			]
--		]
