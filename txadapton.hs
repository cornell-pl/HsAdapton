{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Main where

import Control.Monad.Incremental
import Control.Monad.Incremental.List
import Control.Monad.Transactional
import Control.Monad.Transactional.STM
import Control.Monad.Transactional.TxAdapton
import Control.Monad.Incremental.Display
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Incremental.Adapton
import Control.Monad.Incremental.Draw

import Data.Proxy
import Data.IORef

import Test.QuickCheck.Gen
import Test.QuickCheck
import System.TimeIt
import System.Mem
import System.Random
import System.IO
import Control.Concurrent.Chan
import Control.Exception as Exception
import Control.Monad.Catch as Catch

toListRef :: (
	Eq a,Eq (ListMod mod l inc r m a),Input mod l inc r m) => [a] -> l inc r m (ListMod mod l inc r m a)
toListRef [] = ref NilMod
toListRef (x:xs) = do
	mxs <- toListRef xs
	ref (ConsMod x mxs)

setListModAt :: (Eq a,Eq (ListMod mod l inc r m a),Input mod l inc r m,Layer Outside inc r m)
	=> ListMod mod l inc r m a -> Int -> (ListMod' mod l inc r m a -> Outside inc r m (ListMod' mod l inc r m a)) -> Outside inc r m ()
setListModAt mxs 0 f = getOutside mxs >>= f >>= set mxs
setListModAt mxs i f = getOutside mxs >>= \xs -> case xs of
	ConsMod x mxs -> setListModAt mxs (pred i) f
	NilMod -> error "position not found"

-- generates a random list with a certain length
genList :: Int -> IO [Int]
genList i = do
	xs <- generate $ vectorOf i $ choose (minBound,0)
	return xs

topkInc :: (Eq (ListMod mod l inc r m a),Eq (ListMod thunk l inc r m a),MonadIO m,Memo (ListMod mod l inc r m a),Thunk mod l inc r m,Memo (ListMod thunk l inc r m a),Output thunk l inc r m,Ord a) =>
	Int -> ListMod mod l inc r m a -> l inc r m (ListMod thunk l inc r m a)
topkInc i = memo $ \_ mxs -> (mapInc return >=> quicksortInc (flip compare) >=> takeInc' i) mxs >>= force

-- * concurrent non-incremental code

main_STM = do
	let (size::Int) = 10^5
	hSetBuffering stdout NoBuffering
	forkIO debugger -- for synchronous output
	
	xs <- genList size
	db <- atomically $ inside $ toListRef xs
	-- sparks two new threads
	concurrently (changeDB_STM size db) (readDB_STM db)
	return ()

-- an infinite loop that reads the up-to-date top-k
readDB_STM :: ListMod IncTVar Inside IncSTM IORef IO Int -> IO ()
readDB_STM db = do
	forever $ do
		(time,topk) <- timeItT $ atomically $ inside $ do
			(topk_thunk :: ListMod IncUVar Inside IncSTM IORef IO Int) <- topkInc 3 db
			showInc topk_thunk
		writeChan debugChan $ "top3: " ++ topk ++ " in " ++ show time
		threadDelay 1000000

-- an infinite loop that changes the database
changeDB_STM :: Int -> ListMod IncTVar Inside IncSTM IORef IO Int -> IO ()
changeDB_STM size db = do
	forever $ do
		p <- generate $ choose (0,size-1)
		i <- generate $ choose (minBound,maxBound)
		(time,topk) <- timeItT $ atomically $ do
			setListModAt db p $ \(ConsMod x mxs) -> return $ ConsMod i mxs
			(topk_thunk :: ListMod IncUVar Inside IncSTM IORef IO Int) <- inside $ topkInc 3 db
			inside $ showInc topk_thunk
		writeChan debugChan $ "addedtop3: " ++ show i ++" "++topk ++ " in " ++ show time
		threadDelay 1000000
	return ()

-- * concurrent incremental code

main_TxAdapton = flip Exception.finally (mergePDFsInto' "tx.pdf") $ do
	let (size::Int) = 5
	hSetBuffering stdout NoBuffering
	
	xs <- genList size
	db <- atomically $ inside $ toListRef xs
	-- sparks two new threads
	race_ debugger $ concurrently (changeDB_TxAdapton size db) (readDB_TxAdapton db)
	return ()

-- an infinite loop that reads the up-to-date top-k
readDB_TxAdapton :: ListMod TxM Inside TxAdapton IORef IO Int -> IO ()
readDB_TxAdapton db = do
	replicateM_ 0 $ do
		(time,(topk,topk_thunk)) <- timeItT $ atomicallyTxMsg "read" $ do
			(topk_thunk :: ListMod TxU Inside TxAdapton IORef IO Int) <- topkI db
			topk <- inside $ showInc topk_thunk
--			drawPDF proxyTxAdapton proxyIORef proxyIO (Merge db topk_thunk)
			return (topk,topk_thunk)
		writeChan debugChan $ "top3: " ++ topk ++ " in " ++ show time
--		atomically $ drawPDF proxyTxAdapton proxyIORef proxyIO (Merge db topk_thunk)
		threadDelay 1000000

--mapI = inside . mapInc return
topkI = inside . topkInc 3

-- an infinite loop that changes the database
changeDB_TxAdapton :: Int -> ListMod TxM Inside TxAdapton IORef IO Int -> IO ()
changeDB_TxAdapton size db = do
	replicateM_ 4 $ do
		p <- generate $ choose (0,size-1)
		i <- generate $ choose (0,maxBound)
		(time,(topk,topk_thunk)) <- timeItT $ atomicallyTxMsg "change" $ do
			setListModAt db p $ \(ConsMod x mxs) -> return $ ConsMod i mxs
			drawPDF proxyTxAdapton proxyIORef proxyIO db
			(topk_thunk :: ListMod TxU Inside TxAdapton IORef IO Int) <- topkI db
			topk <- inside $ showInc topk_thunk
			drawPDF proxyTxAdapton proxyIORef proxyIO (Merge db topk_thunk)
			return (topk,topk_thunk)
		writeChan debugChan $ "addedtop3: " ++ show i ++" "++topk ++ " in " ++ show time
		atomically $ drawPDF proxyTxAdapton proxyIORef proxyIO (Merge db topk_thunk)
		threadDelay 1000000
	return ()

-- * sequential incremental code

main_Adapton = flip Exception.finally (mergePDFsInto' "tx.pdf") $ runIncremental $ do
	let (size::Int) = 5
	xs <- inL $ liftIO $ genList size
	db <- inside $ toListRef xs
	
--	let topk_func = topkInc 3 -- we need to use the same function to enable memoization
	
--	drawPDF proxyAdapton proxyIORef proxyIO db
	
	loop_Adapton size db

loop_Adapton :: Int -> ListMod M Inside Adapton IORef IO Int -> Outside Adapton IORef IO ()
loop_Adapton size db = replicateM_ 4 $ do
	(time,()) <- timeOuterT $ do
		p <- inL $ liftIO $ generate $ choose (0,size-1)
		i <- inL $ liftIO $ generate $ choose (0,maxBound)
		setListModAt db p $ \(ConsMod x mxs) -> return $ ConsMod i mxs
		
		drawPDF proxyAdapton proxyIORef proxyIO db
		
		inL $ liftIO $ putStrLn $ "added to db " ++ show i
		(topk :: ListMod U Inside Adapton IORef IO Int) <- topkII db
		inside $ displayAs "top3: " topk
		
		drawPDF proxyAdapton proxyIORef proxyIO (Merge db topk)
		
	inL $ liftIO $ putStrLn ("CPU "++show time)

timeOuterT :: Outer r IO a -> Outer r IO (Double,a)
timeOuterT (Outer m) = Outer $ timeItT m

--mapII = inside . mapInc return
topkII = inside . topkInc 3

main = main_TxAdapton



		
	