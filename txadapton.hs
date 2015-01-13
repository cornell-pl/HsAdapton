{-# LANGUAGE RankNTypes, GADTs, BangPatterns, TemplateHaskell, TupleSections, TypeFamilies, UndecidableInstances, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables, FlexibleContexts #-}

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
import Data.Typeable
import Data.DeepTypeable
import Language.Haskell.TH.Syntax
import System.Mem.WeakTable (stableName)

import Data.WithClass.MData
import Data.DeriveTH
import Data.WithClass.Derive.MData
import Data.WithClass.Derive.DeepTypeable

import Test.QuickCheck.Gen
import Test.QuickCheck
import System.TimeIt
import System.Mem
import System.Random
import System.IO
import Control.Concurrent.Chan
import Control.Exception (Exception(..),SomeException(..))
import qualified Control.Exception as Exception
import qualified Control.Monad.Catch as Catch
import System.Mem.Weak as Weak
import System.Mem.WeakKey as WeakKey
import System.IO.Unsafe
import System.Mem.StableName

toListRef :: (Typeable a,
	Eq a,Eq (ListMod mod l inc r m a),Input mod l inc r m) => [a] -> l inc r m (ListMod mod l inc r m a)
toListRef [] = ref NilMod
toListRef (x:xs) = do
	mxs <- toListRef xs
	ref (ConsMod x mxs)

setListModAt :: (Typeable a,Eq a,Eq (ListMod mod l inc r m a),Input mod l inc r m,Layer Outside inc r m)
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

topkInc :: (Typeable a,Eq (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Hashable (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Eq (ListMod mod l inc r m a),Eq (ListMod thunk l inc r m a),MonadIO m,Memo a,Memo (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Memo (ListMod mod l inc r m a),Thunk mod l inc r m,Memo (ListMod thunk l inc r m a),Output thunk l inc r m,Ord a) =>
	(a -> a -> l inc r m Ordering) -> Int -> ListMod mod l inc r m a -> l inc r m (ListMod thunk l inc r m a)
topkInc cmp i =
	let sortInverse = quicksortInc (flip cmp)
	in memo $ \_ mxs -> (mapInc return >=> sortInverse >=> takeInc' i) mxs >>= force

leastkIncM :: (Typeable a,Eq a,Eq (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Hashable (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Eq (ListMod mod l inc r m a),Eq (ListMod thunk l inc r m a),MonadIO m,Memo a,Memo (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Memo (ListMod mod l inc r m a),Thunk mod l inc r m,Memo (ListMod thunk l inc r m a),Output thunk l inc r m,OrdM (l inc r m) a) =>
	Int -> ListMod mod l inc r m a -> l inc r m (ListMod thunk l inc r m a)
leastkIncM i =
	let sort = quicksortIncM
	in memo $ \_ mxs -> (mapInc return >=> sort >=> takeInc' i) mxs >>= force

leastkInc :: (Typeable a,Eq (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Hashable (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Eq (ListMod mod l inc r m a),Eq (ListMod thunk l inc r m a),MonadIO m,Memo a,Memo (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Memo (ListMod mod l inc r m a),Thunk mod l inc r m,Memo (ListMod thunk l inc r m a),Output thunk l inc r m,Eq a) =>
	(a -> a -> l inc r m Ordering) -> Int -> ListMod mod l inc r m a -> l inc r m (ListMod thunk l inc r m a)
leastkInc cmp i =
	let sort = quicksortInc cmp
	in memo $ \_ mxs -> (mapInc return >=> sort >=> takeInc' i) mxs >>= force

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
			(topk_thunk :: ListMod IncUVar Inside IncSTM IORef IO Int) <- topkInc (\x y -> return $ compare x y) 3 db
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
			(topk_thunk :: ListMod IncUVar Inside IncSTM IORef IO Int) <- inside $ topkInc (\x y -> return $ compare x y) 3 db
			inside $ showInc topk_thunk
		writeChan debugChan $ "addedtop3: " ++ show i ++" "++topk ++ " in " ++ show time
		threadDelay 1000000
	return ()

-- * concurrent incremental code

main_TxAdapton = {- flip Exception.finally (mergePDFsInto' "tx.pdf") $ -} do
	let (size::Int) = 10^3
	hSetBuffering stdout NoBuffering
	
	xs <- genList size
	db <- atomically $ inside $ toListRef xs
	-- sparks two new threads
	race_ debugger $ concurrently (changeDB_TxAdapton size db) (readDB_TxAdapton db)
	return ()

-- an infinite loop that reads the up-to-date top-k
readDB_TxAdapton :: ListMod TxM Inside TxAdapton IORef IO Int -> IO ()
readDB_TxAdapton db = do
	replicateM_ 4 $ do
		(time,(topk,topk_thunk)) <- timeItT $ atomically $ do
			(topk_thunk :: ListMod TxU Inside TxAdapton IORef IO Int) <- topkI db
			topk <- inside $ showInc topk_thunk
--			drawPDF proxyTxAdapton proxyIORef proxyIO (Merge db topk_thunk)
			return (topk,topk_thunk)
		writeChan debugChan $ "top3: " ++ topk ++ " in " ++ show time
--		atomically $ drawPDF proxyTxAdapton proxyIORef proxyIO (Merge db topk_thunk)
		threadDelay $ 300000

--mapI = inside . mapInc return
topkI = inside . topkInc (\x y -> return $ compare x y) 3

-- an infinite loop that changes the database
changeDB_TxAdapton :: Int -> ListMod TxM Inside TxAdapton IORef IO Int -> IO ()
changeDB_TxAdapton size db = do
	replicateM_ 4 $ do
		p <- generate $ choose (0,size-1)
		i <- generate $ choose (0,maxBound)
		(time,(topk,topk_thunk)) <- timeItT $ atomically $ do
			setListModAt db p $ \(ConsMod x mxs) -> return $ ConsMod i mxs
--			drawPDF proxyTxAdapton proxyIORef proxyIO db
			(topk_thunk :: ListMod TxU Inside TxAdapton IORef IO Int) <- topkI db
			topk <- inside $ showInc topk_thunk
--			drawPDF proxyTxAdapton proxyIORef proxyIO (Merge db topk_thunk)
			return (topk,topk_thunk)
		writeChan debugChan $ "addedtop3: " ++ show i ++" "++topk ++ " in " ++ show time
--		atomically $ drawPDF proxyTxAdapton proxyIORef proxyIO (Merge db topk_thunk)
		threadDelay $ 300000
	return ()

-- * sequential incremental code

-- inverse comparison for our topk example
instance Monad m => OrdM m Int where
	compareM x y = return $ compare y x

main_Adapton = flip Exception.finally (mergePDFsInto' "tx.pdf") $ runIncremental $ do
	let (size::Int) = 10^4
	xs <- inL $ liftIO $ genList size
	db <- inside $ toListRef xs
	
	(time,topk) <- timeOuterT $ do
		let topk_func = inside . leastkIncM 3
--		let topk_func = inside . topkInc (\x y -> return $ compare x y) 3
		topk <- topk_func db -- we need to use the same function to enable memoization
		inside $ displayAs "top3: " topk
		return topk
	inL $ liftIO $ putStrLn ("CPU "++show time)
	
--	drawPDF "" proxyAdapton proxyIORef proxyIO (Merge db topk)
	
	loop_Adapton 10 size db topk

loop_Adapton :: Int -> Int -> ListMod M Inside Adapton IORef IO Int -> ListMod U Inside Adapton IORef IO Int -> Outside Adapton IORef IO ()
loop_Adapton 0 size db topk = return ()
loop_Adapton i size db topk = do
	(time,()) <- timeOuterT $ do
		p <- inL $ liftIO $ generate $ choose (0,size-1)
		i <- inL $ liftIO $ generate $ choose (0,maxBound)
		setListModAt db p $ \(ConsMod x mxs) -> return $ ConsMod i mxs
		
--		drawPDF (show i) proxyAdapton proxyIORef proxyIO (Merge db topk)
		
		inL $ liftIO $ putStrLn $ "added to db " ++ show i
		inside $ displayAs "top3: " topk
--		inside $ rnfInc topk
		
--		drawPDF (show i) proxyAdapton proxyIORef proxyIO (Merge db topk)
		
	inL $ liftIO $ putStrLn ("CPU "++show time)
	
	loop_Adapton (pred i) size db topk

timeOuterT :: Outer r IO a -> Outer r IO (Double,a)
timeOuterT (Outer m) = Outer $ timeItT m

--topk3 = inside . topkInc (\x y -> return $ compare x y) 3

-- * Customer example

type ListTxAdaptonU a = ListMod TxU Inside TxAdapton IORef IO a
type TxAdaptonU a = TxU Inside TxAdapton IORef IO a
type ListTxAdaptonM a = ListMod TxM Inside TxAdapton IORef IO a
type TxAdaptonM a = TxM Inside TxAdapton IORef IO a

type Warehouse = ListTxAdaptonM Item
data Item = Item { itemName :: String, itemPrice :: TxAdaptonM Int, itemInflation :: Int, itemQuantity :: TxAdaptonM Int }
	deriving (Typeable,Eq)

data Customer = Customer { customerName :: String, customerBalance :: TxAdaptonM Int, customerPurchases :: ListTxAdaptonM (String,Int) }
	deriving (Typeable,Eq)

-- compare items by their price
instance OrdM (Inside TxAdapton IORef IO) Item where
	compareM i1 i2 = do
		p1 <- get $ itemPrice i1 
		p2 <- get $ itemPrice i2 
		return $ compare p1 p2

leastItem = inside . leastkIncM 1
-- finds the cheapest item
cheapestItem :: String -> Warehouse -> ListTxAdaptonU Item -> STxAdaptonM Item
cheapestItem msg warehouse leastItem = do
	let wrongQuantity item = do
		time <- readTxTime
		str <- showInc item
		throw $ NoBalance $ "WRONG ITEM " ++ str ++ " " ++ show time ++ " " ++ msg
--	showInc warehouse >>= debugTx . ("warehouse: "++)
--	showInc leastItem >>= debugTx . ("cheapestItem: "++)
	xs <- forceOutside leastItem
	case xs of
		NilMod -> throw $ NoBalance $ "NO ITEMS LEFT " ++ msg
		ConsMod item _ -> do
			quantity <- getOutside $ itemQuantity item
			when (quantity <= 0) $ wrongQuantity item
			return item

deleteItem :: Warehouse -> Item -> STxAdaptonM ()
deleteItem warehouse item = do
	getOutside warehouse >>= \xs -> case xs of
		ConsMod item' warehouse' -> if itemName item == itemName item'
			then getOutside warehouse' >>= set warehouse
			else deleteItem warehouse' item
		NilMod -> error $ "ITEM NOT FOUND "

-- buy an item for a customer and increase the item's price
buyCheapestItem :: String -> Warehouse -> ListTxAdaptonU Item -> Customer -> STxAdaptonM Item
buyCheapestItem msg warehouse leastItem customer = do
	item <- cheapestItem msg warehouse leastItem
	let wrongQuantity = do
		str1 <- inside $ showInc customer
		str2 <- inside $ showInc item
		throw $ NoBalance $ str1 ++ " CAN'T BUY " ++ str2 ++ " " ++ msg
	let buy = do
		balance <- getOutside (customerBalance customer)
		price <- getOutside (itemPrice item)
		-- check balance; if the check fails, it calls retry
		guard (price <= balance)
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
	let noBalance = debugM' ("no balance") $ do
		str1 <- inside $ showInc customer
		str2 <- inside $ showInc item
		throw $ NoBalance $ str1 ++ " has insufficient funds for " ++ show str2 ++ " " ++ msg
	buy `orElse` noBalance

data NoBalance = NoBalance String deriving (Show,Typeable)
instance Exception NoBalance

customer_thread :: Warehouse -> ListTxAdaptonU Item -> Customer -> IO ()
customer_thread warehouse leastItem customer = do
	let noBalance (NoBalance msg) = do
--		drawPDF ("customer exception" ++ customerName customer) proxyTxAdapton proxyIORef proxyIO (Merge warehouse leastItem)
		throw (NoBalance msg)
	let action = do
		choice <- generate $ choose (False,True)
		if choice
			then do -- list the cheapest item
				(time,(tx,item)) <- timeItT $ atomicallyTx ("customer " ++ customerName customer) $ flip catch noBalance $ do
					tx <- readTxTime
--					drawPDF ("customer listing" ++ customerName customer) proxyTxAdapton proxyIORef proxyIO (Merge warehouse leastItem)
					item <- cheapestItem ("customer " ++ customerName customer) warehouse leastItem
					str <- showInc item
--					let msg = "customer " ++ customerName customer ++ " found cheapest " ++ str ++ " " ++ show tx
--					drawPDF msg proxyTxAdapton proxyIORef proxyIO (Merge warehouse leastItem)
					return (tx,str)
				writeChan debugChan $ "customer " ++ customerName customer ++ " found cheapest " ++ item ++ " in " ++ show time ++ " " ++ show tx
			else do -- buy the cheapest item
				(time,(tx,item)) <- timeItT $ atomicallyTx ("customer "++ customerName customer) $ flip catch noBalance $ do
					tx <- readTxTime
--					drawPDF ("customer buying" ++ customerName customer) proxyTxAdapton proxyIORef proxyIO (Merge warehouse leastItem)
					item <- buyCheapestItem ("customer " ++ customerName customer) warehouse leastItem customer
					str <- showInc item
--					let msg = "customer " ++ customerName customer ++ " bought cheapest " ++ str ++ " " ++ show tx
--					drawPDF msg proxyTxAdapton proxyIORef proxyIO (Merge warehouse leastItem)
					return (tx,str)
				writeChan debugChan $ "customer " ++ customerName customer ++ " bought cheapest " ++ item ++ " in " ++ show time ++ " " ++ show tx
		threadDelay 300
		action
	let noBalance2 (NoBalance msg) = debugTx' msg
	action `Catch.catch` noBalance2

main = main_Customers
main_Customers = {-flip Exception.finally (mergePDFsInto' "tx.pdf") $ -}do
	let numItems = 10^3
	let numCustomers = 5
	
	hSetBuffering stdout NoBuffering
	
	(warehouse,leastItem,customers) <- atomically $ genDB numItems numCustomers
	
	concurrently debugger $ mapConcurrently (customer_thread warehouse leastItem) customers
	return ()
	
genDB :: Int -> Int -> STxAdaptonM (Warehouse,ListTxAdaptonU Item,[Customer])
genDB numItems numCustomers = do
	let itemIds = [1..numItems]	
	let genItem itemId = do
		let itemName = show itemId
		price <- inL $ liftIO $ generate $ choose (1,500)
		itemPrice <- inside $ ref price
		quantity <- inL $ liftIO $ generate $ choose (1,2)
		itemQuantity <- inside $ ref quantity
		let itemInflation = price `div` quantity
		return $ Item itemName itemPrice itemInflation itemQuantity
	warehouse <- inside . toListRef =<< mapM genItem itemIds
	let customerIds = [1..numCustomers]
	let genCustomer customerId = do
		let customerName = show customerId
		customerBalance <- inside . ref =<< inL (liftIO $ generate $ choose (0,maxBound))
		customerPurchases <- inside $ ref NilMod
		return $ Customer customerName customerBalance customerPurchases
	customers <- mapM genCustomer customerIds
	-- memoize cheapest item query
--	cheapestItem warehouse >>= inside . rnfInc
	thunk <- leastItem warehouse
	return (warehouse,thunk,customers)
	
instance (Display l1 inc r m (TxAdaptonM Int)) => Display l1 inc r m Item where
	displaysPrec (Item itemName itemPrice itemInflation itemQuantity) r = do
		sq <- displaysPrec itemQuantity (')':r)
		si <- displaysPrec itemInflation (' ':sq)
		sp <- displaysPrec itemPrice (' ':si)
		sn <- displaysPrec itemName (' ':sp)
		return $ "(Item " ++ sn

instance (Display l1 inc r m (TxAdaptonM Int),Display l1 inc r m (ListTxAdaptonM (String, Int))) => Display l1 inc r m Customer where
	displaysPrec (Customer customerName customerBalance customerPurchases) r = do
		sp <- displaysPrec customerPurchases (')':r)
		sb <- displaysPrec customerBalance (' ':sp)
		sn <- displaysPrec customerName (' ':sb)
		return $ "(Customer " ++ sn

instance (NFDataInc l1 inc r m (TxAdaptonM Int)) => NFDataInc l1 inc r m Item where
	rnfInc (Item itemName itemPrice itemInflation itemQuantity) = do
		n <- rnfInc itemName
		p <- rnfInc itemPrice
		i <- rnfInc itemInflation
		q <- rnfInc itemQuantity
		return $ n `seq` p `seq` i `seq` q

instance (NFDataInc l1 inc r m (TxAdaptonM Int),NFDataInc l1 inc r m (ListTxAdaptonM (String, Int))) => NFDataInc l1 inc r m Customer where
	rnfInc (Customer customerName customerBalance customerPurchases) = do
		n <- rnfInc customerName
		b <- rnfInc customerBalance
		p <- rnfInc customerPurchases
		return $ n `seq` b `seq` p

instance Memo Item where
	type Key Item = StableName Item
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ Weak.mkWeak x,stableName x)
instance Memo Customer where
	type Key Customer = StableName Customer
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ Weak.mkWeak x,stableName x)
		

$( derive makeMData ''Item )
$( derive makeMData ''Customer )
$( derive makeDeepTypeable ''Item )
$( derive makeDeepTypeable ''Customer )

-- **
--main = main_Tx
main_Tx = do
	let size = 5
	let runs = 3
	gen <- genListPairs size runs
	--let inc_func = filterInc (return . even)
	let inc_func = leastkIncM 1
	
	hSetBuffering stdout NoBuffering
	
	res2 <- race_ debugger $ testM inc_func gen (runs * 2) >> mergePDFsInto' "filterInc.pdf"
	print res2

testM :: (ListTxM Inside IORef IO Int -> Inside TxAdapton IORef IO (ListTxU Inside IORef IO Int)) -> ([Int],[ListChange Int]) -> Int -> IO ()
testM f (xs,chgs) runs = do
	(s,t) <- runIncremental $ do
		s :: ListTxM Inside IORef IO Int <- toListRefInside xs
		t :: ListTxU Inside IORef IO Int <- inside $ f s
		drawPDF "" proxyTxAdapton proxyIORef proxyIO (Merge s t)
		!() <- rnfInc t
		drawPDF "" proxyTxAdapton proxyIORef proxyIO (Merge s t)
		return (s,t)
	testM' s t chgs
	return ()

testM' :: ListTxM Inside IORef IO Int -> ListTxU Inside IORef IO Int -> [ListChange Int] -> IO ()
testM' s t [] = return ()
testM' s t (chg:chgs) = do
	runIncremental $ do
		applyListChange chg s
		drawPDF "" proxyTxAdapton proxyIORef proxyIO (Merge s t)
		rnfInc t
		drawPDF "" proxyTxAdapton proxyIORef proxyIO (Merge s t)
	testM' s t chgs
	return ()

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

insertListModAt :: (
	Typeable a,Eq a,Eq (ListMod mod l inc r m a),Input mod l inc r m,Layer Outside inc r m) => Int -> a -> ListMod mod l inc r m a -> Outside inc r m ()
insertListModAt i x mxs = setListModAt mxs i $ \xs -> do
	mxs' <- refOutside xs
	return $ ConsMod x mxs'

deleteListModAt :: (Typeable a,Eq a,Eq (ListMod mod l inc r m a),Input mod l inc r m,Layer Outside inc r m) => Int -> ListMod mod l inc r m a -> Outside inc r m ()
deleteListModAt i mxs = setListModAt mxs i $ \xs -> case xs of
	ConsMod x mxs' -> getOutside mxs'
	NilMod -> error "shouldn't be empty"

-- creates an input list from a regular list
toListRefInside :: (Typeable a,
	Eq a,Eq (ListMod mod Inside inc r m a),Input mod Inside inc r m,Layer l inc r m) => [a] -> l inc r m (ListMod mod Inside inc r m a)
toListRefInside xs = inside $ toListRef xs

applyListChange :: (Eq (ListMod mod l inc r m a),Input mod l inc r m,Layer l inc r m,Layer Outside inc r m)
	=> ListChange a -> ListMod mod l inc r m a -> Outside inc r m ()
applyListChange (ListChg f) xs = f xs
