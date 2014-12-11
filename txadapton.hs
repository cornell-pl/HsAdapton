{-# LANGUAGE TupleSections, TypeFamilies, UndecidableInstances, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables, FlexibleContexts #-}

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

topkInc :: (Eq (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Hashable (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Eq (ListMod mod l inc r m a),Eq (ListMod thunk l inc r m a),MonadIO m,Memo a,Memo (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Memo (ListMod mod l inc r m a),Thunk mod l inc r m,Memo (ListMod thunk l inc r m a),Output thunk l inc r m,Ord a) =>
	(a -> a -> l inc r m Ordering) -> Int -> ListMod mod l inc r m a -> l inc r m (ListMod thunk l inc r m a)
topkInc cmp i = memo $ \_ mxs -> (mapInc return >=> quicksortInc (flip cmp) >=> takeInc' i) mxs >>= force

leastkInc :: (Eq (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Hashable (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Eq (ListMod mod l inc r m a),Eq (ListMod thunk l inc r m a),MonadIO m,Memo a,Memo (ListMod thunk l inc r m (ListMod thunk l inc r m a)),Memo (ListMod mod l inc r m a),Thunk mod l inc r m,Memo (ListMod thunk l inc r m a),Output thunk l inc r m,Eq a) =>
	(a -> a -> l inc r m Ordering) -> Int -> ListMod mod l inc r m a -> l inc r m (ListMod thunk l inc r m a)
leastkInc cmp i = memo $ \_ mxs -> (mapInc return >=> quicksortInc cmp >=> takeInc' i) mxs >>= force

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

main_Adapton = {-flip Exception.finally (mergePDFsInto' "tx.pdf") $ -} runIncremental $ do
	let (size::Int) = 10^3
	xs <- inL $ liftIO $ genList size
	db <- inside $ toListRef xs
	
--	let topk_func = topkInc 3 -- we need to use the same function to enable memoization
	
--	drawPDF proxyAdapton proxyIORef proxyIO db
	
	loop_Adapton size db

loop_Adapton :: Int -> ListMod M Inside Adapton IORef IO Int -> Outside Adapton IORef IO ()
loop_Adapton size db = replicateM_ 10 $ do
	(time,()) <- timeOuterT $ do
		p <- inL $ liftIO $ generate $ choose (0,size-1)
		i <- inL $ liftIO $ generate $ choose (0,maxBound)
		setListModAt db p $ \(ConsMod x mxs) -> return $ ConsMod i mxs
		
--		drawPDF proxyAdapton proxyIORef proxyIO db
		
		inL $ liftIO $ putStrLn $ "added to db " ++ show i
		(topk :: ListMod U Inside Adapton IORef IO Int) <- topk3 db
		--inside $ displayAs "top3: " topk
		inside $ rnfInc topk
		
--		drawPDF proxyAdapton proxyIORef proxyIO (Merge db topk)
		
	inL $ liftIO $ putStrLn ("CPU "++show time)

timeOuterT :: Outer r IO a -> Outer r IO (Double,a)
timeOuterT (Outer m) = Outer $ timeItT m

topk3 = inside . topkInc (\x y -> return $ compare x y) 3

main = main_DB
	--main_TxAdapton

type ListTxAdaptonU a = ListMod TxU Inside TxAdapton IORef IO a
type TxAdaptonU a = TxU Inside TxAdapton IORef IO a
type ListTxAdaptonM a = ListMod TxM Inside TxAdapton IORef IO a
type TxAdaptonM a = TxM Inside TxAdapton IORef IO a

type Warehouse = ListTxAdaptonM Item
data Item = Item { itemName :: String, itemPrice :: TxAdaptonM Int, itemInflation :: Int, itemQuantity :: TxAdaptonM Int }
	deriving (Typeable,Eq)

data Customer = Customer { customerName :: String, customerBalance :: TxAdaptonM Int, customerPurchases :: ListTxAdaptonM (String,Int) }
	deriving (Typeable,Eq)

leastItem = inside . leastkInc compareItems 1 where
	compareItems i1 i2 = do
		p1 <- get $ itemPrice i1 
		p2 <- get $ itemPrice i2 
		return $ compare p1 p2
-- finds the cheapest item
cheapestItem :: String -> Warehouse -> STxAdaptonM Item
cheapestItem msg warehouse = do
	let wrongQuantity item = do
		time <- readTxTime
		str <- showInc item
		throw $ NoBalance $ "WRONG ITEM " ++ str ++ " " ++ show time ++ " " ++ msg
	showInc warehouse >>= debugTx . ("warehouse: "++)
	(mxs::ListTxAdaptonU Item) <- leastItem warehouse
	showInc mxs >>= debugTx . ("cheapestItem: "++)
	xs <- forceOutside mxs
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
buyCheapestItem :: String -> Warehouse -> Customer -> STxAdaptonM Item
buyCheapestItem msg warehouse customer = do
	item <- cheapestItem msg warehouse
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
	let noBalance = do
		str1 <- inside $ showInc customer
		str2 <- inside $ showInc item
		throw $ NoBalance $ str1 ++ " has insufficient funds for " ++ show str2 ++ " " ++ msg
	buy `orElse` noBalance

data NoBalance = NoBalance String deriving (Show,Typeable)
instance Exception NoBalance

customer_thread :: Warehouse -> Customer -> IO ()
customer_thread warehouse customer = do
	let action = do
		choice <- generate $ choose (False,True)
		if choice
			then do -- list the cheapest item
				(time,(tx,item)) <- timeItT $ atomicallyTx ("customer " ++ customerName customer) $ readTxTime >>= \tx -> cheapestItem ("customer " ++ customerName customer) warehouse >>= liftM (tx,) . showInc
				writeChan debugChan $ "customer " ++ customerName customer ++ " found cheapest " ++ item ++ " in " ++ show time ++ " " ++ show tx
			else do -- buy the cheapest item
				(time,(tx,item)) <- timeItT $ atomicallyTx ("customer "++ customerName customer) $ readTxTime >>= \tx -> buyCheapestItem ("customer " ++ customerName customer) warehouse customer >>= liftM (tx,) . showInc
				writeChan debugChan $ "customer " ++ customerName customer ++ " bought cheapest " ++ item ++ " in " ++ show time ++ " " ++ show tx
		threadDelay 300
		action
	let noBalance (NoBalance msg) = writeChan debugChan msg
	action -- `Catch.catch` noBalance

main_DB = do
	let numItems = 5
	let numCustomers = 3
	
	hSetBuffering stdout NoBuffering
	
	(warehouse,customers) <- atomically $ genDB numItems numCustomers
	
	race_ debugger $ mapConcurrently (customer_thread warehouse) customers
	
genDB :: Int -> Int -> STxAdaptonM (Warehouse,[Customer])
genDB numItems numCustomers = do
	let itemIds = [1..numItems]	
	let genItem itemId = do
		let itemName = show itemId
		price <- inL $ liftIO $ generate $ choose (0,500)
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
	return (warehouse,customers)
	
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
	memoKey x = (MkWeak $ Weak.mkWeak x,unsafePerformIO $ makeStableName x)
instance Memo Customer where
	type Key Customer = StableName Customer
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ Weak.mkWeak x,unsafePerformIO $ makeStableName x)
		
	