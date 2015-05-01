{-# LANGUAGE DataKinds, StandaloneDeriving, ConstraintKinds, RankNTypes, GADTs, BangPatterns, TemplateHaskell, TupleSections, TypeFamilies, UndecidableInstances, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, ScopedTypeVariables, FlexibleContexts #-}

module Examples.Customer where

import Control.Monad.Incremental
import Control.Monad.Incremental.List
import Control.Concurrent.Transactional
import Control.Concurrent.Transactional.STM
import Control.Concurrent.Transactional.TxAdapton
import Control.Monad.Incremental.Display
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Incremental.Adapton
import Control.Monad.Incremental.LazyNonInc
import Control.Monad.Incremental.Draw
import Control.Concurrent.Transactional.Benchmark

import Debug

import Data.Derive.Memo

import Prelude hiding (read)
import Data.Proxy
import Data.IORef
import Data.Typeable
import Data.DeepTypeable
import Language.Haskell.TH.Syntax

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
import System.Mem.Weak.Exts as Weak
import System.IO.Unsafe
import System.Mem.StableName.Exts

import Control.Concurrent.Transactional.SHFSTM as SHFSTM


type Warehouse mod (l :: * -> * -> *) inc = ListMod mod l inc (Item mod l inc)
data Item mod (l :: * -> * -> *) inc = Item { itemName :: String, itemPrice :: mod l inc Int, itemInflation :: Int, itemQuantity :: mod l inc Int }
	deriving Typeable

deriving instance (Eq (mod l inc Int)) => Eq (Item mod l inc)
deriving instance (Show (mod l inc Int)) => Show (Item mod l inc)

data Customer mod (l :: * -> * -> *) inc = Customer { customerName :: String, customerBalance :: mod l inc Int, customerPurchases :: ListMod mod l inc (String,Int) }
	deriving Typeable

deriving instance (Eq (mod l inc Int),Eq (ListMod mod l inc (String, Int))) => Eq (Customer mod l inc)
deriving instance (Show (mod l inc Int),Show (ListMod mod l inc (String, Int))) => Show (Customer mod l inc)

-- compare items by their price
{-# INLINE compareItems #-}
compareItems :: (IncK inc Int,Thunk mod l inc,Layer l inc) => Item mod l inc -> Item mod l inc -> l inc Ordering
compareItems = \i1 i2 -> do
		p1 <- read $! itemPrice i1 
		p2 <- read $! itemPrice i2 
		return $! compare p1 p2
	
$(deriveName "LeastItem")
leastItem :: (Memo (Item mod l inc),Typeable inc,Layer l inc,Memo (ListMod mod l inc (Item mod l inc)),Memo (ListMod thunk l inc (Item mod l inc)),IncK inc Int,Output thunk l inc,Thunk mod l inc,IncK inc (ListMod' thunk l inc (Item mod l inc)),IncK inc (ListMod' mod l inc (Item mod l inc)))
	=> ListMod mod l inc (Item mod l inc) -> l inc (ListMod thunk l inc (Item mod l inc))
leastItem = leastkIncAs' LeastItem compareItems 1

-- finds the cheapest item
cheapestItem :: (IncK inc (ListMod' thunk l inc (Item mod l inc)),Output thunk l inc,Display Outside inc (Item mod l inc),Transactional inc,IncK inc Int,Input mod l inc)
	=> String -> Warehouse mod l inc -> ListMod thunk l inc (Item mod l inc) -> Outside inc (Item mod l inc)
cheapestItem msg warehouse leastItem = do
	e <- cheapestItem' msg warehouse leastItem
	case e of
		Left v -> return v
		Right e -> throw e

cheapestItem' :: (IncK inc (ListMod' thunk l inc (Item mod l inc)),Output thunk l inc,Display Outside inc (Item mod l inc),IncK inc Int,Input mod l inc)
	=> String -> Warehouse mod l inc -> ListMod thunk l inc (Item mod l inc) -> Outside inc (Either (Item mod l inc) NoBalance)
cheapestItem' msg warehouse leastItem = do
	let wrongQuantity item = do
		str <- display item
		return $ Right $ NoBalance $ "WRONG ITEM " ++ str ++ " " ++ msg
--	display warehouse >>= debugTx . ("warehouse: "++)
--	display leastItem >>= debugTx . ("cheapestItem: "++)
	xs <- forceOutside leastItem
	case xs of
		NilMod -> return $ Right $ NoBalance $ "NO ITEMS LEFT " ++ msg
		ConsMod item _ -> do
			quantity <- getOutside $ itemQuantity item
			if (quantity <= 0)
				then wrongQuantity item
				else return $ Left item

deleteItem :: (IncK inc (ListMod' mod l inc (Item mod l inc)),Input mod l inc,Display Outside inc (Item mod l inc),Transactional inc)
	=> Warehouse mod l inc -> Item mod l inc -> Outside inc ()
deleteItem warehouse item = {-# SCC deleteItem #-} do
	e <- deleteItem' warehouse item
	case e of
		Nothing -> return ()
		Just e -> throw e

deleteItem' :: (IncK inc (ListMod' mod l inc (Item mod l inc)),Input mod l inc,Display Outside inc (Item mod l inc))
	=> Warehouse mod l inc -> Item mod l inc -> Outside inc (Maybe ItemNotFound)
deleteItem' warehouse item = {-# SCC deleteItem' #-} do
	getOutside warehouse >>= \xs -> case xs of
		ConsMod item' warehouse' -> if itemName item == itemName item'
			then getOutside warehouse' >>= set warehouse >> return Nothing
			else deleteItem' warehouse' item
		NilMod -> do
			str <- display item
			return $ Just $ ItemNotFound str

-- buy an item for a customer and increase the item's price
buyCheapestItem :: (Output thunk l inc,IncK inc Int,IncK inc (ListMod' thunk l inc (Item mod l inc)),Input mod l inc,Display Outside inc (Customer mod l inc),Display Outside inc (Item mod l inc),Transactional inc,IncK inc (ListMod' mod l inc (String, Int)),IncK inc (ListMod' mod l inc (Item mod l inc)))
	=> String -> Warehouse mod l inc -> ListMod thunk l inc (Item mod l inc) -> Customer mod l inc -> Outside inc (Item mod l inc)
buyCheapestItem msg warehouse leastItem customer = do
	item <- cheapestItem msg warehouse leastItem
	let wrongQuantity = do
		str1 <- display customer
		str2 <- display item
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
	let noBalance = debugInc ("no balance") $ do
		str1 <- display customer
		str2 <- display item
		throw $ NoBalance $ str1 ++ " has insufficient funds for " ++ show str2 ++ " " ++ msg
	buy `orElse` noBalance

-- buy an item for a customer and increase the item's price
buyCheapestItem' :: (Output thunk l inc,IncK inc Int,IncK inc (ListMod' thunk l inc (Item mod l inc)),Input mod l inc,Display Outside inc (Customer mod l inc),Display Outside inc (Item mod l inc),IncK inc (ListMod' mod l inc (String, Int)),IncK inc (ListMod' mod l inc (Item mod l inc)))
	=> String -> Warehouse mod l inc -> ListMod thunk l inc (Item mod l inc) -> Customer mod l inc -> Outside inc (Either (Item mod l inc) SomeException)
buyCheapestItem' msg warehouse leastItem customer = do
	e <- cheapestItem' msg warehouse leastItem
	case e of
		Left item -> do
			let wrongQuantity = do
				str1 <- display customer
				str2 <- display item
				return $ Right $ toException $ NoBalance $ str1 ++ " CAN'T BUY " ++ str2 ++ " " ++ msg
			
			let noBalance = debugInc ("no balance") $ do
				str1 <- display customer
				str2 <- display item
				return $ Right $ toException $ NoBalance $ str1 ++ " has insufficient funds for " ++ show str2 ++ " " ++ msg
			
			balance <- getOutside (customerBalance customer)
			price <- getOutside (itemPrice item)
			-- check balance; if the check fails, it calls retry
			if (price <= balance)
				then do
					if (price <= balance)
						then do
							-- inflate item's price
							set (itemPrice item) $ price + itemInflation item
							-- decrease item's quantity
							quantity <- getOutside (itemQuantity item)
							e <- if (quantity == 1) then deleteItem' warehouse item else return Nothing
							case e of
								Nothing -> do
									if (quantity <= 0)
										then wrongQuantity
										else do
											set (itemQuantity item) $ pred quantity
											-- decrease customer's balance
											set (customerBalance customer) $ balance - price
											-- add customer's purchase
											modify (customerPurchases customer) $ ref >=> return . ConsMod (itemName item,price)
											return $ Left item
								Just e -> return $ Right $ toException e
						else return $ Right $ toException $ NoBalance "too expensive"
				else noBalance
		Right e -> return $ Right $ toException e

data ItemNotFound = ItemNotFound String deriving (Show,Typeable)
instance Exception ItemNotFound
data NoBalance = NoBalance String deriving (Show,Typeable)
instance Exception NoBalance


instance (Thunk mod l inc,Layers l l1,Display l1 inc (mod l inc Int)) => Display l1 inc (Item mod l inc) where
	displaysPrec proxyL proxyInc (Item itemName itemPrice itemInflation itemQuantity) r = do
		sq <- displaysPrec proxyL proxyInc itemQuantity (')':r)
		si <- displaysPrec proxyL proxyInc itemInflation (' ':sq)
		sp <- displaysPrec proxyL proxyInc itemPrice (' ':si)
		sn <- displaysPrec proxyL proxyInc itemName (' ':sp)
		return $ "(Item " ++ sn

instance (Thunk mod l inc,Layers l l1,Display l1 inc (mod l inc Int),Display l1 inc (ListMod mod l inc (String,Int))) => Display l1 inc (Customer mod l inc) where
	displaysPrec proxyL proxyInc (Customer customerName customerBalance customerPurchases) r = do
		sp <- displaysPrec proxyL proxyInc customerPurchases (')':r)
		sb <- displaysPrec proxyL proxyInc customerBalance (' ':sp)
		sn <- displaysPrec proxyL proxyInc customerName (' ':sb)
		return $ "(Customer " ++ sn

instance (Thunk mod Inside inc,Layers l l1,NFDataInc l1 inc (mod l inc Int)) => NFDataInc l1 inc (Item mod l inc) where
	nfDataInc proxyL proxyInc (Item itemName itemPrice itemInflation itemQuantity) = do
		n <- nfDataInc proxyL proxyInc itemName
		p <- nfDataInc proxyL proxyInc itemPrice
		i <- nfDataInc proxyL proxyInc itemInflation
		q <- nfDataInc proxyL proxyInc itemQuantity
		return $ n `seq` p `seq` i `seq` q

instance (Thunk mod l inc,Layers l l1,NFDataInc l1 inc (mod l inc Int),NFDataInc l1 inc (ListMod mod l inc (String,Int))) => NFDataInc l1 inc (Customer mod l inc) where
	nfDataInc proxyL proxyInc (Customer customerName customerBalance customerPurchases) = do
		n <- nfDataInc proxyL proxyInc customerName
		b <- nfDataInc proxyL proxyInc customerBalance
		p <- nfDataInc proxyL proxyInc customerPurchases
		return $ n `seq` b `seq` p

instance (Typeable l,Memo (mod l inc Int),Typeable mod,Typeable inc) => Memo (Item mod l inc) where
	type Key (Item mod l inc) = String
	{-# INLINE memoKey #-}
	memoKey = itemName
	{-# INLINE memoWeak #-}
	memoWeak = \t -> memoWeak (itemPrice t)

instance (Typeable l,Memo (mod l inc Int),Typeable mod,Typeable inc) => Memo (Customer mod l inc) where
	type Key (Customer mod l inc) = String
	{-# INLINE memoKey #-}
	memoKey = customerName
	{-# INLINE memoWeak #-}
	memoWeak = \t -> memoWeak (customerBalance t)

$( derive makeMData ''Item )
$( derive makeMData ''Customer )
$( derive makeDeepTypeable ''Item )
$( derive makeDeepTypeable ''Customer )

type CustomersData mod thunk l inc = (Warehouse mod l inc,ListMod thunk l inc (Item mod l inc))

genCustomersData :: (NFDataInc l inc (ListMod mod l inc (Item mod l inc)),Memo (Item mod l inc),IncK inc (ListMod' mod l inc (Item mod l inc)),Input mod l inc,IncK inc Int,IncK inc (ListMod' thunk l inc (Item mod l inc)),Thunk mod l inc,Output thunk l inc,Memo (ListMod thunk l inc (Item mod l inc)),Memo (ListMod mod l inc (Item mod l inc)),Typeable inc)
	=> IncParams inc -> Int -> Int -> IO (CustomersData mod thunk l inc,[Bool])
genCustomersData params numItems runs = runIncrementalWithParams params $ outside $ do
	let itemIds = [1..numItems]	
	let genItem itemId = do
		let itemName = show itemId
		price <- unsafeIOToInc $ generate $ choose (50,1000)
		itemPrice <- ref price
		quantity <- unsafeIOToInc $ generate $ choose (1,3)
		itemQuantity <- ref quantity
		let itemInflation = succ $ floor (toEnum price / toEnum quantity :: Float)
		return $ Item itemName itemPrice itemInflation itemQuantity
	warehouse <- toListRef =<< mapM genItem itemIds
	choices <- replicateM runs $ unsafeIOToInc $ generate $ choose (True,False)
	!() <- rnfInc warehouse
	least <- leastItem warehouse
	return ((warehouse,least),choices)

genCustomersThreads :: (NFDataInc l inc [Customer mod l inc],IncK inc Int,IncK inc (ListMod' mod l inc (String,Int)),Input mod l inc)
	=> IncParams inc -> Int -> CustomersData mod thunk l inc -> IO [Customer mod l inc]
genCustomersThreads params numCustomers warehouse = runIncrementalWithParams params $ outside $ do
	let customerIds = [1..numCustomers]
	let genCustomer customerId = do
		let customerName = show customerId
		customerBalance <- ref =<< unsafeIOToInc (generate $ choose (0,maxBound))
		customerPurchases <- ref NilMod
		return $ Customer customerName customerBalance customerPurchases
	customers <- mapM genCustomer customerIds
	() <- rnfInc customers
	return customers

customerJob :: (Draw inc (mod l inc (ListMod' mod l inc (Item mod l inc))),Draw inc (thunk l inc (ListMod' thunk l inc (Item mod l inc))),
	Layers l Outside,Display Outside inc (ListMod thunk l inc (Item mod l inc)),Transactional inc,IncK inc Int,IncK inc (ListMod' thunk l inc (Item mod l inc)),IncK inc (ListMod' mod l inc (String, Int)),IncK inc (ListMod' mod l inc (Item mod l inc)),Output thunk l inc,Input mod l inc,Display Outside inc (ListMod mod l inc (String,Int)),Display Outside inc (mod l inc Int))
	=> IncParams inc -> CustomersData mod thunk l inc -> Bool -> Customer mod l inc -> IO ()
customerJob params (warehouse,leastItem) choice customer = do
	let noBalance (NoBalance msg) = do
--		drawDot ("customer exception " ++ customerName customer ++ " " ++ msg) Proxy (Merge warehouse leastItem)
		throw (NoBalance msg)		
	let action = if choice
		then do -- list the cheapest item
			(time,item) <- timeItT $ atomicallyWithParams params $ outside $ do
--				drawDot ("customer listing" ++ customerName customer) Proxy (Merge warehouse leastItem)
				item <- cheapestItem ("customer " ++ customerName customer) warehouse leastItem
				str <- display item
--				let msg = "customer " ++ customerName customer ++ " found cheapest " ++ str
--				drawDot msg Proxy (Merge warehouse leastItem)
				return str
			writeChan debugChan $ "customer " ++ customerName customer ++ " found cheapest " ++ item ++ " in " ++ show time
		else do -- buy the cheapest item
			(time,item) <- timeItT $ atomicallyWithParams params $ flip catch noBalance $ outside $ do
--				drawDot ("customer buying" ++ customerName customer) Proxy (Merge warehouse leastItem)
				item <- buyCheapestItem ("customer " ++ customerName customer) warehouse leastItem customer
				str <- display item
--				let msg = "customer " ++ customerName customer ++ " bought cheapest " ++ str
--				drawDot msg Proxy (Merge warehouse leastItem)
				return str
			writeChan debugChan $ "customer " ++ customerName customer ++ " bought cheapest " ++ item ++ " in " ++ show time
	let noBalance2 e@(NoBalance msg) = error $ "exception: " ++ show msg
	let anyException (e::SomeException) = error $ "exception: " ++ show e
	action `Catch.catches` [Catch.Handler noBalance2,Catch.Handler anyException]

customerJob' :: (Draw inc (thunk l inc (ListMod' thunk l inc (Item mod l inc))),Draw inc (mod l inc (ListMod' mod l inc (Item mod l inc))),
	Layers l Outside,Display Outside inc (ListMod thunk l inc (Item mod l inc)),IncK inc Int,IncK inc (ListMod' thunk l inc (Item mod l inc)),IncK inc (ListMod' mod l inc (String, Int)),IncK inc (ListMod' mod l inc (Item mod l inc)),Output thunk l inc,Input mod l inc,Display Outside inc (ListMod mod l inc (String,Int)),Display Outside inc (mod l inc Int))
	=> IncParams inc -> CustomersData mod thunk l inc -> Bool -> Customer mod l inc -> IO ()
customerJob' params (warehouse,leastItem) choice customer = do	
	let action = if choice
		then do -- list the cheapest item
			(time,item) <- timeItT $ runIncrementalWithParams params $ do
--				drawDot ("customer listing" ++ customerName customer ) Proxy (Merge warehouse leastItem)
				e <- cheapestItem' ("customer " ++ customerName customer) warehouse leastItem
				case e of
					Left item -> do
						str <- display item
--						let msg = "customer " ++ customerName customer ++ " found cheapest " ++ str
--						drawDot msg Proxy (Merge warehouse leastItem)
						return str
					Right e -> return $ show e
			writeChan debugChan $ "customer " ++ customerName customer ++ " found cheapest " ++ item ++ " in " ++ show time
		else do -- buy the cheapest item
			(time,item) <- timeItT $ runIncrementalWithParams params $ do
--				drawDot ("customer buying" ++ customerName customer ) Proxy (Merge warehouse leastItem)
				e <- buyCheapestItem' ("customer " ++ customerName customer) warehouse leastItem customer
				case e of
					Left item -> do
						str <- display item
--						let msg = "customer " ++ customerName customer ++ " bought cheapest " ++ str
--						drawDot msg Proxy (Merge warehouse leastItem)
						return str
					Right e -> return $ show e
			writeChan debugChan $ "customer " ++ customerName customer ++ " bought cheapest " ++ item ++ " in " ++ show time
	action

customersSTMBench :: TxBenchmark STM (CustomersData STMVar T Outside STM) Bool (Customer STMVar Outside STM)
customersSTMBench = TxBenchmark "CustomerSTM" genCustomersData genCustomersThreads customerJob'

customersSHFSTMBench :: TxBenchmark SHFSTM (CustomersData SHFSTMVar T Outside SHFSTM) Bool (Customer SHFSTMVar Outside SHFSTM)
customersSHFSTMBench = TxBenchmark "CustomerSHFSTM" genCustomersData genCustomersThreads customerJob'

customersLazyNonIncBench :: TxBenchmark LazyNonInc (CustomersData LazyNonIncM LazyNonIncU Outside LazyNonInc) Bool (Customer LazyNonIncM Outside LazyNonInc)
customersLazyNonIncBench = TxBenchmark "CustomerLazyNonInc" genCustomersData genCustomersThreads customerJob'

customersAdaptonBench :: TxBenchmark Adapton (CustomersData M U Inside Adapton) Bool (Customer M Inside Adapton)
customersAdaptonBench = TxBenchmark "CustomerAdapton" genCustomersData genCustomersThreads customerJob'

customersTxAdaptonEBench :: TxBenchmark TxAdaptonE (CustomersData (TxME Versioned) TxUE Inside TxAdaptonE) Bool (Customer (TxME Versioned) Inside TxAdaptonE)
customersTxAdaptonEBench = TxBenchmark "CustomerTxAdaptonE" genCustomersData genCustomersThreads customerJob

customersTxAdaptonEBench' :: TxBenchmark TxAdaptonE (CustomersData (TxME Versioned) TxUE Inside TxAdaptonE) Bool (Customer (TxME Versioned) Inside TxAdaptonE)
customersTxAdaptonEBench' = TxBenchmark "CustomerTxAdaptonE" genCustomersData genCustomersThreads customerJob'

customersTxAdaptonCBench :: TxBenchmark TxAdaptonC (CustomersData (TxMC Versioned) TxUC Inside TxAdaptonC) Bool (Customer (TxMC Versioned) Inside TxAdaptonC)
customersTxAdaptonCBench = TxBenchmark "CustomerTxAdaptonC" genCustomersData genCustomersThreads customerJob

customersTxAdaptonCBench' :: TxBenchmark TxAdaptonC (CustomersData (TxMC Versioned) TxUC Inside TxAdaptonC) Bool (Customer (TxMC Versioned) Inside TxAdaptonC)
customersTxAdaptonCBench' = TxBenchmark "CustomerTxAdaptonC" genCustomersData genCustomersThreads customerJob'

customersNonIncEBench :: TxBenchmark TxAdaptonE (CustomersData (TxME Versioned) T Outside TxAdaptonE) Bool (Customer (TxME Versioned) Outside TxAdaptonE)
customersNonIncEBench = TxBenchmark "CustomerNonIncE" genCustomersData genCustomersThreads customerJob'

customersNonIncCBench :: TxBenchmark TxAdaptonC (CustomersData (TxMC Versioned) T Outside TxAdaptonC) Bool (Customer (TxMC Versioned) Outside TxAdaptonC)
customersNonIncCBench = TxBenchmark "CustomerNonIncC" genCustomersData genCustomersThreads customerJob'

main :: IO ()
main = {-flip Exception.finally (mergeGraphsInto' "tx.pdf") $-} do
	-- synchronous debugging I/O
	hSetBuffering stdout NoBuffering
	
	concurrently debugger $ do
		
		-- compare to other STMs
--		runTxBenchmark customersSTMBench defaultIncParams (10^4) 10 5
--		runTxBenchmark customersSHFSTMBench defaultIncParams (10^4) 10 5
--		runTxBenchmark customersNonIncEBench ((defaultIncParamsProxy proxyTxAdaptonE) { txAdaptonMemoSize = 10^4 }) (10^4) 10 5
		
		-- non-incremental, non-threaded
--		runTxBenchmark customersLazyNonIncBench (defaultIncParamsProxy proxyLazyNonInc) (10^5) 50 1
		-- incremental, non-threaded
--		runTxBenchmark customersAdaptonBench ((defaultIncParamsProxy proxyAdapton) { adaptonMemoSize = 10^5 }) (10^5) 20 1

		-- non-incremental, threaded
--		runTxBenchmark customersNonIncEBench ((defaultIncParamsProxy proxyTxAdaptonE) { txAdaptonMemoSize = 10^4 }) (10^4) 40 1
--		runTxBenchmark customersNonIncCBench ((defaultIncParamsProxy proxyTxAdaptonC) { txAdaptonMemoSize = 10^4 }) (10^4) 40 1
    	
		-- incremental, 1 thread
		runTxBenchmark customersTxAdaptonEBench ((defaultIncParamsProxy proxyTxAdaptonE) { txAdaptonMemoSize = 10^4 }) (10^4) 10 5
--		runTxBenchmark customersTxAdaptonCBench ((defaultIncParamsProxy proxyTxAdaptonC) { txAdaptonMemoSize = 10^4 }) (10^4) 10 5
	
	return ()



