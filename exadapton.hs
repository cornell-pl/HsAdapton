{-# LANGUAGE StandaloneDeriving, ScopedTypeVariables, MultiParamTypeClasses, DeriveDataTypeable, KindSignatures, ExistentialQuantification, DataKinds, FlexibleInstances, DeriveGeneric, FlexibleContexts #-}

module Main where

import Generics.Lenses.Symmetric.Language hiding (get,put)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State (State(..),StateT(..),MonadState)
import qualified Control.Monad.State as State
import GHC.Generics
import Control.Monad.Adapton.DataTypes

import Debug.Trace
import System.IO.Unsafe
import Data.Maybe
import Control.Monad.Identity
import Data.List as List
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.IORef

import Control.Monad.Adapton
import System.Mem.MemoTable (Memo(..),MemoKey(..))
import qualified System.Mem.MemoTable as Memo
import System.Mem.StableName
import System.Mem.Weak
import Control.Monad.Trans
import Data.Typeable
import Control.Monad.Ref
import Data.STRef.Lazy as STRef
import Control.Monad.ST.Lazy

import Control.Monad.Adapton.Display
import Control.Monad.ST.Lazy.Unsafe

data ListM' m r a = NilM | ConsM a (ListM m r a) deriving (Eq,Generic,Typeable)
type ListM m r a = M m r (ListM' m r a)

instance (Typeable m,Typeable r,Typeable a) => Memo (ListM' m r a) where
	memoKey = MemoKey . unsafePerformIO . makeStableName
	
instance (Eq a,Typeable a,Display l m r a,Layer l m r) => Display l m r (ListM' m r a) where
	showL = showL . from

data ListU' m r a = NilU | ConsU a (ListU m r a) deriving (Eq,Generic,Typeable)
type ListU m r a = U m r (ListU' m r a)

instance (Typeable m,Typeable r,Typeable a) => Memo (ListU' m r a) where
	memoKey = MemoKey . unsafePerformIO . makeStableName

instance (Eq a,Typeable a,Display l m r a,Layer l m r) => Display l m r (ListU' m r a) where
	showL = showL . from

-- * filter
filterInc :: (Eq a,Typeable a,Layer l m r) => (a -> Bool) -> ListM m r a -> l m r (ListU m r a)
filterInc p = thunk . filterInc' p

filterInc' :: (Eq a,Typeable a,Layer l m r) => (a -> Bool) -> ListM m r a -> l m r (ListU' m r a)
filterInc' p mxs = get mxs >>= \xs -> case xs of
	ConsM x mxs' -> if p x
		then liftM (ConsU x) $ filterInc p mxs'
		else filterInc' p mxs'
	NilM -> return NilU

toListM :: (Typeable a,Layer Outer m r) => [a] -> Outer m r (ListM m r a)
toListM xs = toListM' xs >>= ref
	
toListM' :: (Typeable a,Layer Outer m r) => [a] -> Outer m r (ListM' m r a)
toListM' [] = return NilM
toListM' (x:xs) = do
	mxs <- toListM xs
	return $ ConsM x mxs

tailOfListM :: (Eq a,Typeable a,Layer l m r) => ListM m r a -> l m r (ListM m r a)
tailOfListM mxs = get mxs >>= \xs -> case xs of
	NilM -> return mxs
	ConsM x mxs' -> tailOfListM mxs'

testFilter :: IO ()
testFilter = stToIO $ run $ do
	(mxs :: ListM (ST RealWorld) (STRef RealWorld) Int) <- toListM [1,2,3,4]
	liftIO (putStrLn "input: ") >> display mxs
	tys <- inner $ filterInc odd mxs
	inner $ liftIO (putStrLn "output: ") >> display tys
	
--	inL $ printDependentsM "" mxs
--	inL $ printDependenciesU "" tys
	
	liftIO $ putStrLn "changing"
	mtailx <- tailOfListM mxs
	tailx <- toListM' [5,6]
	set mtailx tailx
--	inL $ printDependentsM "" mxs
--	inL $ printDependenciesU "" tys
	liftIO (putStrLn "input: ") >> display mxs
	inner $ liftIO (putStrLn "output: ") >> display tys

-- * length

lengthInc :: (Eq a,Typeable a,Layer l m r) => ListM m r a -> l m r (U m r Int)
lengthInc mxs = thunk $ get mxs >>= \xs -> case xs of
	ConsM x mxs' -> lengthInc mxs' >>= force >>= return . succ
	NilM -> return 0

lengthMemo :: (Eq a,Typeable a,Layer Inner m r) => ListM m r a -> Inner m r (U m r Int)
lengthMemo = memo lengthMemo'
lengthMemo' :: (Eq a,Typeable a,Layer Inner m r) => (ListM m r a -> Inner m r (U m r Int)) -> ListM m r a -> Inner m r Int
lengthMemo' rec mxs = get mxs >>= \xs -> case xs of
	ConsM x mxs' -> rec mxs' >>= force >>= return . succ
	NilM -> return 0

-- it is safe to memoize even thunks that correspond to changed refs, because they have been always dirtied
testLength :: IO ()
testLength = stToIO $ run $ do
	(mxs :: ListM (ST RealWorld) (STRef RealWorld) Int) <- toListM ([1,2,3,4]::[Int])
	liftIO (putStrLn "input: ") >> display mxs
	tys <- inner $ lengthMemo mxs
	inner $ liftIO (putStrLn "output: ") >> display tys

	ConsM x1 mxs234 <- get mxs
	xs234 <- get mxs234
	
--	let ConsM x2 mxs34 = xs234
--	xs34 <- get mxs34
--	let ConsM x3 mxs4 = xs34
--	xs56 <- toListM' [5,6]
--	set mxs4 xs56
	
	set mxs xs234
	
	liftIO (putStrLn "input: ") >> display mxs
	inner $ liftIO (putStrLn "output: ") >> display tys

-- * fibonnaci

fib :: Layer Inner m r => Int -> Inner m r (U m r Int)
fib = memo fib'

fib' :: Layer Inner m r => (Int -> Inner m r (U m r Int)) -> Int -> Inner m r Int
fib' memo_fib n = if n <= 1
	then return 1
	else do
		n1 <- memo_fib (n - 1) >>= force
		n2 <- memo_fib (n - 2) >>= force
		return (n1 + n2)

testMemo = do
	let fmemo = Memo.memo False ex
	print $ fmemo 0
	print $ fmemo 0
  where
	ex :: Int -> Int
	ex 0 = 100
	ex 1 = 200
	ex 2 = 400

test = do
	let x = 1
	let p = (x,x)
	sn1 <- makeStableName $ fst p
	sn2 <- makeStableName $ snd p
	print $ eqStableName sn1 sn2

--printDependenciesM :: (MonadIO m,Ref m r) => String -> M m r a -> m ()
--printDependenciesM pad m = liftIO $ putStrLn $ pad ++ "<--M" ++ show (idM m)
--printDependenciesU :: Layer l m r => String -> U l m r a -> m ()
--printDependenciesU pad t = do
--	liftIO $ putStrLn $ pad ++ "<--U" ++ show (idU t)
--	deps <- readRef (dependenciesU t)
--	mapM_ (\(i,some) -> case some of { Left (SomeM m') -> printDependenciesM (pad++" ") m' ; Right (SomeU t') -> printDependenciesU (pad++" ") t' }) deps
--
--printDependentsM :: (MonadIO m,Ref m r) => String -> M m r a -> m ()
--printDependentsM pad m = do
--	liftIO $ putStrLn $ pad ++ "-->M" ++ show (idM m)
--	deps <- readRef (dependentsM m)
--	intMapM_ (\some -> case some of { Left (SomeM m') -> printDependentsM (pad++" ") m' ; Right (SomeU t') -> printDependentsU (pad++" ") t' }) deps
--printDependentsU :: (MonadIO m,Layer l m r) => String -> U l m r a -> m ()
--printDependentsU pad t = do
--	liftIO $ putStrLn $ pad ++ "-->U" ++ show (idU t)
--	deps <- readRef (dependentsU t)
--	intMapM_ (\some -> case some of { Left (SomeM m') -> printDependentsM (pad++" ") m' ; Right (SomeU t') -> printDependentsU (pad++" ") t' }) deps


deriving instance Typeable ST

instance MonadIO (ST RealWorld) where
	liftIO = unsafeIOToST



