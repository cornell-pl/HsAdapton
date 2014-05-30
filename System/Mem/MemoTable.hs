{-# LANGUAGE MagicHash, ScopedTypeVariables, GADTs, FlexibleContexts, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module System.Mem.MemoTable where

-- | Implementation of memo tables using hash tables and weak pointers as presented in http://community.haskell.org/~simonmar/papers/weak.pdf.
-- | Requires the package hashtables.

import System.Mem.Weak
import System.Mem.StableName
import System.IO.Unsafe
import Data.HashTable.IO as HashIO
import Data.HashTable.ST.Basic
import Control.Monad
import Data.Hashable
import Data.Typeable
import GHC.Base
import Control.Monad.Trans

import Debug.Trace

data MemoKey where
	MemoKey :: (Show k,Ord k,Hashable k,Typeable k) => k -> MemoKey

instance Ord MemoKey where
	compare (MemoKey (a :: a)) (MemoKey (b :: b)) = case cast a :: Maybe b of
		Just b' -> compare b b'
		Nothing -> compare (typeOf a) (typeOf b)

instance Show MemoKey where
	show (MemoKey a) = show a

-- this is kind of an hack, it may cause trouble
instance Ord (StableName a) where
	compare sn1 sn2 = compare (hashStableName sn1) (hashStableName sn2)

instance Show (StableName a) where
	show = show . hashStableName

instance Hashable MemoKey where
	hashWithSalt i (MemoKey a) = hashWithSalt i a

instance Eq MemoKey where
	(MemoKey (a :: a)) == (MemoKey (b :: b)) = case cast a :: Maybe b of
		Just b' -> b == b'
		Nothing -> False

class Memo a where
	memoKey :: a -> MemoKey -- we need the IO to generate stable names

instance Memo Int where
	memoKey = MemoKey
instance Memo Bool where
	memoKey = MemoKey
instance Memo Char where
	memoKey = MemoKey
instance Memo String where
	memoKey = MemoKey
instance Memo Float where
	memoKey = MemoKey
instance Memo Double where
	memoKey = MemoKey

-- for any other data type besides primitive types, use this instance
instance (Eq a,Eq b,Typeable a,Typeable b) => Memo (Either a b) where
		memoKey = MemoKey . unsafePerformIO . makeStableName
instance (Eq a,Eq b,Typeable a,Typeable b) => Memo (a,b) where
		memoKey = MemoKey . unsafePerformIO . makeStableName
	
type SNMap k v = IOHashTable HashTable (MemoKey) v

newSNMap :: Memo k => k -> IO (SNMap k v)
newSNMap _ = HashIO.new

lookupSNMap :: Memo k => k -> SNMap k v -> MemoKey -> IO (Maybe v)
lookupSNMap _ = HashIO.lookup

insertSNMap :: Memo k => k -> SNMap k v -> MemoKey -> v -> IO ()
insertSNMap _ = HashIO.insert

removeSNMap :: Memo k => k -> SNMap k v -> MemoKey -> IO ()
removeSNMap _ = HashIO.delete

snMapElems :: Memo k => k -> SNMap k v -> IO [(MemoKey,v)]
snMapElems _ = HashIO.toList

type MemoTable a b = SNMap a (Weak b)

-- | Memoizes a function with pointer equality, that does not evaluate the arguments and preserves laziness
memoLazy :: Memo a => (a -> b) -> a -> b
memoLazy = memo False

-- | Memoizes a function with strict equality, that loses laziness but increases sharing of memoized results
memoStrict :: Memo a => (a -> b) -> a -> b
memoStrict = memo True

-- | @memo@ takes a function with arbitrary range and domain, and returns a memoized version of the function
memo :: Memo a => Bool -> (a -> b) -> a -> b
memo isStrict (f :: a -> b) =
	let (tbl,weak) = unsafePerformIO $ do
		tbl <- newSNMap (undefined :: a)
		weak <- mkWeak tbl tbl $ Just $ table_finalizer (undefined :: a) tbl
		return (tbl,weak)
	in memo' isStrict f tbl weak

table_finalizer :: Memo a => a -> SNMap a (Weak b) -> IO ()
table_finalizer k tbl = do
	pairs <- snMapElems k tbl
	sequence_ [ finalize w | (_,w) <- pairs ]

memo' :: Memo a => Bool -> (a -> b) -> MemoTable a b -> Weak (MemoTable a b) -> a -> b
memo' isStrict f tbl weak_tbl arg = unsafePerformIO $ do
	let val = f arg
	let sn = if isStrict then memoKey $! arg else memoKey arg
	let not_found = do
		weak <- mkWeak arg val $ Just $ finalizer arg tbl sn weak_tbl
		trace ("memo add " ++ show sn) $ insertSNMap arg tbl sn weak
		keys <- liftM (map fst) $ HashIO.toList tbl
		return val
	keys <- liftM (map fst) $ HashIO.toList tbl
	lkp <- trace ("memo search " ++ show sn ++ " in " ++ show keys) $ lookupSNMap arg tbl sn
	case lkp of
		Nothing -> not_found
		Just w -> do
			maybe_val <- deRefWeak w
			case maybe_val of
				Nothing -> not_found
				Just val -> trace "memo hit" $ return val   

finalizer :: Memo a => a -> MemoTable a b -> MemoKey -> Weak (MemoTable a b) -> IO ()
finalizer k tbl sn weak_tbl = do
	r <- deRefWeak weak_tbl
	case r of
		Nothing -> return ()
		Just mvar -> removeSNMap k tbl sn


memoLazyM :: (Monad m,Memo a) => (a -> m b) -> a -> m b
memoLazyM = memoM False

memoStrictM :: (Monad m,Memo a) => (a -> m b) -> a -> m b
memoStrictM = memoM True

-- | memoizes a monad function. the difference to @memo@ is that the result @b@ is memoized, rather than the monadic computation @m b@
memoM :: (Monad m,Memo a) => Bool -> (a -> m b) -> a -> m b
memoM isStrict (f :: a -> m b) =
	let (tbl,weak) = unsafePerformIO $ do
		tbl <- trace "memo new"$ newSNMap (undefined :: a)
		weak <- mkWeak tbl tbl $ Just $ table_finalizer (undefined :: a) tbl
		return (tbl,weak)
	in memoM' isStrict f tbl weak

-- it is assuming that the monad itself is lazy
memoM' :: (Monad m,Memo a) => Bool -> (a -> m b) -> MemoTable a b -> Weak (MemoTable a b) -> a -> m b
memoM' isStrict f tbl weak_tbl arg = do
	val <- f arg -- we need to compute the value within the monad
	let res = unsafePerformIO $ do
		let sn = if isStrict then memoKey $! arg else memoKey arg
		let not_found = liftIO $ do
			weak <- mkWeak arg val $ Just $ finalizer arg tbl sn weak_tbl
			trace ("memo add " ++ show sn) $ insertSNMap arg tbl sn weak
			keys <- liftM (map fst) $ HashIO.toList tbl
			return val
		keys <- liftIO $ liftM (map fst) $ HashIO.toList tbl
		lkp <- liftIO $ trace ("memo search " ++ show sn ++ " in " ++ show keys) $ lookupSNMap arg tbl sn
		case lkp of
			Nothing -> not_found
			Just w -> do
				maybe_val <- liftIO $ deRefWeak w
				case maybe_val of
					Nothing -> not_found
					Just val -> trace "memo hit" $ return val
	return res



