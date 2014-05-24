{-# LANGUAGE FlexibleContexts, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module System.Mem.MemoTable (
	Memo(..),
	memoLazy,
	memoStrict
) where

-- | Implementation of memo tables using hash tables and weak pointers as presented in http://community.haskell.org/~simonmar/papers/weak.pdf.
-- | Requires the package hashtables.

import System.Mem.Weak
import System.Mem.StableName
import System.IO.Unsafe
import Data.HashTable.IO as HashIO
import Data.HashTable.ST.Basic
import Control.Monad
import Data.Hashable

class (Eq (MemoKey a),Hashable (MemoKey a)) => Memo a where
	type family MemoKey a :: *
	memoKey :: a -> IO (MemoKey a) -- we need the IO to generate stable names

instance Memo Int where
	type MemoKey Int = Int
	memoKey = return
	
instance Memo Bool where
	type MemoKey Bool = Bool
	memoKey = return

instance Memo Char where
	type MemoKey Char = Char
	memoKey = return

instance Memo String where
	type MemoKey String = String
	memoKey = return

instance Memo Float where
	type MemoKey Float = Float
	memoKey = return

instance Memo Double where
	type MemoKey Double = Double
	memoKey = return

-- for any other data type besides primitive types, use this instance
instance Memo (Either a b) where
	type MemoKey (Either a b) = StableName (Either a b)
	memoKey = makeStableName
	
type SNMap k v = IOHashTable HashTable (MemoKey k) v

newSNMap :: Memo k => k -> IO (SNMap k v)
newSNMap _ = HashIO.new

lookupSNMap :: Memo k => k -> SNMap k v -> MemoKey k -> IO (Maybe v)
lookupSNMap _ = HashIO.lookup

insertSNMap :: Memo k => k -> SNMap k v -> MemoKey k -> v -> IO ()
insertSNMap _ = HashIO.insert

removeSNMap :: Memo k => k -> SNMap k v -> MemoKey k -> IO ()
removeSNMap _ = HashIO.delete

snMapElems :: Memo k => k -> SNMap k v -> IO [(MemoKey k,v)]
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
memo isStrict f a =
	let (tbl,weak) = unsafePerformIO $ do
		tbl <- newSNMap a
		weak <- mkWeak tbl tbl $ Just $ table_finalizer a tbl
		return (tbl,weak)
	in memo' isStrict f tbl weak a

table_finalizer :: Memo a => a -> SNMap a (Weak b) -> IO ()
table_finalizer k tbl = do
	pairs <- snMapElems k tbl
	sequence_ [ finalize w | (_,w) <- pairs ]

memo' :: Memo a => Bool -> (a -> b) -> MemoTable a b -> Weak (MemoTable a b) -> a -> b
memo' isStrict f tbl weak_tbl arg = unsafePerformIO $ do
	let val = f arg
	sn <- if isStrict then memoKey $! arg else memoKey arg
	let not_found = do
		weak <- mkWeak arg val $ Just $ finalizer arg tbl sn weak_tbl
		insertSNMap arg tbl sn weak
		return val
	lkp <- lookupSNMap arg tbl sn
	case lkp of
		Nothing -> not_found
		Just w -> do
			maybe_val <- deRefWeak w
			case maybe_val of
				Nothing -> not_found
				Just val -> return val   

finalizer :: Memo a => a -> MemoTable a b -> MemoKey a -> Weak (MemoTable a b) -> IO ()
finalizer k tbl sn weak_tbl = do
	r <- deRefWeak weak_tbl
	case r of
		Nothing -> return ()
		Just mvar -> removeSNMap k tbl sn



