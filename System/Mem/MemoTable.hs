module System.Mem.MemoTable (
	Hash(..),
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
import Data.Hashable

class Hash a where
	hashVal :: a -> Int
	hashList :: [a] -> Int -- same trick as showList
	hashList = hashStableName . unsafePerformIO . makeStableName

instance Hash Int where
	hashVal = hash

instance Hash Char where
	hashVal = hash
	hashList = hash
	
instance Hash Bool where
	hashVal = hash
	
instance Hash Float where
	hashVal = hash

instance Hash Double where
	hashVal = hash
	
-- for any other data type besides primitive types, use this instance
instance Hash [a] where
	hashVal = hashStableName . unsafePerformIO . makeStableName

type SNMap v = IOHashTable HashTable Int v

newSNMap :: IO (SNMap v)
newSNMap = HashIO.new

lookupSNMap :: SNMap v -> Int -> IO (Maybe v)
lookupSNMap = HashIO.lookup

insertSNMap :: SNMap v -> Int -> v -> IO ()
insertSNMap = HashIO.insert

removeSNMap :: SNMap v -> Int -> IO ()
removeSNMap = HashIO.delete

snMapElems :: SNMap v -> IO [(Int,v)]
snMapElems = HashIO.toList

type MemoTable b = SNMap (Weak b)

-- | Memoizes a function with pointer equality, that does not evaluate the arguments and preserves laziness
memoLazy :: Hash a => (a -> b) -> a -> b
memoLazy = memo False

-- | Memoizes a function with strict equality, that loses laziness but increases sharing of memoized results
memoStrict :: Hash a => (a -> b) -> a -> b
memoStrict = memo True

-- | @memo@ takes a function with arbitrary range and domain, and returns a memoized version of the function
memo :: Hash a => Bool -> (a -> b) -> a -> b
memo isStrict f =
	let (tbl,weak) = unsafePerformIO $ do
		tbl <- newSNMap
		weak <- mkWeak tbl tbl $ Just $ table_finalizer tbl
		return (tbl,weak)
	in memo' isStrict f tbl weak

table_finalizer :: SNMap (Weak b) -> IO ()
table_finalizer tbl = do
	pairs <- snMapElems tbl
	sequence_ [ finalize w | (_,w) <- pairs ]

memo' :: Hash a => Bool -> (a -> b) -> MemoTable b -> Weak (MemoTable b) -> a -> b
memo' isStrict f tbl weak_tbl arg = unsafePerformIO $ do
	let val = f arg
	let sn = if isStrict then hashVal $! arg else hashVal arg
	let not_found = do
		weak <- mkWeak arg val $ Just $ finalizer tbl sn weak_tbl
		insertSNMap tbl sn weak
		return val
	lkp <- lookupSNMap tbl sn
	case lkp of
		Nothing -> not_found
		Just w -> do
			maybe_val <- deRefWeak w
			case maybe_val of
				Nothing -> not_found
				Just val -> return val   

finalizer :: MemoTable b -> Int -> Weak (MemoTable b) -> IO ()
finalizer tbl sn weak_tbl = do
	r <- deRefWeak weak_tbl
	case r of
		Nothing -> return ()
		Just mvar -> removeSNMap tbl sn



