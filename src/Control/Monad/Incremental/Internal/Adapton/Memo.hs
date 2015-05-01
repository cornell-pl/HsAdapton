{-# LANGUAGE CPP, DeriveDataTypeable, UndecidableInstances, Rank2Types, BangPatterns, FunctionalDependencies, MultiParamTypeClasses, MagicHash, ScopedTypeVariables, GADTs, FlexibleContexts, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module Control.Monad.Incremental.Internal.Adapton.Memo (
	memoNonRecU,memoNonRecUAs, Memo(..), Hashable(..),MemoPolicy(..)
	, GenericQMemoU(..),MemoCtx(..),Sat(..),gmemoNonRecU,gmemoNonRecUAs,proxyMemoCtx,NewGenericQ(..),NewGenericQMemo(..),NewGenericQMemoU(..)
	) where

import Data.IORef

 as WeakKey
import System.Mem.MemoTable as MemoTable
import Data.Hashable
import Control.Monad.Incremental.Internal.Adapton.Types
import Control.Monad.Incremental.Internal.Adapton.Layers
import Control.Monad.Incremental.Internal.Adapton.Display
import Data.Unique
import Control.Monad.IO.Class
import Debug
import System.Mem.StableName.Exts
import Control.Concurrent
import System.Mem.Weak.Exts as Weak
import Control.Monad.Incremental

import Control.Monad.Trans
import System.IO.Unsafe
import Data.Typeable
import Data.WithClass.MData
import Control.Monad
import Data.Maybe
import Data.Dynamic
import Data.WithClass.MGenerics.Aliases
import Data.Strict.Tuple
import Data.Memo
import Data.Global.Dynamic as Dyn
import Control.Monad.ST

--- * Generic memoization

type GenericQMemoU ctx l inc b = GenericQMemo ctx U l inc b

-- | An encapsulated generic query
newtype NewGenericQ ctx m b = NewGenericQ { unNewGenericQ :: GenericQ ctx m b } deriving Typeable

type NewGenericQMemo ctx (thunk :: (* -> * -> *) -> * -> * -> *) l inc b = NewGenericQ (MemoCtx ctx) (l inc) (thunk l inc b)
type NewGenericQMemoU ctx l inc b = NewGenericQMemo ctx U l inc b

gmemoNonRecU :: (Typeable inc,Typeable ctx,Typeable b,Layer Inside inc) => Proxy ctx -> GenericQMemoU ctx Inside inc b -> GenericQMemoU ctx Inside inc b
gmemoNonRecU ctx f = unNewGenericQ (newGmemoNonRecU ctx (NewGenericQ f))
	where
	newGmemoNonRecU ctx f = gmemoNonRecU' (adaptonMemoPolicy $ unsafePerformIO $ readIORef adaptonParams) ctx f $ debug "NewTable!!" $ Dyn.declareWeakBasicHashTable (adaptonMemoSize $ unsafePerformIO $ readIORef adaptonParams) (stableName f)

-- The Haskell type system is very reluctant to accept this type signature, so we need a newtype to work around it
gmemoNonRecUAs :: (Typeable inc,Memo name,Typeable ctx,Typeable b,Layer Inside inc) => Proxy ctx -> name -> GenericQMemoU ctx Inside inc b -> GenericQMemoU ctx Inside inc b
gmemoNonRecUAs ctx name f = unNewGenericQ (newGmemoNonRecU ctx (NewGenericQ f))
	where
	newGmemoNonRecU ctx f = gmemoNonRecU' (adaptonMemoPolicy $ unsafePerformIO $ readIORef adaptonParams) ctx f $ debug "NewTable!!" $ Dyn.declareWeakBasicHashTable (adaptonMemoSize $ unsafePerformIO $ readIORef adaptonParams) (memoKey name)

-- | memoizes a generic function on values
gmemoNonRecU' :: (Layer Inside inc) => MemoPolicy -> Proxy ctx -> NewGenericQMemoU ctx Inside inc b -> MemoTable (TypeRep,KeyDynamic) (U Inside inc b) -> NewGenericQMemoU ctx Inside inc b
gmemoNonRecU' mode ctx (NewGenericQ f) tbl = NewGenericQ $ \arg -> do
	let (mkWeak,k) = memoWeakKeyCtx dict ctx $! arg
	let tyk = (typeRepOf arg,keyDynamicCtx dict ctx (proxyOf arg) k)
	lkp <- debug ("memo search ") $ unsafeIOToInc $ MemoTable.lookup tbl tyk
	case lkp of
		Nothing -> do
			thunk <- f arg
			let thunkWeak = case mode of
				MemoLinear -> MkWeak (mkWeakRefKey (dataU thunk)) `andMkWeak` mkWeak
				MemoSuperlinear -> mkWeak
			unsafeIOToInc $ MemoTable.insertWithMkWeak tbl tyk thunk thunkWeak
			debug (" => "++show thunk) $ return thunk
		Just thunk -> debug ("memo hit " ++ " " ++ show thunk) $ return thunk

-- *		

memoNonRecU :: (Typeable inc,Typeable a,Typeable b,Memo a,Layer Inside inc) => (a -> Inside inc (U Inside inc b)) -> a -> Inside inc (U Inside inc b)
memoNonRecU f =
	let tbl = debug "NewTable!!!" $ Dyn.declareWeakBasicHashTable (adaptonMemoSize $ unsafePerformIO $ readIORef adaptonParams) (stableName f)
	in  memoNonRecU' (adaptonMemoPolicy $ unsafePerformIO $ readIORef adaptonParams) f tbl

memoNonRecUAs :: (Typeable inc,Memo name,Typeable a,Typeable b,Memo a,Layer Inside inc) => name -> (a -> Inside inc (U Inside inc b)) -> a -> Inside inc (U Inside inc b)
memoNonRecUAs name f =
	let tbl = debug "NewTable!!!" $ Dyn.declareWeakBasicHashTable (adaptonMemoSize $ unsafePerformIO $ readIORef adaptonParams) (memoKey name)
	in  memoNonRecU' (adaptonMemoPolicy $ unsafePerformIO $ readIORef adaptonParams) f tbl

memoNonRecU' :: (Memo a,Layer Inside inc) => MemoPolicy -> (a -> Inside inc (U Inside inc b)) -> MemoTable (Key a) (U Inside inc b) -> a -> Inside inc (U Inside inc b)
memoNonRecU' mode f tbl arg = do
		let (mkWeak,k) = (memoWeak $! arg,memoKey $! arg)
		lkp <- debug ("memo search with key ") $ do
			unsafeIOToInc $ MemoTable.lookup tbl k
		case lkp of
			Nothing -> do
				thunk <- f arg
				let thunkWeak = case mode of
					MemoLinear -> MkWeak (mkWeakKey (dataU thunk)) `andMkWeak` mkWeak
					MemoSuperlinear -> mkWeak
				unsafeIOToInc $ MemoTable.insertWithMkWeak tbl k thunk thunkWeak
				debug (" => "++show thunk) $ return thunk
			Just thunk -> debug ("memoM hit " ++ " " ++ show thunk) $ return thunk

instance (Typeable l,Typeable inc,Typeable a) => Memo (M l inc a) where
	type Key (M l inc a) = Unique
	{-# INLINE memoKey #-}
	memoKey = idNM . metaM
	{-# INLINE memoWeak #-}
	memoWeak t = MkWeak $ mkWeakRefKey (dataM t)
                                 
instance (Typeable l,Typeable inc,Typeable a) => Memo (U l inc a) where
	type Key (U l inc a) = Unique
	{-# INLINE memoKey #-}
	memoKey = idU
	{-# INLINE memoWeak #-}
	memoWeak t = MkWeak $ mkWeakRefKey (dataU t)

instance (Typeable l,Typeable inc,Typeable a) => Memo (L l inc a) where
	type Key (L l inc a) = Unique
	{-# INLINE memoKey #-}
	memoKey = idNM . metaL
	{-# INLINE memoWeak #-}
	memoWeak t = MkWeak $ mkWeakRefKey (dataL t)

instance Hashable (U l inc a) where
	hashWithSalt i u = hashWithSalt i (idU u)
instance Hashable (M l inc a) where
	hashWithSalt i m = hashWithSalt i (idNM $ metaM m)
instance Hashable (L l inc a) where
	hashWithSalt i l = hashWithSalt i (idNM $ metaL l)
