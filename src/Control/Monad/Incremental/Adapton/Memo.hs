{-# LANGUAGE DeriveDataTypeable, UndecidableInstances, Rank2Types, BangPatterns, FunctionalDependencies, MultiParamTypeClasses, MagicHash, ScopedTypeVariables, GADTs, FlexibleContexts, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module Control.Monad.Incremental.Adapton.Memo (
	memoNonRecU,memoNonRecUNamed, Memo(..), Hashable(..),MemoMode(..)
	, GenericQMemoU(..),MemoCtx(..),Sat(..),gmemoNonRecU,gmemoNonRecUNamed,proxyMemoCtx,NewGenericQ(..),NewGenericQMemo(..),NewGenericQMemoU(..)
	) where

import System.Mem.WeakKey as WeakKey
import System.Mem.WeakTable as WeakTable
import System.Mem.MemoTable
import Data.Hashable
import Control.Monad.Incremental.Adapton.Types
import Control.Monad.Incremental.Adapton.Layers
import Control.Monad.Incremental.Adapton.Display
import Data.Unique
import Control.Monad.IO.Class
import Debug
import Control.Concurrent
import System.Mem.Weak as Weak
import qualified System.Mem.WeakTable as WeakTable
import Control.Monad.Incremental
import Control.Monad.Ref
import Control.Monad.Trans
import System.IO.Unsafe
import Data.Typeable
import Data.WithClass.MData
import Control.Monad
import Data.Maybe
import Data.Dynamic
import Data.WithClass.MGenerics.Aliases
import Data.Strict.Tuple
import qualified Data.HashTable.IO as HashIO
import qualified Data.HashTable.ST.Basic as HashST
import Data.Global.Dynamic

--- * Generic memoization

type GenericQMemoU ctx l inc r m b = GenericQMemo ctx U l inc r m b

-- | An encapsulated generic query
newtype NewGenericQ ctx m b = NewGenericQ { unNewGenericQ :: GenericQ ctx m b } deriving Typeable

type NewGenericQMemo ctx (thunk :: (* -> (* -> *) -> (* -> *) -> * -> *) -> * -> (* -> *) -> (* -> *) -> * -> *) l inc r m b = NewGenericQ (MemoCtx ctx) (l inc r m) (thunk l inc r m b)
type NewGenericQMemoU ctx l inc r m b = NewGenericQMemo ctx U l inc r m b

gmemoNonRecU :: (Typeable ctx,Typeable b,MonadRef r m,MonadIO m,Layer Inside inc r m) => MemoMode -> Proxy ctx -> GenericQMemoU ctx Inside inc r m b -> GenericQMemoU ctx Inside inc r m b
gmemoNonRecU mode ctx f = unNewGenericQ (newGmemoNonRecU ctx (NewGenericQ f)) where
	newGmemoNonRecU ctx f = gmemoNonRecU' mode ctx f (debug "NewTable!!" $ declareWeakTable f (stableName f))

-- The Haskell type system is very reluctant to accept this type signature, so we need a newtype to work around it
gmemoNonRecUNamed :: (Memo name,Typeable ctx,Typeable b,MonadRef r m,MonadIO m,Layer Inside inc r m) => MemoMode -> Proxy ctx -> name -> GenericQMemoU ctx Inside inc r m b -> GenericQMemoU ctx Inside inc r m b
gmemoNonRecUNamed mode ctx name f = unNewGenericQ (newGmemoNonRecU ctx (NewGenericQ f)) where
	newGmemoNonRecU ctx f = gmemoNonRecU' mode ctx f (debug "NewTable!!" $ declareWeakTable f (memoKey name))

-- | memoizes a generic function on values
gmemoNonRecU' :: (MonadRef r m,MonadIO m,Layer Inside inc r m) => MemoMode -> Proxy ctx -> NewGenericQMemoU ctx Inside inc r m b -> MemoTable (TypeRep,KeyDynamic) (U Inside inc r m b) -> NewGenericQMemoU ctx Inside inc r m b
gmemoNonRecU' mode ctx (NewGenericQ f) tbl = NewGenericQ $ \arg -> do
	let (mkWeak,k) = memoWeakKeyCtx dict ctx $! arg
	let tyk = (typeRepOf arg,keyDynamicCtx dict ctx (proxyOf arg) k)
	lkp <- debug ("memo search ") $ inL $ liftIO $ WeakTable.lookup tbl tyk
	case lkp of
		Nothing -> do
			thunk <- f arg
			let thunkWeak = case mode of
				MemoLinear -> MkWeak (WeakKey.mkWeakRefKey (dataU thunk)) `andMkWeak` mkWeak
				MemoSuperlinear -> mkWeak
			inL $ liftIO $ WeakTable.insertWithMkWeak tbl thunkWeak tyk thunk
			debug (" => "++show thunk) $ return thunk
		Just thunk -> debug ("memo hit " ++ " " ++ show thunk) $ return thunk

-- *		

data MemoMode = MemoLinear -- stores at most as much space as a single run of the program; users have to explicitely retain output thunks for them to remain memoized
			  | MemoSuperlinear -- stores all previous executions for live inputs; users do not have to explicitely retain output thunks for them to remain memoized

memoNonRecU :: (Typeable a,Typeable b,MonadRef r m,MonadIO m,Memo a,Layer Inside inc r m) => MemoMode -> (a -> Inside inc r m (U Inside inc r m b)) -> a -> Inside inc r m (U Inside inc r m b)
memoNonRecU mode f =
	let tbl = debug "NewTable!!!" $ declareWeakTable f (stableName f)
	in memoNonRecU' mode f tbl

memoNonRecUNamed :: (Memo name,Typeable a,Typeable b,MonadRef r m,MonadIO m,Memo a,Layer Inside inc r m) => MemoMode -> name -> (a -> Inside inc r m (U Inside inc r m b)) -> a -> Inside inc r m (U Inside inc r m b)
memoNonRecUNamed mode name f =
	let tbl = debug "NewTable!!!" $ declareWeakTable f (memoKey name)
	in memoNonRecU' mode f tbl

memoNonRecU' :: (MonadRef r m,MonadIO m,Memo a,Layer Inside inc r m) => MemoMode -> (a -> Inside inc r m (U Inside inc r m b)) -> MemoTable (Key a) (U Inside inc r m b) -> a -> Inside inc r m (U Inside inc r m b)
memoNonRecU' mode f tbl arg = do
		let (mkWeak,k) = (memoWeak $! arg,memoKey $! arg)
		lkp <- debug ("memo search with key ") $ do
			inL $ liftIO $ WeakTable.lookup tbl k
		case lkp of
			Nothing -> do
				thunk <- f arg
				let thunkWeak = case mode of
					MemoLinear -> MkWeak (WeakKey.mkWeakRefKey (dataU thunk)) `andMkWeak` mkWeak
					MemoSuperlinear -> mkWeak
				inL $ liftIO $ WeakTable.insertWithMkWeak tbl thunkWeak k thunk
				debug (" => "++show thunk) $ return thunk
			Just thunk -> debug ("memoM hit " ++ " " ++ show thunk) $ return thunk

instance (Typeable l,Typeable inc,Typeable r,Typeable m,Typeable a,WeakRef r) => Memo (M l inc r m a) where
	type Key (M l inc r m a) = Unique
	{-# INLINE memoKey #-}
	memoKey = idNM . metaM
	{-# INLINE memoWeak #-}
	memoWeak t = MkWeak $ WeakKey.mkWeakRefKey (dataM t)
                                 
instance (Typeable l,Typeable inc,Typeable r,Typeable m,Typeable a,WeakRef r) => Memo (U l inc r m a) where
	type Key (U l inc r m a) = Unique
	{-# INLINE memoKey #-}
	memoKey = idU
	{-# INLINE memoWeak #-}
	memoWeak t = MkWeak $ WeakKey.mkWeakRefKey (dataU t)

instance (Typeable l,Typeable inc,Typeable r,Typeable m,Typeable a,WeakRef r) => Memo (L l inc r m a) where
	type Key (L l inc r m a) = Unique
	{-# INLINE memoKey #-}
	memoKey = idNM . metaL
	{-# INLINE memoWeak #-}
	memoWeak t = MkWeak $ WeakKey.mkWeakRefKey (dataL t)

instance Hashable (U l inc r m a) where
	hashWithSalt i u = hashWithSalt i (idU u)
instance Hashable (M l inc r m a) where
	hashWithSalt i m = hashWithSalt i (idNM $ metaM m)
instance Hashable (L l inc r m a) where
	hashWithSalt i l = hashWithSalt i (idNM $ metaL l)
