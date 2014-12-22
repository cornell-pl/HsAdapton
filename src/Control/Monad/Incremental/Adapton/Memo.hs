{-# LANGUAGE UndecidableInstances, Rank2Types, BangPatterns, FunctionalDependencies, MultiParamTypeClasses, MagicHash, ScopedTypeVariables, GADTs, FlexibleContexts, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module Control.Monad.Incremental.Adapton.Memo (
	memoNonRecU, Memo(..), Hashable(..),MemoMode(..)
	, GenericQMemoU(..),MemoCtx(..),Sat(..),gmemoNonRecU,proxyMemoCtx,NewGenericQ(..),NewGenericQMemo(..),NewGenericQMemoU(..)
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

--- * Generic memoization

type GenericQMemoU ctx l inc r m b = GenericQMemo ctx U l inc r m b

-- | An encapsulated generic query
newtype NewGenericQ ctx m b = NewGenericQ { unNewGenericQ :: GenericQ ctx m b }

type NewGenericQMemo ctx (thunk :: (* -> (* -> *) -> (* -> *) -> * -> *) -> * -> (* -> *) -> (* -> *) -> * -> *) l inc r m b = NewGenericQ (MemoCtx ctx) (l inc r m) (thunk l inc r m b)
type NewGenericQMemoU ctx l inc r m b = NewGenericQMemo ctx U l inc r m b

-- The Haskell type system is very reluctant to accept this type signature, so we need a newtype to work around it
gmemoNonRecU :: (MonadRef r m,MonadIO m,Layer Inside inc r m) => MemoMode -> Proxy ctx -> GenericQMemoU ctx Inside inc r m b -> GenericQMemoU ctx Inside inc r m b
gmemoNonRecU mode ctx f = unNewGenericQ (newGmemoNonRecU ctx (NewGenericQ f)) where
	newGmemoNonRecU ctx f = gmemoNonRecU' mode ctx f (unsafePerformIO $ debug "NewTable!!" $ WeakTable.newFor f)

-- | memoizes a generic function on values
gmemoNonRecU' :: (MonadRef r m,MonadIO m,Layer Inside inc r m) => MemoMode -> Proxy ctx -> NewGenericQMemoU ctx Inside inc r m b -> MemoTable (TypeRep,KeyDynamic) (U Inside inc r m b) -> NewGenericQMemoU ctx Inside inc r m b
gmemoNonRecU' mode ctx (NewGenericQ f) tbl = NewGenericQ $ \arg -> do
	let (mkWeak,k) = memoKeyCtx dict ctx $! arg
	let tyk = (typeRepOf arg,keyDynamicCtx dict ctx (proxyOf arg) k)
	lkp <- debug ("memo search "++show tyk) $ inL $ liftIO $ WeakTable.lookup tbl tyk
	case lkp of
		Nothing -> do
			thunk <- f arg
			let thunkWeak = case mode of
				MemoLinear -> MkWeak (WeakKey.mkWeakRefKey (dataU thunk)) `andMkWeak` mkWeak
				MemoSuperlinear -> mkWeak
			inL $ liftIO $ WeakTable.updateWithMkWeak tbl thunkWeak tyk thunk
			debug (show tyk ++" => "++show thunk) $ return thunk
		Just thunk -> debug ("memo hit "++show tyk ++ " " ++ show thunk) $ return thunk

-- *		

data MemoMode = MemoLinear -- stores at most as much space as a single run of the program; users have to explicitely retain output thunks for them to remain memoized
			  | MemoSuperlinear -- stores all previous executions for live inputs; users do not have to explicitely retain output thunks for them to remain memoized

memoNonRecU :: (MonadRef r m,MonadIO m,Memo a,Layer Inside inc r m) => MemoMode -> (a -> Inside inc r m (U Inside inc r m b)) -> a -> Inside inc r m (U Inside inc r m b)
memoNonRecU mode f =
	let tbl = debug "NewTable!!!" $ WeakTable.unsafeNewFor f
	in memoNonRecU' mode f tbl

memoNonRecU' :: (MonadRef r m,MonadIO m,Memo a,Layer Inside inc r m) => MemoMode -> (a -> Inside inc r m (U Inside inc r m b)) -> MemoTable (Key a) (U Inside inc r m b) -> a -> Inside inc r m (U Inside inc r m b)
memoNonRecU' mode f tbl arg = do
		let (mkWeak,k) = memoKey $! arg
		lkp <- debug ("memo search with key " ++ show k) $ do
			inL $ liftIO $ WeakTable.lookup tbl k
		case lkp of
			Nothing -> do
				thunk <- f arg
				let thunkWeak = case mode of
					MemoLinear -> MkWeak (WeakKey.mkWeakRefKey (dataU thunk)) `andMkWeak` mkWeak
					MemoSuperlinear -> mkWeak
				inL $ liftIO $ WeakTable.updateWithMkWeak tbl thunkWeak k thunk
				debug (show k ++" => "++show thunk) $ return thunk
			Just thunk -> debug ("memoM hit " ++show k ++ " " ++ show thunk) $ return thunk

instance WeakRef r => Memo (M l inc r m a) where
	type Key (M l inc r m a) = Unique
	{-# INLINE memoKey #-}
	memoKey t = (MkWeak $ WeakKey.mkWeakRefKey (dataM t),idNM $ metaM t)
                                 
instance WeakRef r => Memo (U l inc r m a) where
	type Key (U l inc r m a) = Unique
	{-# INLINE memoKey #-}
	memoKey t = (MkWeak $ WeakKey.mkWeakRefKey (dataU t),idU t)

instance WeakRef r => Memo (L l inc r m a) where
	type Key (L l inc r m a) = Unique
	{-# INLINE memoKey #-}
	memoKey t = (MkWeak $ WeakKey.mkWeakRefKey (dataL t),idNM $ metaL t)



instance Hashable (U l inc r m a) where
	hashWithSalt i u = hashWithSalt i (idU u)
instance Hashable (M l inc r m a) where
	hashWithSalt i m = hashWithSalt i (idNM $ metaM m)
instance Hashable (L l inc r m a) where
	hashWithSalt i l = hashWithSalt i (idNM $ metaL l)
