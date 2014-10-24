{-# LANGUAGE UndecidableInstances, Rank2Types, BangPatterns, FunctionalDependencies, MultiParamTypeClasses, MagicHash, ScopedTypeVariables, GADTs, FlexibleContexts, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module Control.Monad.Incremental.Adapton.Memo (
	memoNonRecU, Memo(..), Hashable(..)
	, GenericQMemoU(..),MemoCtx(..),Sat(..),gmemoNonRecU,proxyMemoCtx
	) where

import System.Mem.WeakRef as WeakRef
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
import System.Mem.Weak
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

--- * Generic memoization

type GenericQMemoU ctx l inc r m b = GenericQMemo ctx U l inc r m b

gmemoNonRecU :: (MonadRef r m,MonadIO m,Layer Inside inc r m) => Proxy ctx -> GenericQMemoU ctx Inside inc r m b -> GenericQMemoU ctx Inside inc r m b
gmemoNonRecU ctx f = gmemoNonRecU' ctx f (unsafePerformIO $ debug "NewTable!!" $ WeakTable.newFor f)

-- | memoizes a generic function on values
gmemoNonRecU' :: (MonadRef r m,MonadIO m,Layer Inside inc r m) => Proxy ctx -> GenericQMemoU ctx Inside inc r m b -> MemoTable (TypeRep,KeyDynamic) (U Inside inc r m b) -> GenericQMemoU ctx Inside inc r m b
gmemoNonRecU' ctx (GenericQMemo f) tbl = GenericQMemo $ \arg -> do
	let (mkWeak,k) = memoKeyCtx dict ctx $! arg
	let tyk = (typeRepOf arg,keyDynamicCtx dict ctx (proxyOf arg) k)
	lkp <- debug ("memo search "++show tyk) $ inL $ liftIO $ WeakTable.lookup tbl tyk
	case lkp of
		Nothing -> do
			let finalizethunk = WeakTable.finalize tbl tyk
			thunk <- f arg
			let thunkmemo = addFinalizerU thunk (liftIO finalizethunk)
			inL $ liftIO $ WeakTable.insertWithMkWeak tbl mkWeak tyk thunk
			debug (show tyk ++" => "++show thunk) $ return thunkmemo
		Just thunk -> debug ("memo hit "++show tyk ++ " " ++ show thunk) $ do
			let finalizethunk = WeakTable.finalize tbl tyk
			let thunkmemo = addFinalizerU thunk (liftIO finalizethunk)
			return thunkmemo

-- *		

memoNonRecU :: (MonadRef r m,MonadIO m,Memo a,Layer Inside inc r m) => (a -> Inside inc r m (U Inside inc r m b)) -> a -> Inside inc r m (U Inside inc r m b)
memoNonRecU f = do
	let tbl = unsafePerformIO $ debug "NewTable!!!!" $ WeakTable.newFor f
	memoNonRecU' f tbl

memoNonRecU' :: (MonadRef r m,MonadIO m,Memo a,Layer Inside inc r m) => (a -> Inside inc r m (U Inside inc r m b)) -> MemoTable (Key a) (U Inside inc r m b) -> a -> Inside inc r m (U Inside inc r m b)
memoNonRecU' f tbl arg = do
		let (mkWeak,k) = memoKey $! arg
		lkp <- debug ("memo search with key " ++ show k) $ do
--			inL $ liftIO $ WeakTable.debugWeakTable tbl
			inL $ liftIO $ WeakTable.lookup tbl k
		case lkp of
			Nothing -> do
				let finalizethunk = WeakTable.finalize tbl k
				thunk <- f arg
				let thunkmemo = addFinalizerU thunk (liftIO finalizethunk) -- change thunk so that whenever it is modified directly (by set), its memo table entry is removed
				inL $ liftIO $ WeakTable.insertWithMkWeak tbl mkWeak k thunk
				debug (show k ++" => "++show thunk) $ return thunkmemo
			Just thunk -> debug ("memoM hit " ++show k ++ " " ++ show thunk) $ do
				let finalizethunk = WeakTable.finalize tbl k
				let thunkmemo = addFinalizerU thunk (liftIO finalizethunk) -- change thunk so that whenever it is modified directly (by set), its memo table entry is removed
				return thunkmemo

instance WeakRef r => Memo (M l inc r m a) where
	type Key (M l inc r m a) = Unique
	{-# INLINE memoKey #-}
	memoKey t = (MkWeak $ WeakRef.mkWeakWithRefKey (dataM t),idNM $ metaM t)
                                 
instance WeakRef r => Memo (U l inc r m a) where
	type Key (U l inc r m a) = Unique
	{-# INLINE memoKey #-}
	memoKey t = (MkWeak $ WeakRef.mkWeakWithRefKey (dataU t),idU t)

instance WeakRef r => Memo (L l inc r m a) where
	type Key (L l inc r m a) = Unique
	{-# INLINE memoKey #-}
	memoKey t = (MkWeak $ WeakRef.mkWeakWithRefKey (dataL t),idNM $ metaL t)



instance Hashable (U l inc r m a) where
	hashWithSalt i u = hashWithSalt i (idU u)
instance Hashable (M l inc r m a) where
	hashWithSalt i m = hashWithSalt i (idNM $ metaM m)
instance Hashable (L l inc r m a) where
	hashWithSalt i l = hashWithSalt i (idNM $ metaL l)