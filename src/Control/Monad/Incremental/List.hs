{-# LANGUAGE TupleSections, TypeFamilies, ScopedTypeVariables, TemplateHaskell, StandaloneDeriving, KindSignatures, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances, DeriveDataTypeable #-}

module Control.Monad.Incremental.List where

import System.Random
import Data.WithClass.MData
import Control.Monad.Incremental
import Data.Proxy
import Control.Monad.Incremental.Display
import Control.Monad.Incremental.LazyNonInc
import Control.Monad.Incremental.Adapton hiding (MData)
import Data.IORef
import Prelude hiding (mod,const,read)
import qualified Prelude
import Data.WithClass.MGenerics.Instances

import Data.DeriveTH                 -- Library for deriving instances for existing types
import Data.DeepTypeable
import Data.WithClass.Derive.DeepTypeable
import Language.Haskell.TH.Syntax hiding (lift,Infix,Fixity)

import Control.Monad
import Control.Monad.IO.Class
import System.Mem.StableName
import System.IO.Unsafe
import System.Mem.WeakKey
import System.Mem.Weak as Weak

-- * standard lists

data ListMod'
	(mod :: ((* -> (* -> *) -> (* -> *) -> * -> *) -> * -> (* -> *) -> (* -> *) -> * -> *))
	(l :: * -> (* -> *) -> (* -> *) -> * -> *)
	inc
	(r :: * -> *)
	(m :: * -> *)
	a
	= NilMod | ConsMod a (ListMod mod l inc r m a)

type ListMod mod l inc r m a = mod l inc r m (ListMod' mod l inc r m a)

deriving instance Typeable ListMod'
deriving instance (Eq a,Eq (ListMod mod l inc r m a)) => Eq (ListMod' mod l inc r m a)

instance (DeepTypeable mod,DeepTypeable l,DeepTypeable inc,DeepTypeable r,DeepTypeable m,DeepTypeable a,DeepTypeable (ListMod mod l inc r m a)) => DeepTypeable (ListMod' mod l inc r m a) where
	typeTree (_::Proxy (ListMod' mod l inc r m a)) = MkTypeTree (mkName "Control.Monad.Incremental.List.ListMod'") args [MkConTree (mkName "Control.Monad.Incremental.List.NilMod") [],MkConTree (mkName "Control.Monad.Incremental.List.ConsMod") [typeTree (Proxy::Proxy a),typeTree (Proxy::Proxy (ListMod mod l inc r m a))]]
		where args = [typeTree (Proxy::Proxy mod),typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy r),typeTree (Proxy::Proxy m),typeTree (Proxy::Proxy a)]

type ListU l r m a = ListMod U l Adapton r m a
type ListU' l r m a = ListMod' U l Adapton r m a

type ListM l r m a = ListMod M l Adapton r m a
type ListM' l r m a = ListMod' M l Adapton r m a

type ListL l r m a = ListMod L l Adapton r m a
type ListL' l r m a = ListMod' L l Adapton r m a

type ListLazyNonIncU l r m a = ListMod LazyNonIncU l LazyNonInc r m a
type ListLazyNonIncU' l r m a = ListMod' LazyNonIncU l LazyNonInc r m a

type ListLazyNonIncL l r m a = ListMod LazyNonIncL l LazyNonInc r m a
type ListLazyNonIncL' l r m a = ListMod' LazyNonIncL l LazyNonInc r m a

type ListLazyNonIncM l r m a = ListMod LazyNonIncM l LazyNonInc r m a
type ListLazyNonIncM' l r m a = ListMod' LazyNonIncM l LazyNonInc r m a

instance (Display l1 inc r m a,Display l1 inc r m (ListMod mod l inc r m a)) => Display l1 inc r m (ListMod' mod l inc r m a) where
	displaysPrec NilMod r = return $ "NilMod" ++ r
	displaysPrec (ConsMod x mxs) rest = do
		sxs <- displaysPrec mxs (')':rest)
		sx <- displaysPrec x (' ':sxs)
		return $ "(ConsMod " ++ sx

--instance (Serialize l1 inc r m a,Serialize l1 inc r m (ListMod mod l inc r m a)) => Serialize l1 inc r m (ListMod' mod l inc r m a) where
--	serialize proxy NilMod = fromString "NilMod"
--	serialize proxy (ConsMod x mxs) = mconcat
--		[ fromString "(ConsMod "
--		, serialize proxy x
--		, fromChar ' '
--		, serialize proxy mxs
--		, fromChar ')'
--		]

instance (NFDataInc l1 inc r m a,NFDataInc l1 inc r m (ListMod mod l inc r m a)) => NFDataInc l1 inc r m (ListMod' mod l inc r m a) where
	rnfInc NilMod = return $! ()
	rnfInc (ConsMod x mxs) = do
		a <- rnfInc x
		b <- rnfInc mxs
		return $! a `seq` b

instance (DeepTypeable mod,DeepTypeable inc,DeepTypeable r,DeepTypeable m,DeepTypeable l,Sat (ctx (ListMod' mod l inc r m a)),MData ctx (l1 inc r m) a,MData ctx (l1 inc r m) (ListMod mod l inc r m a))
			=> MData ctx (l1 inc r m) (ListMod' mod l inc r m a) where
      gfoldl ctx k z NilMod = z NilMod
      gfoldl ctx k z (ConsMod x1 x2) = (((z (\mx1 -> return (\mx2 -> mx1 >>= \x1 -> mx2 >>= \x2 -> return (ConsMod x1 x2)))) >>= (flip k $ return x1)) >>= (flip k $ return x2))
      gunfold ctx k z c = case constrIndex c of
            1 -> z NilMod
            2 -> (((z (\mx1 -> return (\mx2 -> mx1 >>= \x1 -> mx2 >>= \x2 -> return (ConsMod x1 x2)))) >>= k) >>= k)
      toConstr ctx x@NilMod = ((dataTypeOf ctx x) >>= (return . (flip indexConstr 1)))
      toConstr ctx x@(ConsMod x1 x2) = ((dataTypeOf ctx x) >>= (return . (flip indexConstr 2)))
      dataTypeOf ctx x = return ty where
            ty = mkDataType "Todo.ListMod'" [mkConstr ty "NilMod" [] Prefix,mkConstr ty "ConsMod" [] Prefix]

-- a simpler version where input and output thunks are the same
takeInc' :: (Eq a,Eq (ListMod thunk l inc r m a),Memo (ListMod thunk l inc r m a),Output thunk l inc r m) => Int -> ListMod thunk l inc r m a -> l inc r m (ListMod thunk l inc r m a)
takeInc' = takeInc

-- | take
takeInc :: (Eq a,Eq (ListMod thunk l inc r m a),Memo (ListMod mod l inc r m a),Output thunk l inc r m,Eq (ListMod mod l inc r m a),Thunk mod l inc r m) => Int -> ListMod mod l inc r m a -> l inc r m (ListMod thunk l inc r m a)
takeInc = memo2 $ \recur i mxs -> case i of
	0 -> return NilMod
	n -> read mxs >>= \xs -> case xs of
		ConsMod x mxs' -> liftM (ConsMod x) $ recur (n-1) mxs'
		NilMod -> return NilMod

-- | filter
filterInc :: (Thunk mod l inc r m,Eq (ListMod thunk l inc r m a),MonadIO m,Memo (ListMod mod l inc r m a),Eq a,Eq (ListMod mod l inc r m a),Output thunk l inc r m)
	=> (a -> Bool) -> ListMod mod l inc r m a -> l inc r m (ListMod thunk l inc r m a)
filterInc p = memo $ \recur mxs -> read mxs >>= \xs -> case xs of
	ConsMod x mxs -> if p x
		then liftM (ConsMod x) $ recur mxs
		else recur mxs >>= force
	NilMod -> return NilMod
	
-- | filter
filterWithKeyInc :: (Thunk mod l inc r m,Eq (ListMod thunk l inc r m a),MonadIO m,Memo (ListMod mod l inc r m a),Eq a,Eq (ListMod mod l inc r m a),Output thunk l inc r m)
	=> (a -> a -> Bool) -> a -> ListMod mod l inc r m a -> l inc r m (ListMod thunk l inc r m a)
filterWithKeyInc cmp k = memo $ \recur mxs -> read mxs >>= \xs -> case xs of
	ConsMod x mxs -> if cmp k x
		then liftM (ConsMod x) $ recur mxs
		else recur mxs >>= force
	NilMod -> return NilMod

-- | length
lengthInc :: (Thunk mod l inc r m,Eq a,Eq (ListMod mod l inc r m a),MonadIO m,Memo (ListMod mod l inc r m a),Layer l inc r m,Output thunk l inc r m) => ListMod mod l inc r m a -> l inc r m (thunk l inc r m Int)
lengthInc = memo $ \recur mxs -> read mxs >>= \xs -> case xs of
	ConsMod x mxs' -> recur mxs' >>= force >>= return . succ
	NilMod -> return 0

-- | list map
mapInc :: (Thunk mod l inc r m,Eq b,Eq (ListMod thunk l inc r m b),MonadIO m,Memo (ListMod mod l inc r m a),Eq a,Eq (ListMod mod l inc r m a),Output thunk l inc r m)
	=> (a -> l inc r m b) -> ListMod mod l inc r m a -> l inc r m (ListMod thunk l inc r m b)
mapInc f = memo $ \recur mxs -> read mxs >>= \xs -> case xs of
	NilMod -> return NilMod
	ConsMod x mxs -> do
		y <- f x
		mys <- recur mxs
		return $ ConsMod y mys

-- | fold1 with an associative operation
fold1Inc :: (Memo (ListMod mod l inc r m a),MonadIO m,Hashable (ListMod mod l inc r m a),Eq (ListMod mod l inc r m a),Output mod l inc r m,Eq a)
	=> (a -> a -> l inc r m a) -> ListMod mod l inc r m a -> l inc r m (mod l inc r m a)
fold1Inc f mxs = do
	g <- inL (liftIO newStdGen)
	let seeds = makeSeeds g
	memo2 (\recur seeds mxs -> force mxs >>= \xs -> case xs of
		ConsMod x' mxs' -> force mxs' >>= \xs' -> case xs' of
			ConsMod _ _ -> do
				let (seed,seeds') = popSeed seeds
				fold_pairs f seed mxs >>= recur seeds' >>= force
			NilMod -> return x'
		NilMod -> error "fold1 requires a non-empty list") seeds mxs
  where
	fold_pairs :: (Memo (mod l inc r m (ListMod' mod l inc r m a)),Eq a,Eq (ListMod mod l inc r m a),Output mod l inc r m,Hashable (ListMod mod l inc r m a)) => (a -> a -> l inc r m a) -> Int -> ListMod mod l inc r m a -> l inc r m (ListMod mod l inc r m a)
	fold_pairs f = memo2 (\recur seed mxs -> force mxs >>= \xs -> case xs of
		ConsMod x' mxs' -> do
			if (hashWithSalt seed mxs `Prelude.mod` 2 == 0)
				then liftM (ConsMod x') $ recur seed mxs'
				else force mxs' >>= \xs' -> case xs' of
					ConsMod x'' mxs'' -> f x' x'' >>= \y -> liftM (ConsMod y) $ recur seed mxs''
					NilMod -> return xs
		NilMod -> return NilMod)

-- | partition
partitionInc :: (Eq (ListMod mod l inc r m a),Memo (ListMod mod l inc r m a),Eq a,Output mod l inc r m) => (a -> Bool) -> ListMod mod l inc r m a -> l inc r m (mod l inc r m (ListMod mod l inc r m a,ListMod mod l inc r m a))
partitionInc p = memo $ \recur mxs -> force mxs >>= \xs -> case xs of
	ConsMod x mxs' -> do
		(left,right) <- recur mxs' >>= force
		if p x
			then const (ConsMod x left) >>= \left' -> return (left',right)
			else const (ConsMod x right) >>= \right' -> return (left,right')
	NilMod -> do
		nil <- const NilMod
		return (nil,nil)

-- | quicksort
quicksortInc :: (Eq (ListMod mod l inc r m a),Output mod l inc r m,Memo (ListMod mod l inc r m a),MonadIO m,Eq a)
	=> (a -> a -> Ordering) -> ListMod mod l inc r m a -> l inc r m (ListMod mod l inc r m a)
quicksortInc cmp (mxs :: ListMod mod l inc r m a) = do
	let filter_left = filterWithKeyInc (\k y -> cmp y k == LT)
	let filter_right = filterWithKeyInc (\k y -> cmp y k /= LT)
	(nil :: ListMod mod l inc r m a) <- const NilMod
	let quicksortInc' = memo2 $ \recur mxs rest -> force mxs >>= \xs -> case xs of
		ConsMod x mxs' -> do
			left <- filter_left x mxs'
			right <- filter_right x mxs'
			tright <- thunk $ liftM (ConsMod x) $ recur right rest
			recur left tright >>= force
--			recur right rest >>= const . ConsMod x >>= recur left >>= force
		NilMod -> force rest
	quicksortInc' mxs nil

-- | quicksort with partition (this IC version is actually slower than using two filters)
quicksortInc2 :: (Eq (ListMod mod l inc r m a),Output mod l inc r m,Memo (ListMod mod l inc r m a),MonadIO m,Eq a)
	=> (a -> a -> Ordering) -> ListMod mod l inc r m a -> l inc r m (ListMod mod l inc r m a)
quicksortInc2 cmp mxs = const NilMod >>= (quicksortInc2' cmp) mxs
  where
	quicksortInc2' :: (Eq (ListMod mod l inc r m a),Output mod l inc r m,Memo (ListMod mod l inc r m a),MonadIO m,Eq a)
		=> (a -> a -> Ordering) -> ListMod mod l inc r m a -> ListMod mod l inc r m a -> l inc r m (ListMod mod l inc r m a)
	quicksortInc2' cmp = memo2 (\recur mxs rest -> force mxs >>= \xs -> case xs of
		ConsMod x mxs' -> do
			(left,right) <- partitionInc (\y -> cmp y x == LT) mxs' >>= force
			tright <- thunk $ liftM (ConsMod x) $ recur right rest
			recur left tright >>= force
		NilMod -> force rest)

quicksortInc3 :: (Eq (ListMod mod l inc r m a),Output mod l inc r m,Memo (ListMod mod l inc r m a),MonadIO m,Eq a)
	=> (a -> a -> Ordering) -> ListMod mod l inc r m a -> l inc r m (ListMod mod l inc r m a)
quicksortInc3 cmp (mxs :: ListMod mod l inc r m a) = do
	(nil :: ListMod mod l inc r m a) <- const NilMod
	let quicksortInc' = memo2 $ \recur mxs rest -> force mxs >>= \xs -> case xs of
		ConsMod x mxs' -> do
			left <- filterInc (\y -> cmp y x == LT) mxs'
			right <- filterInc (\y -> cmp y x /= LT) mxs'
			tright <- thunk $ liftM (ConsMod x) $ recur right rest
			recur left tright >>= force
		NilMod -> force rest
	quicksortInc' mxs nil

-- | mergesort
mergesortInc :: (Memo (ListMod mod l inc r m (ListMod mod l inc r m a)),Memo a,Memo (mod l inc r m (ListMod' mod l inc r m a)),MonadIO m,Hashable (ListMod mod l inc r m (ListMod mod l inc r m a)),Eq (ListMod mod l inc r m (ListMod mod l inc r m a)),Output mod l inc r m,Eq (ListMod mod l inc r m a),Eq a)
	=> (a -> a -> Ordering) -> ListMod mod l inc r m a -> l inc r m (ListMod mod l inc r m a)
mergesortInc cmp mxs = do
	nil <- const NilMod
	memo (\_ mxs -> force mxs >>= \xs -> case xs of
		ConsMod _ _ -> liftInc nil mxs >>= fold1Inc (mergeInc cmp) >>= force >>= force
		NilMod -> return NilMod) mxs
  where
	-- | appends a list to each element of a list
	liftInc :: (MonadIO m,Eq a,Eq (ListMod mod l inc r m a),Output mod l inc r m,Memo a,Memo (ListMod mod l inc r m a),Eq (ListMod mod l inc r m (ListMod mod l inc r m a))) => ListMod mod l inc r m a -> ListMod mod l inc r m a -> l inc r m (ListMod mod l inc r m (ListMod mod l inc r m a))
	liftInc nil = mapInc (memo (\_ x -> return $ ConsMod x nil))

-- | merges two sorted lists
mergeInc :: (Memo (mod l inc r m (ListMod' mod l inc r m a)),Eq a,Output mod l inc r m,Eq (ListMod mod l inc r m a))
	=> (a -> a -> Ordering) -> ListMod mod l inc r m a -> ListMod mod l inc r m a -> l inc r m (ListMod mod l inc r m a)
mergeInc cmp = memo2 $ \recur mxs mys -> force mxs >>= \xs -> force mys >>= \ys -> case (xs,ys) of
	(ConsMod x mxs',ConsMod y mys') -> if cmp x y == LT
		then liftM (ConsMod x) $ recur mxs' mys
		else liftM (ConsMod y) $ recur mxs mys'
	(mxs',NilMod) -> return mxs'
	(NilMod,mys') -> return mys'

-- | insertion sort (slower IC...)
isortInc :: (Eq a,Eq (ListMod mod l inc r m a),Memo a,Memo (mod l inc r m (ListMod' mod l inc r m a)),Output mod l inc r m) => (a -> a -> Ordering) -> ListMod mod l inc r m a -> l inc r m (ListMod mod l inc r m a)
isortInc cmp = memo $ \recur mxs -> force mxs >>= \xs -> case xs of
	ConsMod x' mxs' -> recur mxs' >>= insertInc cmp x' >>= force
	NilMod -> return NilMod

-- | insert an element into a sorted list
insertInc :: (Eq a,Eq (ListMod mod l inc r m a),Memo a,Memo (mod l inc r m (ListMod' mod l inc r m a)),Output mod l inc r m) => (a -> a -> Ordering) -> a -> ListMod mod l inc r m a -> l inc r m (ListMod mod l inc r m a)
insertInc cmp = memo2 $ \recur x mys -> force mys >>= \ys -> case ys of
	ConsMod y mys' -> if cmp x y == LT
		then return $ ConsMod x mys
		else liftM (ConsMod y) $ recur x mys'
	NilMod -> liftM (ConsMod x) $ const NilMod

-- | looks up an element in a list of key-value pairs and removes that element from the list
--lookupAndRemove :: (Eq v,Eq (ListMod mod l inc r m (k, v)),Memo k,Memo (ListMod mod l inc r m (k, v)),Eq k,Output mod l inc r m) => k -> ListMod mod l inc r m (k,v) -> l inc r m (mod l inc r m (Maybe (k,v),ListMod mod l inc r m (k,v)))
--lookupAndRemove = memo2 $ \recur k mxs -> force mxs >>= \xs -> case xs of
--	ConsMod x@(kx,_) mxs' -> if k == kx
--		then return (Just x,mxs')
--		else do
--			t <- recur k mxs'
--			x' <- liftM fst $ force t
--			mxs'' <- thunk $ liftM (ConsMod x) $ liftM snd $ force t
--			return (x',mxs'')
--	NilMod -> return (Nothing,mxs)

--mergeMapInc :: (MonadIO m,vEq v,Eq (ListMod mod l inc r m (k, v)),Memo k,Memo (ListMod mod l inc r m (k, v)),Eq k,Output mod l inc r m) => ((k,v) -> (k,v) -> Ordering) -> (v -> v -> l inc r m v) -> ListMod mod l inc r m (k,v) -> ListMod mod l inc r m (k,v) -> l inc r m (ListMod mod l inc r m (k,v))
--mergeMapInc cmp mrg = memo2 $ \recur mxs mys -> force mxs >>= \xs -> force mys >>= \ys -> case (xs,ys) of
--	(ConsMod x@(kx,vx) mxs',ConsMod y@(ky,vy) mys') -> do
--		mby <- findInc ((==kx) . fst) mys >>= force
--		case mby of
--			Nothing -> if cmp x y == LT
--				then liftM (ConsMod x) $ recur mxs' mys
--				else do
--					mbx <- findInc ((==ky) . fst) mxs' >>= force
--					case mbx of
--						Nothing -> liftM (ConsMod y) $ recur mxs' mys'
--						Just (_,vx') -> do
--							mxs'' <- filterInc ((/=ky) . fst) mxs
--							vxy' <- mrg vx' vy
--							liftM (ConsMod (ky,vxy')) $ recur mxs'' mys'
--			Just (_,vy) -> do
--				mys'' <- filterInc ((/=kx) . fst) mys'
--				vxy <- mrg vx vy
--				liftM (ConsMod (kx,vxy)) $ recur mxs' mys''
--	(NilMod,mys') -> return mys'
--	(mxs',NilMod) -> return mxs'

-- | Merges two sorted key-value lists into a sorted list, using a merging operation on pairs with the same key
mergeMapInc :: (Eq (ListMod mod l inc r m (ListMod mod l inc r m (k, v))),Hashable (ListMod mod l inc r m (ListMod mod l inc r m (k, v))),Memo k,Memo v,Memo (ListMod mod l inc r m (ListMod mod l inc r m (k, v))),MonadIO m,Eq v,Eq (ListMod mod l inc r m (k,v)),Memo (ListMod mod l inc r m (k, v)),Eq k,Output mod l inc r m) => ((k,v) -> (k,v) -> Ordering) -> (v -> v -> l inc r m v) -> ListMod mod l inc r m (k,v) -> ListMod mod l inc r m (k,v) -> l inc r m (ListMod mod l inc r m (k,v))
mergeMapInc cmp mrg mxs mys = do
	let samekey (kx,vx) (ky,vy) = kx == ky
	let mrg' (kx,vx) (ky,vy) = liftM (kx,) $ mrg vx vy
	mxys' <- intersectionByInc samekey mrg' mxs mys >>= quicksortInc cmp
	mxs' <- differenceByInc samekey mxs mxys'
	mys' <- differenceByInc samekey mys mxys'
	mergeInc cmp mxys' mxs' >>= mergeInc cmp mys'

-- | merge two sorted key-value lists with a merging function for values with the same key
--mergeMapInc2 :: (Memo k,Memo v,Memo (ListMod mod l inc r m (k,v)),Eq k,Eq v,Output mod l inc r m,Eq (ListMod mod l inc r m (k,v)))
--	=> ((k,v) -> (k,v) -> Ordering) -> (v -> v -> l inc r m v) -> ListMod mod l inc r m (k,v) -> ListMod mod l inc r m (k,v) -> l inc r m (ListMod mod l inc r m (k,v))
--mergeMapInc2 cmp mrg = memo2 $ \recur mxs mys -> force mxs >>= \xs -> case xs of
--	ConsMod x@(k,vx) mxs' -> do
--		(mb,mys') <- lookupAndRemove k mys >>= force
--		x' <- liftM (k,) $ maybe (return vx) (\y@(_,vy) -> mrg vx vy) mb
--		insertInc cmp x' mys' >>= recur mxs' >>= force
--	NilMod -> force mys

-- | finds the first element in a list that satisfies a predicate
findInc :: (Eq a,Output mod l inc r m,Eq (ListMod mod l inc r m a),Memo (ListMod mod l inc r m a)) => (a -> Bool) -> ListMod mod l inc r m a -> l inc r m (mod l inc r m (Maybe a))
findInc p = memo $ \recur mxs -> force mxs >>= \xs -> case xs of
	ConsMod x mxs' -> if p x
		then return $ Just x
		else recur mxs' >>= force
	NilMod -> return Nothing

-- | list intersection with a merge operation for similar entries
intersectionByInc :: (Eq a,Output mod l inc r m,Eq (ListMod mod l inc r m a),Memo (ListMod mod l inc r m a)) => (a -> a -> Bool) -> (a -> a -> l inc r m a) -> ListMod mod l inc r m a -> ListMod mod l inc r m a -> l inc r m (ListMod mod l inc r m a)
intersectionByInc cmp mrg mxs mys = memo (\recur mxs -> force mxs >>= \xs -> case xs of
	ConsMod x mxs' -> do
		mb <- findInc (cmp x) mys >>= force
		case mb of
			Just y -> mrg x y >>= \xy -> liftM (ConsMod xy) $ recur mxs'
			Nothing -> recur mxs' >>= force
	NilMod -> return NilMod) mxs

-- | list difference
differenceByInc :: (Eq a,Output mod l inc r m,Eq (ListMod mod l inc r m a),Memo (ListMod mod l inc r m a)) => (a -> a -> Bool) -> ListMod mod l inc r m a -> ListMod mod l inc r m a -> l inc r m (ListMod mod l inc r m a)
differenceByInc cmp mxs mys = memo (\recur mxs -> force mxs >>= \xs -> case xs of
	ConsMod x mxs' -> do
		mb <- findInc (cmp x) mys >>= force
		case mb of
			Just y -> recur mxs' >>= force
			Nothing -> liftM (ConsMod x) $ recur mxs'
	NilMod -> return NilMod) mxs

-- * joins lists with constant concatenation

data JoinListMod'
	(mod :: ((* -> (* -> *) -> (* -> *) -> * -> *) -> * -> (* -> *) -> (* -> *) -> * -> *))
	(l :: * -> (* -> *) -> (* -> *) -> * -> *)
	inc
	(r :: * -> *)
	(m :: * -> *)
	a
	= EmptyMod | SingleMod a | JoinMod (JoinListMod mod l inc r m a) (JoinListMod mod l inc r m a) 

type JoinListMod mod l inc r m a = mod l inc r m (JoinListMod' mod l inc r m a)

deriving instance Typeable JoinListMod'
deriving instance (Eq a,Eq (JoinListMod mod l inc r m a)) => Eq (JoinListMod' mod l inc r m a)

instance (DeepTypeable mod,DeepTypeable l,DeepTypeable inc,DeepTypeable r,DeepTypeable m,DeepTypeable a,DeepTypeable (JoinListMod mod l inc r m a)) => DeepTypeable (JoinListMod' mod l inc r m a) where
	typeTree (_::Proxy (JoinListMod' mod l inc r m a)) = MkTypeTree (mkName "Control.Monad.Incremental.List.JoinListMod'") args [MkConTree (mkName "Control.Monad.Incremental.List.EmptyMod") [],MkConTree (mkName "Control.Monad.Incremental.List.SingleMod") [typeTree (Proxy::Proxy a)],MkConTree (mkName "Control.Monad.Incremental.List.JoinMod") [typeTree (Proxy::Proxy (JoinListMod mod l inc r m a))]]
		where args = [typeTree (Proxy::Proxy mod),typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy r),typeTree (Proxy::Proxy m),typeTree (Proxy::Proxy a)]

type JoinListU l r m a = JoinListMod U l Adapton r m a
type JoinListU' l r m a = JoinListMod' U l Adapton r m a

type JoinListM l r m a = JoinListMod M l Adapton r m a
type JoinListM' l r m a = JoinListMod' M l Adapton r m a

type JoinListL l r m a = JoinListMod L l Adapton r m a
type JoinListL' l r m a = JoinListMod' L l Adapton r m a

type JoinListLazyNonIncU l r m a = JoinListMod LazyNonIncU l LazyNonInc r m a
type JoinListLazyNonIncU' l r m a = JoinListMod' LazyNonIncU l LazyNonInc r m a

type JoinListLazyNonIncL l r m a = JoinListMod LazyNonIncL l LazyNonInc r m a
type JoinListLazyNonIncL' l r m a = JoinListMod' LazyNonIncL l LazyNonInc r m a

type JoinListLazyNonIncM l r m a = JoinListMod LazyNonIncM l LazyNonInc r m a
type JoinListLazyNonIncM' l r m a = JoinListMod' LazyNonIncM l LazyNonInc r m a

instance (Display l1 inc r m a,Display l1 inc r m (JoinListMod mod l inc r m a)) => Display l1 inc r m (JoinListMod' mod l inc r m a) where
	displaysPrec EmptyMod r = return $ "EmptyMod" ++ r
	displaysPrec (SingleMod x) rest = do
		sx <- displaysPrec x (')':rest)
		return $ "(SingleMod " ++ sx
	displaysPrec (JoinMod mxs mys) rest = do
		sys <- displaysPrec mys (')':rest)
		sxs <- displaysPrec mxs (' ':sys)
		return $ "(JoinMod " ++ sxs

instance (NFDataInc l1 inc r m a,NFDataInc l1 inc r m (JoinListMod mod l inc r m a)) => NFDataInc l1 inc r m (JoinListMod' mod l inc r m a) where
	rnfInc EmptyMod = return $! ()
	rnfInc (SingleMod x) = rnfInc x
	rnfInc (JoinMod x y) = do
		a <- rnfInc x
		b <- rnfInc y
		return $! a `seq` b

instance (DeepTypeable mod,DeepTypeable inc,DeepTypeable r,DeepTypeable m,DeepTypeable l,Sat (ctx (JoinListMod' mod l inc r m a)),MData ctx (l1 inc r m) a,MData ctx (l1 inc r m) (JoinListMod mod l inc r m a))
			=> MData ctx (l1 inc r m) (JoinListMod' mod l inc r m a) where
      gfoldl ctx k z EmptyMod = z EmptyMod
      gfoldl ctx k z (SingleMod x) = z (liftM SingleMod) >>= flip k (return x)
      gfoldl ctx k z (JoinMod x1 x2) = (((z (\mx1 -> return (\mx2 -> mx1 >>= \x1 -> mx2 >>= \x2 -> return (JoinMod x1 x2)))) >>= (flip k $ return x1)) >>= (flip k $ return x2))
      gunfold ctx k z c = case constrIndex c of
		1 -> z EmptyMod
		2 -> z (liftM SingleMod) >>= k
		3 -> ((z (\mx1 -> return (\mx2 -> mx1 >>= \x1 -> mx2 >>= \x2 -> return (JoinMod x1 x2)))) >>= k) >>= k
      toConstr ctx x@EmptyMod = ((dataTypeOf ctx x) >>= (return . (flip indexConstr 1)))
      toConstr ctx x@(SingleMod x1) = ((dataTypeOf ctx x) >>= (return . (flip indexConstr 2)))
      toConstr ctx x@(JoinMod x1 x2) = ((dataTypeOf ctx x) >>= (return . (flip indexConstr 3)))
      dataTypeOf ctx x = return ty where
            ty = mkDataType "Todo.JoinListMod'" [mkConstr ty "EmptyMod" [] Prefix,mkConstr ty "SingleMod" [] Prefix,mkConstr ty "JoinMod" [] Prefix]

joinListInc :: (Eq a,Eq (JoinListMod mod l inc r m a),Output mod l inc r m) => JoinListMod mod l inc r m a -> JoinListMod mod l inc r m a -> l inc r m (JoinListMod mod l inc r m a)
joinListInc mxs mys = thunk $ return $ JoinMod mxs mys

joinListInc' :: (Eq a,Eq (JoinListMod mod l inc r m a),Output mod l inc r m) => JoinListMod' mod l inc r m a -> JoinListMod' mod l inc r m a -> l inc r m (JoinListMod' mod l inc r m a)
joinListInc' mxs mys = do
	txs <- thunk $ return mxs
	tys <- thunk $ return mys
	return $ JoinMod txs tys

-- | a self-pruning joinlist concatenation operation
joinListPruneInc :: (Eq a,Eq (JoinListMod mod l inc r m a),Output mod l inc r m) => JoinListMod mod l inc r m a -> JoinListMod mod l inc r m a -> l inc r m (JoinListMod mod l inc r m a)
joinListPruneInc tx ty = thunk $ do
	x <- force tx
	isEmptyJoinListMod' x >>= \b -> if b
		then force ty
		else do
			y <- force ty
			isEmptyJoinListMod' y >>= \b -> if b
				then return x
				else return $ JoinMod tx ty

mapJoinListInc :: (Thunk mod l inc r m,Eq b,Eq (JoinListMod thunk l inc r m b),MonadIO m,Memo (JoinListMod mod l inc r m a),Eq a,Eq (JoinListMod mod l inc r m a),Output thunk l inc r m)
	=> (a -> l inc r m b) -> JoinListMod mod l inc r m a -> l inc r m (JoinListMod thunk l inc r m b)
mapJoinListInc f = memo $ \recur mxs -> read mxs >>= \xs -> case xs of
	EmptyMod -> return EmptyMod
	SingleMod x -> liftM SingleMod $ f x
	JoinMod mxs1 mxs2 -> do
		mys1 <- recur mxs1
		mys2 <- recur mxs2
		return $ JoinMod mys1 mys2

-- | deep traversal that tests whether a joinlist is empty
isEmptyJoinListMod' :: (Eq a,Output mod l inc r m,Eq (JoinListMod mod l inc r m a)) => JoinListMod' mod l inc r m a -> l inc r m Bool
isEmptyJoinListMod' EmptyMod = return True
isEmptyJoinListMod' (SingleMod _) = return False
isEmptyJoinListMod' (JoinMod l r) = do
	lb <- force l >>= isEmptyJoinListMod'
	if lb
		then force r >>= isEmptyJoinListMod'
		else return False

-- | Random number generator
data Seeds = Seeds !Int Seeds -- infinite list
instance Show Seeds where
	show (Seeds i _) = "(Seeds "++show i++" _)"
instance Eq Seeds where
	(Seeds h1 _) == (Seeds h2 _) = h1 == h2
makeSeeds :: StdGen -> Seeds
makeSeeds g = let (i,g') = next g in Seeds i (makeSeeds g')
popSeed :: Seeds -> (Int,Seeds)
popSeed (Seeds i s) = s `seq` (i,s)
		
instance Memo Seeds where
	type Key Seeds = Int
	memoKey (Seeds i _) = (MkWeak $ Weak.mkWeak i,i)
		
instance Memo (ListMod' mod l inc r m a) where
	type Key (ListMod' mod l inc r m a) = StableName (ListMod' mod l inc r m a)
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ Weak.mkWeak x,unsafePerformIO $ makeStableName x)

instance Memo (JoinListMod' mod l inc r m a) where
	type Key (JoinListMod' mod l inc r m a) = StableName (JoinListMod' mod l inc r m a)
	{-# INLINE memoKey #-}
	memoKey x = (MkWeak $ Weak.mkWeak x,unsafePerformIO $ makeStableName x)

