{-# LANGUAGE Rank2Types, GADTs, ConstraintKinds, BangPatterns, TupleSections, TypeFamilies, ScopedTypeVariables, TemplateHaskell, StandaloneDeriving, KindSignatures, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances, DeriveDataTypeable #-}

module Control.Monad.Incremental.List where

import System.Random
import Data.WithClass.MData
import Control.Monad.Incremental
import Data.Proxy
import Control.Monad.Incremental.Display
import Control.Monad.Incremental.LazyNonInc
import Control.Monad.Incremental.Adapton hiding (MData)
import Control.Concurrent.Transactional.TxAdapton
import Data.IORef

import Prelude hiding (mod,const,read,mapM_)
import qualified Prelude
import Data.WithClass.MGenerics.Instances

import Data.DeriveTH                 -- Library for deriving instances for existing types
import Data.DeepTypeable
import Data.WithClass.Derive.DeepTypeable
import Language.Haskell.TH.Syntax hiding (lift,Infix,Fixity)

import Control.Monad hiding (mapM_)
import Control.Monad.IO.Class
import System.Mem.StableName.Exts
import System.IO.Unsafe

import System.Mem.Weak.Exts as Weak

import Data.Memo
import Data.Derive.Memo

-- * standard lists

data ListMod'
	(mod :: ((* -> * -> *) -> * -> * -> *))
	(l :: * -> * -> *)
	inc
	a
	= NilMod | ConsMod a (ListMod mod l inc a)
	
deriving instance (Show a,Show (ListMod mod l inc a)) => Show (ListMod' mod l inc a)

type ListMod mod l inc a = mod l inc (ListMod' mod l inc a)

deriving instance Typeable ListMod'
deriving instance (Eq a,Eq (ListMod mod l inc a)) => Eq (ListMod' mod l inc a)

instance (DeepTypeable mod,DeepTypeable l,DeepTypeable inc,DeepTypeable a,DeepTypeable (ListMod mod l inc a)) => DeepTypeable (ListMod' mod l inc a) where
	typeTree (_::Proxy (ListMod' mod l inc a)) = MkTypeTree (mkName "Control.Monad.Incremental.List.ListMod'") args [MkConTree (mkName "Control.Monad.Incremental.List.NilMod") [],MkConTree (mkName "Control.Monad.Incremental.List.ConsMod") [typeTree (Proxy::Proxy a),typeTree (Proxy::Proxy (ListMod mod l inc a))]]
		where args = [typeTree (Proxy::Proxy mod),typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy a)]

type ListU l a = ListMod U l Adapton a
type ListU' l a = ListMod' U l Adapton a

type ListM l a = ListMod M l Adapton a
type ListM' l a = ListMod' M l Adapton a

type ListL l a = ListMod L l Adapton a
type ListL' l a = ListMod' L l Adapton a

type ListLazyNonIncU l a = ListMod LazyNonIncU l LazyNonInc a
type ListLazyNonIncU' l a = ListMod' LazyNonIncU l LazyNonInc a

type ListLazyNonIncL l a = ListMod LazyNonIncL l LazyNonInc a
type ListLazyNonIncL' l a = ListMod' LazyNonIncL l LazyNonInc a

type ListLazyNonIncM l a = ListMod LazyNonIncM l LazyNonInc a
type ListLazyNonIncM' l a = ListMod' LazyNonIncM l LazyNonInc a

type ListTxU c l a = ListMod (TxU c) l (TxAdapton c) a
type ListTxU' c l a = ListMod' (TxU c) l (TxAdapton c) a

type ListTxM i c l a = ListMod (TxM i c) l (TxAdapton c) a
type ListTxM' i c l a = ListMod' (TxM i c) l (TxAdapton c) a


instance (Display l1 inc a,Display l1 inc (ListMod mod l inc a)) => Display l1 inc (ListMod' mod l inc a) where
	displaysPrec proxyL proxyInc NilMod r = return $ "NilMod" ++ r
	displaysPrec proxyL proxyInc (ConsMod x mxs) rest = do
		sxs <- displaysPrec proxyL proxyInc mxs (')':rest)
		sx <- displaysPrec proxyL proxyInc x (' ':sxs)
		return $ "(ConsMod " ++ sx

instance (NFDataInc l1 inc a,NFDataInc l1 inc (ListMod mod l inc a)) => NFDataInc l1 inc (ListMod' mod l inc a) where
	nfDataInc proxyL proxyInc NilMod = return $! ()
	nfDataInc proxyL proxyInc (ConsMod x mxs) = do
		a <- nfDataInc proxyL proxyInc x
		b <- nfDataInc proxyL proxyInc mxs
		return $! a `seq` b

instance (DeepTypeable mod,DeepTypeable inc,DeepTypeable l,Sat (ctx (ListMod' mod l inc a)),MData ctx (l1 inc) a,MData ctx (l1 inc) (ListMod mod l inc a))
			=> MData ctx (l1 inc) (ListMod' mod l inc a) where
      gfoldl ctx k z NilMod = z NilMod
      gfoldl ctx k z (ConsMod x1 x2) = (((z (\mx1 -> return (\mx2 -> mx1 >>= \x1 -> mx2 >>= \x2 -> return (ConsMod x1 x2)))) >>= (flip k $ return x1)) >>= (flip k $ return x2))
      gunfold ctx k z c = case constrIndex c of
            1 -> z NilMod
            2 -> (((z (\mx1 -> return (\mx2 -> mx1 >>= \x1 -> mx2 >>= \x2 -> return (ConsMod x1 x2)))) >>= k) >>= k)
      toConstr ctx x@NilMod = ((dataTypeOf ctx x) >>= (return . (flip indexConstr 1)))
      toConstr ctx x@(ConsMod x1 x2) = ((dataTypeOf ctx x) >>= (return . (flip indexConstr 2)))
      dataTypeOf ctx x = return ty where
            ty = mkDataType "Todo.ListMod'" [mkConstr ty "NilMod" [] Prefix,mkConstr ty "ConsMod" [] Prefix]

headMayInc :: (IncK inc (ListMod' mod l inc a),Thunk mod l inc) => ListMod mod l inc a -> l inc (Maybe a)
headMayInc mxs = read mxs >>= \xs -> case xs of
	NilMod -> return Nothing
	ConsMod x mxs' -> return $ Just x

-- a simpler version where input and output thunks are the same
takeInc' :: (IncK inc (ListMod' thunk l inc a),Memo (ListMod thunk l inc a),Output thunk l inc) => Int -> ListMod thunk l inc a -> l inc (ListMod thunk l inc a)
takeInc' = takeInc

takeIncAs' :: (Memo name,IncK inc (ListMod' thunk l inc a),Memo (ListMod thunk l inc a),Output thunk l inc) => name -> Int -> ListMod thunk l inc a -> l inc (ListMod thunk l inc a)
takeIncAs' = takeIncAs

-- | take
takeInc :: (IncK inc (ListMod' thunk l inc a),IncK inc (ListMod' mod l inc a),Memo (ListMod mod l inc a),Output thunk l inc,Thunk mod l inc) => Int -> ListMod mod l inc a -> l inc (ListMod thunk l inc a)
takeInc = memo2 $ \recur i mxs -> case i of
	0 -> return NilMod
	n -> read mxs >>= \xs -> case xs of
		ConsMod x mxs' -> liftM (ConsMod x) $ recur (n-1) mxs'
		NilMod -> return NilMod

takeIncAs :: (Memo name,IncK inc (ListMod' thunk l inc a),IncK inc (ListMod' mod l inc a),Memo (ListMod mod l inc a),Output thunk l inc,Thunk mod l inc) => name -> Int -> ListMod mod l inc a -> l inc (ListMod thunk l inc a)
takeIncAs name = memo2As name $ \recur i mxs -> case i of
	0 -> return NilMod
	n -> read mxs >>= \xs -> case xs of
		ConsMod x mxs' -> liftM (ConsMod x) $ recur (n-1) mxs'
		NilMod -> return NilMod

filterIncAs :: (Memo name,IncK inc (ListMod' thunk l inc a),IncK inc (ListMod' mod l inc a),Thunk mod l inc,Memo (ListMod mod l inc a),Output thunk l inc)
	=> name -> (a -> l inc Bool) -> ListMod mod l inc a -> l inc (ListMod thunk l inc a)
filterIncAs name p = memoAs name $ \recur mxs -> read mxs >>= \xs -> case xs of
	ConsMod x mxs -> do
		b <- p x
		if b
			then liftM (ConsMod x) $ recur mxs
			else recur mxs >>= force
	NilMod -> return NilMod

-- | filter
filterInc :: (IncK inc (ListMod' thunk l inc a),IncK inc (ListMod' mod l inc a),Thunk mod l inc,Memo (ListMod mod l inc a),Output thunk l inc)
	=> (a -> l inc Bool) -> ListMod mod l inc a -> l inc (ListMod thunk l inc a)
filterInc p = memo $ \recur mxs -> read mxs >>= \xs -> case xs of
	ConsMod x mxs -> p x >>= \b -> if b
		then liftM (ConsMod x) $ recur mxs
		else recur mxs >>= force
	NilMod -> return NilMod
	
-- | filter=
filterWithKeyInc :: (IncK inc (ListMod' thunk l inc a),IncK inc (ListMod' mod l inc a),Memo a,Thunk mod l inc,Memo (ListMod mod l inc a),Output thunk l inc)
	=> (a -> a -> l inc Bool) -> a -> ListMod mod l inc a -> l inc (ListMod thunk l inc a)
filterWithKeyInc cmp = memo2 $ \recur k mxs -> read mxs >>= \xs -> case xs of
	ConsMod x mxs -> cmp k x >>= \b -> if b
		then liftM (ConsMod x) $ recur k mxs
		else recur k mxs >>= force
	NilMod -> return NilMod

filterWithKeyIncAs :: (Memo name,IncK inc (ListMod' thunk l inc a),IncK inc (ListMod' mod l inc a),Memo a,Thunk mod l inc,Memo (ListMod mod l inc a),Output thunk l inc)
	=> name -> (a -> a -> l inc Bool) -> a -> ListMod mod l inc a -> l inc (ListMod thunk l inc a)
filterWithKeyIncAs name cmp = memo2As name $ \recur k mxs -> read mxs >>= \xs -> case xs of
	ConsMod x mxs -> cmp k x >>= \b -> if b
		then liftM (ConsMod x) $ recur k mxs
		else recur k mxs >>= force
	NilMod -> return NilMod

-- | length
lengthInc :: (IncK inc Int,IncK inc (ListMod' mod l inc a),Thunk mod l inc,Memo (ListMod mod l inc a),Layer l inc,Output thunk l inc) => ListMod mod l inc a -> l inc (thunk l inc Int)
lengthInc = memo $ \recur mxs -> read mxs >>= \xs -> case xs of
	ConsMod x mxs' -> recur mxs' >>= force >>= return . succ
	NilMod -> return 0

-- | list map
mapInc :: (IncK inc (ListMod' thunk l inc b),IncK inc (ListMod' mod l inc a),Thunk mod l inc,Memo (ListMod mod l inc a),Output thunk l inc)
	=> (a -> l inc b) -> ListMod mod l inc a -> l inc (ListMod thunk l inc b)
mapInc f = memo $ \recur mxs -> read mxs >>= \xs -> case xs of
	NilMod -> return NilMod
	ConsMod x mxs -> do
		y <- f x
		mys <- recur mxs
		return $ ConsMod y mys

mapIncAs :: (Memo name,IncK inc (ListMod' thunk l inc b),IncK inc (ListMod' mod l inc a),Thunk mod l inc,Memo (ListMod mod l inc a),Output thunk l inc)
	=> name -> (a -> l inc b) -> ListMod mod l inc a -> l inc (ListMod thunk l inc b)
mapIncAs name f = memoAs name $ \recur mxs -> read mxs >>= \xs -> case xs of
	NilMod -> return NilMod
	ConsMod x mxs -> do
		y <- f x
		mys <- recur mxs
		return $ ConsMod y mys

-- | fold1 with an associative operation
fold1Inc :: (IncK inc a,IncK inc (ListMod' mod l inc a),Memo (ListMod mod l inc a),Hashable (ListMod mod l inc a),Output mod l inc)
	=> (a -> a -> l inc a) -> ListMod mod l inc a -> l inc (mod l inc a)
fold1Inc f mxs = do
	g <- unsafeIOToInc newStdGen
	let seeds = makeSeeds g
	memo2 (\recur seeds mxs -> force mxs >>= \xs -> case xs of
		ConsMod x' mxs' -> force mxs' >>= \xs' -> case xs' of
			ConsMod _ _ -> do
				let (seed,seeds') = popSeed seeds
				fold_pairs f seed mxs >>= recur seeds' >>= force
			NilMod -> return x'
		NilMod -> error "fold1 requires a non-empty list") seeds mxs
  where
	fold_pairs :: (IncK inc (ListMod' mod l inc a),Memo (mod l inc (ListMod' mod l inc a)),Output mod l inc,Hashable (ListMod mod l inc a)) => (a -> a -> l inc a) -> Int -> ListMod mod l inc a -> l inc (ListMod mod l inc a)
	fold_pairs f = memo2 (\recur seed mxs -> force mxs >>= \xs -> case xs of
		ConsMod x' mxs' -> do
			if (hashWithSalt seed mxs `Prelude.mod` 2 == 0)
				then liftM (ConsMod x') $ recur seed mxs'
				else force mxs' >>= \xs' -> case xs' of
					ConsMod x'' mxs'' -> f x' x'' >>= \y -> liftM (ConsMod y) $ recur seed mxs''
					NilMod -> return xs
		NilMod -> return NilMod)

fold1IncAs' :: (IncK inc (ListMod' thunk l inc a),Thunk mod l inc,Hashable (ListMod thunk l inc a),Memo name,IncK inc a,IncK inc (ListMod' mod l inc a),Memo (ListMod mod l inc a),Hashable (ListMod mod l inc a),Output thunk l inc,Memo (ListMod thunk l inc a))
	=> name -> (a -> a -> l inc a) -> ListMod mod l inc a -> l inc (thunk l inc a)
fold1IncAs' name f = mapIncAs (name,()) return >=> fold1IncAs name f

fold1IncAs :: (Memo name,IncK inc a,IncK inc (ListMod' mod l inc a),Memo (ListMod mod l inc a),Hashable (ListMod mod l inc a),Output mod l inc)
	=> name -> (a -> a -> l inc a) -> ListMod mod l inc a -> l inc (mod l inc a)
fold1IncAs name f mxs = do
	g <- unsafeIOToInc newStdGen
	let seeds = makeSeeds g
	memo2As name (\recur seeds mxs -> force mxs >>= \xs -> case xs of
		ConsMod x' mxs' -> force mxs' >>= \xs' -> case xs' of
			ConsMod _ _ -> do
				let (seed,seeds') = popSeed seeds
				fold_pairs (name,seed) f seed mxs >>= recur seeds' >>= force
			NilMod -> return x'
		NilMod -> error "fold1 requires a non-empty list") seeds mxs
  where
	fold_pairs :: (Memo name,IncK inc (ListMod' mod l inc a),Memo (mod l inc (ListMod' mod l inc a)),Output mod l inc,Hashable (ListMod mod l inc a)) => name -> (a -> a -> l inc a) -> Int -> ListMod mod l inc a -> l inc (ListMod mod l inc a)
	fold_pairs name f = memo2As name (\recur seed mxs -> force mxs >>= \xs -> case xs of
		ConsMod x' mxs' -> do
			if (hashWithSalt seed mxs `Prelude.mod` 2 == 0)
				then liftM (ConsMod x') $ recur seed mxs'
				else force mxs' >>= \xs' -> case xs' of
					ConsMod x'' mxs'' -> f x' x'' >>= \y -> liftM (ConsMod y) $ recur seed mxs''
					NilMod -> return xs
		NilMod -> return NilMod)

-- | partition
partitionInc :: (IncK
                        inc (ListMod mod l inc a, ListMod mod l inc a),IncK inc (ListMod' mod l inc a),Memo (ListMod mod l inc a),Output mod l inc)
	=> (a -> l inc Bool) -> ListMod mod l inc a -> l inc (mod l inc (ListMod mod l inc a,ListMod mod l inc a))
partitionInc p = memo $ \recur mxs -> force mxs >>= \xs -> case xs of
	ConsMod x mxs' -> do
		(left,right) <- recur mxs' >>= force
		p x >>= \b -> if b
			then const (ConsMod x left) >>= \left' -> return (left',right)
			else const (ConsMod x right) >>= \right' -> return (left,right')
	NilMod -> do
		nil <- const NilMod
		return (nil,nil)

quicksortIncAs :: (Memo name,IncK inc (ListMod' mod l inc a),Memo a,Output mod l inc,Memo (ListMod mod l inc a))
	=> name -> (a -> a -> l inc Ordering) -> ListMod mod l inc a -> l inc (ListMod mod l inc a)
quicksortIncAs name cmp (mxs :: ListMod mod l inc a) = do
	(nil :: ListMod mod l inc a) <- const NilMod
	let quicksortInc' = memo2As name $ \recur mxs rest -> force mxs >>= \xs -> case xs of
		ConsMod x mxs' -> do
			left <- filterWithKeyIncAs (name,True) (\k y -> liftM (==LT) $ cmp y k) x mxs'
			right <- filterWithKeyIncAs (name,False) (\k y -> liftM (/=LT) $ cmp y k) x mxs'
			recur right rest >>= const . ConsMod x >>= recur left >>= force
		NilMod -> force rest
	quicksortInc' mxs nil

quicksortIncAs' :: (IncK inc (ListMod' mod l inc a),Memo (ListMod mod l inc a),Thunk mod l inc,Memo name,IncK inc (ListMod' thunk l inc a),Memo a,Output thunk l inc,Memo (ListMod thunk l inc a))
	=> name -> (a -> a -> l inc Ordering) -> ListMod mod l inc a -> l inc (ListMod thunk l inc a)
quicksortIncAs' name cmp = mapIncAs (name,()) return >=> quicksortIncAs name cmp

quicksortInc :: (IncK inc (ListMod' mod l inc a),Memo a,Output mod l inc,Memo (ListMod mod l inc a))
	=> (a -> a -> l inc Ordering) -> ListMod mod l inc a -> l inc (ListMod mod l inc a)
quicksortInc cmp (mxs :: ListMod mod l inc a) = do
	(nil :: ListMod mod l inc a) <- const NilMod
	let quicksortInc' = memo2 $ \recur mxs rest -> force mxs >>= \xs -> case xs of
		ConsMod x mxs' -> do
			left <- filterInc (\y -> liftM (==LT) $ cmp y x) mxs'
			right <- filterInc (\y -> liftM (/=LT) $ cmp y x) mxs'
			recur right rest >>= const . ConsMod x >>= recur left >>= force
		NilMod -> force rest
	quicksortInc' mxs nil

-- | quicksort with partition (this IC version is actually slower than using two filters)
quicksortInc2 :: (IncK inc (ListMod' mod l inc a),IncK inc (ListMod mod l inc a, ListMod mod l inc a),Output mod l inc,Memo (ListMod mod l inc a))
	=> (a -> a -> l inc Ordering) -> ListMod mod l inc a -> l inc (ListMod mod l inc a)
quicksortInc2 cmp mxs = const NilMod >>= (quicksortInc2' cmp) mxs
  where
	quicksortInc2' :: (IncK inc (ListMod' mod l inc a),IncK inc (ListMod mod l inc a, ListMod mod l inc a),Output mod l inc,Memo (ListMod mod l inc a))
		=> (a -> a -> l inc Ordering) -> ListMod mod l inc a -> ListMod mod l inc a -> l inc (ListMod mod l inc a)
	quicksortInc2' cmp = memo2 (\recur mxs rest -> force mxs >>= \xs -> case xs of
		ConsMod x mxs' -> do
			(left,right) <- partitionInc (\y -> liftM (==LT) $ cmp y x) mxs' >>= force
			tright <- thunk $ liftM (ConsMod x) $ recur right rest
			recur left tright >>= force
		NilMod -> force rest)

quicksortInc3 :: (IncK inc (ListMod' mod l inc a),Output mod l inc,Memo (ListMod mod l inc a))
	=> (a -> a -> l inc Ordering) -> ListMod mod l inc a -> l inc (ListMod mod l inc a)
quicksortInc3 cmp (mxs :: ListMod mod l inc a) = do
	(nil :: ListMod mod l inc a) <- const NilMod
	let quicksortInc' = memo2 $ \recur mxs rest -> force mxs >>= \xs -> case xs of
		ConsMod x mxs' -> do
			left <- filterInc (\y -> liftM (==LT) $ cmp y x) mxs'
			right <- filterInc (\y -> liftM (/=LT) $ cmp y x) mxs'
			tright <- thunk $ liftM (ConsMod x) $ recur right rest
			recur left tright >>= force
		NilMod -> force rest
	quicksortInc' mxs nil

-- | mergesort
mergesortInc :: (IncK inc (ListMod' mod l inc (ListMod mod l inc a)),IncK inc (ListMod' mod l inc a),IncK inc (ListMod mod l inc a),Memo (ListMod mod l inc (ListMod mod l inc a)),Memo a,Memo (mod l inc (ListMod' mod l inc a)),Hashable (ListMod mod l inc (ListMod mod l inc a)),Output mod l inc)
	=> (a -> a -> l inc Ordering) -> ListMod mod l inc a -> l inc (ListMod mod l inc a)
mergesortInc cmp mxs = do
	nil <- const NilMod
	memo (\_ mxs -> force mxs >>= \xs -> case xs of
		ConsMod _ _ -> liftInc nil mxs >>= fold1Inc (mergeInc cmp) >>= force >>= force
		NilMod -> return NilMod) mxs
  where
	-- | appends a list to each element of a list
	liftInc :: (IncK inc (ListMod' mod l inc (ListMod mod l inc a)),IncK inc (ListMod' mod l inc a),Output mod l inc,Memo a,Memo (ListMod mod l inc a)) => ListMod mod l inc a -> ListMod mod l inc a -> l inc (ListMod mod l inc (ListMod mod l inc a))
	liftInc nil = mapInc (memo (\_ x -> return $ ConsMod x nil))

mergesortIncAs :: (Memo name,IncK inc (ListMod' mod l inc (ListMod mod l inc a)),IncK inc (ListMod' mod l inc a),IncK inc (ListMod mod l inc a),Memo (ListMod mod l inc (ListMod mod l inc a)),Memo a,Memo (mod l inc (ListMod' mod l inc a)),Hashable (ListMod mod l inc (ListMod mod l inc a)),Output mod l inc)
	=> name -> (a -> a -> l inc Ordering) -> ListMod mod l inc a -> l inc (ListMod mod l inc a)
mergesortIncAs name cmp mxs = do
	nil <- const NilMod
	memoAs name (\_ mxs -> force mxs >>= \xs -> case xs of
		ConsMod _ _ -> liftIncAs (name,nil) nil mxs >>= fold1IncAs (name,True) (mergeIncAs (name,False) cmp) >>= force >>= force
		NilMod -> return NilMod) mxs
  where
	-- | appends a list to each element of a list
	liftIncAs :: (Memo name,IncK inc (ListMod' mod l inc (ListMod mod l inc a)),IncK inc (ListMod' mod l inc a),Output mod l inc,Memo a,Memo (ListMod mod l inc a)) => name -> ListMod mod l inc a -> ListMod mod l inc a -> l inc (ListMod mod l inc (ListMod mod l inc a))
	liftIncAs name nil = mapIncAs name (memoAs (name,nil) (\_ x -> return $ ConsMod x nil))

mergesortIncAs' :: (IncK inc (ListMod' thunk l inc a),Output thunk l inc,IncK inc (ListMod' thunk l inc (ListMod thunk l inc a)),IncK inc (ListMod thunk l inc a),Hashable (ListMod thunk l inc (ListMod thunk l inc a)),Memo (thunk l inc (ListMod' thunk l inc a)),Memo (ListMod thunk l inc (ListMod thunk l inc a)),Memo name,IncK inc (ListMod' mod l inc (ListMod mod l inc a)),IncK inc (ListMod' mod l inc a),IncK inc (ListMod mod l inc a),Memo (ListMod mod l inc (ListMod mod l inc a)),Memo a,Memo (mod l inc (ListMod' mod l inc a)),Hashable (ListMod mod l inc (ListMod mod l inc a)),Thunk mod l inc)
	=> name -> (a -> a -> l inc Ordering) -> ListMod mod l inc a -> l inc (ListMod thunk l inc a)
mergesortIncAs' name cmp = mapIncAs (name,()) return >=> mergesortIncAs name cmp

-- | merges two sorted lists
mergeInc :: (IncK inc (ListMod' mod l inc a),Memo (mod l inc (ListMod' mod l inc a)),Output mod l inc)
	=> (a -> a -> l inc Ordering) -> ListMod mod l inc a -> ListMod mod l inc a -> l inc (ListMod mod l inc a)
mergeInc cmp = memo2 $ \recur mxs mys -> force mxs >>= \xs -> force mys >>= \ys -> case (xs,ys) of
	(ConsMod x mxs',ConsMod y mys') -> cmp x y >>= \b -> if b == LT
		then liftM (ConsMod x) $ recur mxs' mys
		else liftM (ConsMod y) $ recur mxs mys'
	(mxs',NilMod) -> return mxs'
	(NilMod,mys') -> return mys'
	
mergeIncAs :: (Memo name,IncK inc (ListMod' mod l inc a),Memo (mod l inc (ListMod' mod l inc a)),Output mod l inc)
	=> name -> (a -> a -> l inc Ordering) -> ListMod mod l inc a -> ListMod mod l inc a -> l inc (ListMod mod l inc a)
mergeIncAs name cmp = memo2As name $ \recur mxs mys -> force mxs >>= \xs -> force mys >>= \ys -> case (xs,ys) of
	(ConsMod x mxs',ConsMod y mys') -> cmp x y >>= \b -> if b == LT
		then liftM (ConsMod x) $ recur mxs' mys
		else liftM (ConsMod y) $ recur mxs mys'
	(mxs',NilMod) -> return mxs'
	(NilMod,mys') -> return mys'

mergeIncAs' :: (IncK inc (ListMod' thunk l inc a),Memo (thunk l inc (ListMod' thunk l inc a)),Memo name,IncK inc (ListMod' mod l inc a),Memo (mod l inc (ListMod' mod l inc a)),Output thunk l inc,Thunk mod l inc)
	=> name -> (a -> a -> l inc Ordering) -> ListMod mod l inc a -> ListMod mod l inc a -> l inc (ListMod thunk l inc a)
mergeIncAs' name cmp mxs mys = do
	mxs' <- mapIncAs (name,()) return mxs
	mys' <- mapIncAs (name,()) return mys
	mergeIncAs name cmp mxs' mys'

-- | insertion sort (slower IC...)
isortInc :: (IncK inc (ListMod' mod l inc a),Memo a,Memo (mod l inc (ListMod' mod l inc a)),Output mod l inc)
	=> (a -> a -> l inc Ordering) -> ListMod mod l inc a -> l inc (ListMod mod l inc a)
isortInc cmp = memo $ \recur mxs -> force mxs >>= \xs -> case xs of
	ConsMod x' mxs' -> recur mxs' >>= insertInc cmp x' >>= force
	NilMod -> return NilMod

-- | insert an element into a sorted list
insertInc :: (IncK inc (ListMod' mod l inc a),Memo a,Memo (mod l inc (ListMod' mod l inc a)),Output mod l inc)
	=> (a -> a -> l inc Ordering) -> a -> ListMod mod l inc a -> l inc (ListMod mod l inc a)
insertInc cmp = memo2 $ \recur x mys -> force mys >>= \ys -> case ys of
	ConsMod y mys' -> cmp x y >>= \b -> if b == LT
		then return $ ConsMod x mys
		else liftM (ConsMod y) $ recur x mys'
	NilMod -> liftM (ConsMod x) $ const NilMod

updown1IncAs :: (IncK inc (ListMod' mod l inc a),IncK inc Bool,Memo a,Output mod l inc,Ord a,Memo name,Memo (ListMod mod l inc a))
	=> name -> mod l inc Bool -> ListMod mod l inc a -> l inc (ListMod mod l inc a)
updown1IncAs name mb mxs = do
	let up = quicksortIncAs (name,True) (\x y -> return $ compare x y)
	let down = quicksortIncAs (name,False) (\x y -> return $ compare y x)
	thunk $ force mb >>= \b -> (if b then up mxs else down mxs) >>= force

updown2IncAs :: (IncK inc (ListMod' mod l inc a),IncK inc Bool,Memo a,Output mod l inc,Ord a,Memo name,Memo (ListMod mod l inc a))
	=> name -> mod l inc Bool -> ListMod mod l inc a -> l inc (ListMod mod l inc a)
updown2IncAs name mb mxs = do
	let up = quicksortIncAs (name,True) (\x y -> return $ compare x y) mxs
	let down = quicksortIncAs (name,False) (\x y -> return $ compare y x) mxs
	thunk $ force mb >>= \b -> (if b then up else down) >>= force

topkIncAs :: (IncK inc (ListMod' thunk l inc a),IncK inc (ListMod' mod l inc a),Memo name,Memo (ListMod mod l inc a),Output thunk l inc,Thunk mod l inc,Memo a,Memo (ListMod thunk l inc a),Ord a) => name -> Int -> ListMod mod l inc a -> l inc (ListMod thunk l inc a)
topkIncAs name k = mapIncAs (name,True) return >=> quicksortIncAs name (\x y -> return $ compare y x) >=> takeIncAs' (name,False) k

topkIncAs' :: (IncK inc (ListMod' thunk l inc a),IncK inc (ListMod' mod l inc a),Memo name,Memo (ListMod mod l inc a),Output thunk l inc,Thunk mod l inc,Memo a,Memo (ListMod thunk l inc a)) => name -> (a -> a -> l inc Ordering) -> Int -> ListMod mod l inc a -> l inc (ListMod thunk l inc a)
topkIncAs' name cmp k = mapIncAs (name,True) return >=> quicksortIncAs name (\x y -> cmp y x) >=> takeIncAs' (name,False) k

leastkIncAs' :: (IncK inc (ListMod' thunk l inc a),IncK inc (ListMod' mod l inc a),Memo name,Memo (ListMod mod l inc a),Output thunk l inc,Thunk mod l inc,Memo a,Memo (ListMod thunk l inc a)) => name -> (a -> a -> l inc Ordering) -> Int -> ListMod mod l inc a -> l inc (ListMod thunk l inc a)
leastkIncAs' name cmp k = mapIncAs (name,True) return >=> quicksortIncAs name cmp >=> takeIncAs' (name,False) k

-- | looks up an element in a list of key-value pairs and removes that element from the list
--lookupAndRemove :: (Eq v,Eq (ListMod mod l inc (k, v)),Memo k,Memo (ListMod mod l inc (k, v)),Eq k,Output mod l inc) => k -> ListMod mod l inc (k,v) -> l inc (mod l inc (Maybe (k,v),ListMod mod l inc (k,v)))
--lookupAndRemove = memo2 $ \recur k mxs -> force mxs >>= \xs -> case xs of
--	ConsMod x@(kx,_) mxs' -> if k == kx
--		then return (Just x,mxs')
--		else do
--			t <- recur k mxs'
--			x' <- liftM fst $ force t
--			mxs'' <- thunk $ liftM (ConsMod x) $ liftM snd $ force t
--			return (x',mxs'')
--	NilMod -> return (Nothing,mxs)

--mergeMapInc :: (Eq v,Eq (ListMod mod l inc (k, v)),Memo k,Memo (ListMod mod l inc (k, v)),Eq k,Output mod l inc) => ((k,v) -> (k,v) -> Ordering) -> (v -> v -> l inc v) -> ListMod mod l inc (k,v) -> ListMod mod l inc (k,v) -> l inc (ListMod mod l inc (k,v))
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
mergeMapInc :: (IncK inc (ListMod' mod l inc (k, v)),IncK inc (Maybe (k, v)),Typeable k,Typeable v,Eq (ListMod mod l inc (ListMod mod l inc (k, v))),Hashable (ListMod mod l inc (ListMod mod l inc (k, v))),Memo k,Memo v,Memo (ListMod mod l inc (ListMod mod l inc (k, v))),Eq v,Eq (ListMod mod l inc (k,v)),Memo (ListMod mod l inc (k, v)),Eq k,Output mod l inc) => ((k,v) -> (k,v) -> Ordering) -> (v -> v -> l inc v) -> ListMod mod l inc (k,v) -> ListMod mod l inc (k,v) -> l inc (ListMod mod l inc (k,v))
mergeMapInc cmp mrg mxs mys = do
	let samekey (kx,vx) (ky,vy) = kx == ky
	let mrg' (kx,vx) (ky,vy) = liftM (kx,) $ mrg vx vy
	mxys' <- intersectionByInc samekey mrg' mxs mys >>= quicksortInc (\x y -> return $ cmp x y)
	mxs' <- differenceByInc samekey mxs mxys'
	mys' <- differenceByInc samekey mys mxys'
	mergeInc (\x y -> return $ cmp x y) mxys' mxs' >>= mergeInc (\x y -> return $ cmp x y) mys'

-- | merge two sorted key-value lists with a merging function for values with the same key
--mergeMapInc2 :: (Memo k,Memo v,Memo (ListMod mod l inc (k,v)),Eq k,Eq v,Output mod l inc,Eq (ListMod mod l inc (k,v)))
--	=> ((k,v) -> (k,v) -> Ordering) -> (v -> v -> l inc v) -> ListMod mod l inc (k,v) -> ListMod mod l inc (k,v) -> l inc (ListMod mod l inc (k,v))
--mergeMapInc2 cmp mrg = memo2 $ \recur mxs mys -> force mxs >>= \xs -> case xs of
--	ConsMod x@(k,vx) mxs' -> do
--		(mb,mys') <- lookupAndRemove k mys >>= force
--		x' <- liftM (k,) $ maybe (return vx) (\y@(_,vy) -> mrg vx vy) mb
--		insertInc cmp x' mys' >>= recur mxs' >>= force
--	NilMod -> force mys

-- | finds the first element in a list that satisfies a predicate
findInc :: (IncK inc (Maybe a),IncK inc (ListMod' mod l inc a),Output mod l inc,Memo (ListMod mod l inc a)) => (a -> Bool) -> ListMod mod l inc a -> l inc (mod l inc (Maybe a))
findInc p = memo $ \recur mxs -> force mxs >>= \xs -> case xs of
	ConsMod x mxs' -> if p x
		then return $ Just x
		else recur mxs' >>= force
	NilMod -> return Nothing

-- | list intersection with a merge operation for similar entries
intersectionByInc :: (IncK inc (ListMod' mod l inc a),IncK inc (Maybe a),Output mod l inc,Memo (ListMod mod l inc a)) => (a -> a -> Bool) -> (a -> a -> l inc a) -> ListMod mod l inc a -> ListMod mod l inc a -> l inc (ListMod mod l inc a)
intersectionByInc cmp mrg mxs mys = memo (\recur mxs -> force mxs >>= \xs -> case xs of
	ConsMod x mxs' -> do
		mb <- findInc (cmp x) mys >>= force
		case mb of
			Just y -> mrg x y >>= \xy -> liftM (ConsMod xy) $ recur mxs'
			Nothing -> recur mxs' >>= force
	NilMod -> return NilMod) mxs

-- | list difference
differenceByInc :: (IncK inc (ListMod' mod l inc a),IncK inc (Maybe a),Output mod l inc,Memo (ListMod mod l inc a)) => (a -> a -> Bool) -> ListMod mod l inc a -> ListMod mod l inc a -> l inc (ListMod mod l inc a)
differenceByInc cmp mxs mys = memo (\recur mxs -> force mxs >>= \xs -> case xs of
	ConsMod x mxs' -> do
		mb <- findInc (cmp x) mys >>= force
		case mb of
			Just y -> recur mxs' >>= force
			Nothing -> liftM (ConsMod x) $ recur mxs'
	NilMod -> return NilMod) mxs

-- * joins lists with constant concatenation

data JoinListMod'
	(mod :: ((* -> * -> *) -> * -> * -> *))
	(l :: * -> * -> *)
	inc
	a
	= EmptyMod | SingleMod a | JoinMod (JoinListMod mod l inc a) (JoinListMod mod l inc a) 

type JoinListMod mod l inc a = mod l inc (JoinListMod' mod l inc a)

deriving instance Typeable JoinListMod'
deriving instance (Eq a,Eq (JoinListMod mod l inc a)) => Eq (JoinListMod' mod l inc a)

instance (DeepTypeable mod,DeepTypeable l,DeepTypeable inc,DeepTypeable a,DeepTypeable (JoinListMod mod l inc a)) => DeepTypeable (JoinListMod' mod l inc a) where
	typeTree (_::Proxy (JoinListMod' mod l inc a)) = MkTypeTree (mkName "Control.Monad.Incremental.List.JoinListMod'") args [MkConTree (mkName "Control.Monad.Incremental.List.EmptyMod") [],MkConTree (mkName "Control.Monad.Incremental.List.SingleMod") [typeTree (Proxy::Proxy a)],MkConTree (mkName "Control.Monad.Incremental.List.JoinMod") [typeTree (Proxy::Proxy (JoinListMod mod l inc a))]]
		where args = [typeTree (Proxy::Proxy mod),typeTree (Proxy::Proxy l),typeTree (Proxy::Proxy inc),typeTree (Proxy::Proxy a)]

type JoinListU l a = JoinListMod U l Adapton a
type JoinListU' l a = JoinListMod' U l Adapton a

type JoinListM l a = JoinListMod M l Adapton a
type JoinListM' l a = JoinListMod' M l Adapton a

type JoinListL l a = JoinListMod L l Adapton a
type JoinListL' l a = JoinListMod' L l Adapton a

type JoinListLazyNonIncU l a = JoinListMod LazyNonIncU l LazyNonInc a
type JoinListLazyNonIncU' l a = JoinListMod' LazyNonIncU l LazyNonInc a

type JoinListLazyNonIncL l a = JoinListMod LazyNonIncL l LazyNonInc a
type JoinListLazyNonIncL' l a = JoinListMod' LazyNonIncL l LazyNonInc a

type JoinListLazyNonIncM l a = JoinListMod LazyNonIncM l LazyNonInc a
type JoinListLazyNonIncM' l a = JoinListMod' LazyNonIncM l LazyNonInc a

instance (Display l1 inc a,Display l1 inc (JoinListMod mod l inc a)) => Display l1 inc (JoinListMod' mod l inc a) where
	displaysPrec proxyL proxyInc EmptyMod r = return $ "EmptyMod" ++ r
	displaysPrec proxyL proxyInc (SingleMod x) rest = do
		sx <- displaysPrec proxyL proxyInc x (')':rest)
		return $ "(SingleMod " ++ sx
	displaysPrec proxyL proxyInc (JoinMod mxs mys) rest = do
		sys <- displaysPrec proxyL proxyInc mys (')':rest)
		sxs <- displaysPrec proxyL proxyInc mxs (' ':sys)
		return $ "(JoinMod " ++ sxs

instance (NFDataInc l1 inc a,NFDataInc l1 inc (JoinListMod mod l inc a)) => NFDataInc l1 inc (JoinListMod' mod l inc a) where
	nfDataInc proxyL proxyInc EmptyMod = return $! ()
	nfDataInc proxyL proxyInc (SingleMod x) = nfDataInc proxyL proxyInc x
	nfDataInc proxyL proxyInc (JoinMod x y) = do
		a <- nfDataInc proxyL proxyInc x
		b <- nfDataInc proxyL proxyInc y
		return $! a `seq` b

instance (DeepTypeable mod,DeepTypeable inc,DeepTypeable l,Sat (ctx (JoinListMod' mod l inc a)),MData ctx (l1 inc) a,MData ctx (l1 inc) (JoinListMod mod l inc a))
			=> MData ctx (l1 inc) (JoinListMod' mod l inc a) where
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

joinListInc :: (IncK inc (JoinListMod' mod l inc a),Output mod l inc) => JoinListMod mod l inc a -> JoinListMod mod l inc a -> l inc (JoinListMod mod l inc a)
joinListInc mxs mys = thunk $ return $ JoinMod mxs mys

joinListInc' :: (IncK inc (JoinListMod' mod l inc a),Output mod l inc) => JoinListMod' mod l inc a -> JoinListMod' mod l inc a -> l inc (JoinListMod' mod l inc a)
joinListInc' mxs mys = do
	txs <- thunk $ return mxs
	tys <- thunk $ return mys
	return $ JoinMod txs tys

-- | a self-pruning joinlist concatenation operation
joinListPruneInc :: (IncK inc (JoinListMod' mod l inc a),Output mod l inc) => JoinListMod mod l inc a -> JoinListMod mod l inc a -> l inc (JoinListMod mod l inc a)
joinListPruneInc tx ty = thunk $ do
	x <- force tx
	isEmptyJoinListMod' x >>= \b -> if b
		then force ty
		else do
			y <- force ty
			isEmptyJoinListMod' y >>= \b -> if b
				then return x
				else return $ JoinMod tx ty

mapJoinListInc :: (IncK inc (JoinListMod' thunk l inc b),IncK inc (JoinListMod' mod l inc a),Thunk mod l inc,Eq (JoinListMod thunk l inc b),Memo (JoinListMod mod l inc a),Output thunk l inc)
	=> (a -> l inc b) -> JoinListMod mod l inc a -> l inc (JoinListMod thunk l inc b)
mapJoinListInc f = memo $ \recur mxs -> read mxs >>= \xs -> case xs of
	EmptyMod -> return EmptyMod
	SingleMod x -> liftM SingleMod $ f x
	JoinMod mxs1 mxs2 -> do
		mys1 <- recur mxs1
		mys2 <- recur mxs2
		return $ JoinMod mys1 mys2

-- | deep traversal that tests whether a joinlist is empty
isEmptyJoinListMod' :: (IncK inc (JoinListMod' mod l inc a),Output mod l inc) => JoinListMod' mod l inc a -> l inc Bool
isEmptyJoinListMod' EmptyMod = return True
isEmptyJoinListMod' (SingleMod _) = return False
isEmptyJoinListMod' (JoinMod l r) = do
	lb <- force l >>= isEmptyJoinListMod'
	if lb
		then force r >>= isEmptyJoinListMod'
		else return False

-- | Random number generator
data Seeds = Seeds !Int Seeds deriving Typeable -- infinite list
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
	memoKey (Seeds i _) = i
	memoWeak (Seeds i _) = MkWeak $ Weak.mkWeak i

-- * List change operations

mapM_ :: (Thunk mod l inc,IncK inc (ListMod' mod l inc a))
	=> (a -> l inc b) -> ListMod mod l inc a -> l inc ()
mapM_ f mxs = do
	xs <- read mxs
	case xs of
		NilMod -> return ()
		ConsMod x mxs -> f x >> mapM_ f mxs

mapListMod :: (IncK inc (ListMod' mod l inc a),IncK inc (ListMod' mod l inc b),Input mod l inc) => (a -> l inc b) -> [a] -> l inc (ListMod mod l inc b)
mapListMod f [] = ref NilMod
mapListMod f (x:xs) = mod $ do
	y <- f x
	mys <- mapListMod f xs
	return $ ConsMod y mys

toListRef :: (IncK inc (ListMod' mod l inc a),Input mod l inc) => [a] -> l inc (ListMod mod l inc a)
toListRef [] = ref NilMod
toListRef (x:xs) = do
	mxs <- toListRef xs
	ref (ConsMod x mxs)

toListRef' :: (IncK inc (ListMod' mod l inc a),Input mod l inc) => [a] -> l inc (ListMod' mod l inc a)
toListRef' [] = return NilMod
toListRef' (x:xs) = do
	mxs <- toListRef xs
	return (ConsMod x mxs)

toListMod :: (IncK inc (ListMod' mod l inc a),Input mod l inc) => [a] -> l inc (ListMod mod l inc a)
toListMod = mapListMod return

toListModIO :: (IncK inc (ListMod' mod l inc a),Input mod l inc) => [a] -> IO (ListMod mod l inc a)
toListModIO = runIncremental . outside . toListMod

modifyListModAt :: (IncK inc (ListMod' mod l inc a),Layer Outside inc,Input mod l inc)
	=> ListMod mod l inc a -> Int -> (ListMod' mod l inc a -> l inc (ListMod' mod l inc a)) -> Outside inc ()
modifyListModAt mxs 0 f = modify mxs f
modifyListModAt mxs i f = getOutside mxs >>= \xs -> case xs of
	ConsMod x mxs -> modifyListModAt mxs (pred i) f
	NilMod -> error "position not found"

setListModAt :: (IncK inc (ListMod' mod l inc a),Input mod l inc,Layer Outside inc)
	=> ListMod mod l inc a -> Int -> (ListMod' mod l inc a -> Outside inc (ListMod' mod l inc a)) -> Outside inc ()
setListModAt mxs 0 f = getOutside mxs >>= f >>= set mxs
setListModAt mxs i f = getOutside mxs >>= \xs -> case xs of
	ConsMod x mxs -> setListModAt mxs (pred i) f
	NilMod -> error "position not found"

setListModHeadAt :: (IncK inc (ListMod' mod l inc a),Input mod l inc,Layer Outside inc)
	=> ListMod mod l inc a -> Int -> a -> Outside inc ()
setListModHeadAt mxs i x' = setListModAt mxs i $ \xs -> case xs of
	ConsMod x mxs -> return $ ConsMod x' mxs
	NilMod -> return NilMod

deleteListModAt :: (IncK inc (ListMod' mod l inc a),Input mod l inc,Layer Outside inc) => Int -> ListMod mod l inc a -> Outside inc ()
deleteListModAt i mxs = setListModAt mxs i $ \xs -> case xs of
	ConsMod x mxs' -> getOutside mxs'
	NilMod -> error "shouldn't be empty"

insertListModAt :: (IncK inc (ListMod' mod l inc a),
	Input mod l inc,Layer Outside inc) => Int -> a -> ListMod mod l inc a -> Outside inc ()
insertListModAt i x mxs = setListModAt mxs i $ \xs -> do
	mxs' <- refOutside xs
	return $ ConsMod x mxs'

$(deriveMemo ''ListMod')
$(deriveMemo ''JoinListMod')
