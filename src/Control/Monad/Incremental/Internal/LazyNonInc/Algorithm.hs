{-# LANGUAGE ConstraintKinds, UndecidableInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Control.Monad.Incremental.Internal.LazyNonInc.Algorithm where

import Control.Monad.Incremental.Internal.LazyNonInc.Types
import Control.Monad.Incremental.Internal.LazyNonInc.Layers
import Control.Monad.Incremental

import Control.Monad.Trans
import Data.Unique
import Prelude hiding (const,read,mod)
import Data.WithClass.MData
import Data.IORef

instance (Layer l inc) => Thunk LazyNonIncU l inc where
	new = thunkLazyNonIncU
	{-# INLINE new #-}
	newc = thunkLazyNonIncU . return
	{-# INLINE newc #-}
	read = forceLazyNonIncU
	{-# INLINE read #-}

instance (Layer l inc) => Output LazyNonIncU l inc where
	thunk = thunkLazyNonIncU
	{-# INLINE thunk #-}
	force = forceLazyNonIncU
	{-# INLINE force #-}

thunkLazyNonIncU :: (Layer l inc,Layer l1 inc) => l1 inc a -> l inc (LazyNonIncU l1 inc a)
thunkLazyNonIncU c = unsafeIOToInc $ do
	rc <- newIORef c
	return $ LazyNonIncU rc

forceLazyNonIncU :: (Layer l inc) => LazyNonIncU l inc a -> l inc a
forceLazyNonIncU (LazyNonIncU rc) = do
	c <- unsafeIOToInc $ readIORef rc
	c

refLazyNonIncL :: (Layer l inc) => a -> l inc (LazyNonIncL l inc a)
refLazyNonIncL a = modLazyNonIncL (return a)

modLazyNonIncL :: (Layer l inc) => l inc a -> l inc (LazyNonIncL l inc a)
modLazyNonIncL m = unsafeIOToInc $ do
	dta <- newIORef m
	return $ LazyNonIncL dta

setLazyNonIncL :: (Layer l inc) => LazyNonIncL l inc a -> a -> Outside inc ()
setLazyNonIncL l a = overwriteLazyNonIncL l (return a)

overwriteLazyNonIncL :: (Layer l inc) => LazyNonIncL l inc a -> l inc a -> Outside inc ()
overwriteLazyNonIncL (LazyNonIncL rm) m = unsafeIOToInc $ do
	writeIORef rm m

modifyLazyNonIncL :: (Layer l inc) => LazyNonIncL l inc a -> (a -> l inc a) -> Outside inc ()
modifyLazyNonIncL (LazyNonIncL (rm)) f = unsafeIOToInc $ do
	m <- readIORef rm
	writeIORef rm $ m >>= f

getLazyNonIncInnerL :: (Layer Inside inc) => LazyNonIncL Inside inc a -> Inside inc a
getLazyNonIncInnerL (LazyNonIncL (rm)) = do
	m <- unsafeIOToInc $ readIORef rm
	a <- m
	unsafeIOToInc $ writeIORef rm $ return a
	return a
	
getLazyNonIncOuterL :: (Layer Outside inc) => LazyNonIncL Outside inc a -> Outside inc a
getLazyNonIncOuterL (LazyNonIncL (rm)) = do
	m <- unsafeIOToInc $ readIORef rm
	m

instance (Layer Inside inc) => Thunk LazyNonIncL Inside inc where
	new = modLazyNonIncL
	{-# INLINE new #-}
	newc = refLazyNonIncL
	{-# INLINE newc #-}
	read = getLazyNonIncInnerL
	{-# INLINE read #-}

instance (Layer Outside inc) => Thunk LazyNonIncL Outside inc where
	new = modLazyNonIncL
	{-# INLINE new #-}
	newc = refLazyNonIncL
	{-# INLINE newc #-}
	read = getLazyNonIncOuterL
	{-# INLINE read #-}

instance (Layer Inside inc) => Input LazyNonIncL Inside inc where
	ref = refLazyNonIncL
	{-# INLINE ref #-}
	mod = modLazyNonIncL
	{-# INLINE mod #-}
	get = getLazyNonIncInnerL
	{-# INLINE get #-}
	set = setLazyNonIncL
	{-# INLINE set #-}
	overwrite = overwriteLazyNonIncL
	{-# INLINE overwrite #-}
	modify = modifyLazyNonIncL
	{-# INLINE modify #-}

instance (Layer Outside inc) => Input LazyNonIncL Outside inc where
	ref = refLazyNonIncL
	{-# INLINE ref #-}
	mod = modLazyNonIncL
	{-# INLINE mod #-}
	get = getLazyNonIncOuterL
	{-# INLINE get #-}
	set = setLazyNonIncL
	{-# INLINE set #-}
	overwrite = overwriteLazyNonIncL
	{-# INLINE overwrite #-}
	modify = modifyLazyNonIncL
	{-# INLINE modify #-}

refLazyNonIncM :: (Layer l inc) => a -> l inc (LazyNonIncM l inc a)
refLazyNonIncM a = unsafeIOToInc $ do
	dta <- newIORef a
	return $ LazyNonIncM (dta)
	
setLazyNonIncM :: (Layer l inc) => LazyNonIncM l inc a -> a -> Outside inc ()
setLazyNonIncM (LazyNonIncM rm) a = unsafeIOToInc $ writeIORef rm a
	
getLazyNonIncM :: (Layer l inc) => LazyNonIncM l inc a -> l inc a
getLazyNonIncM (LazyNonIncM rm) = unsafeIOToInc $ readIORef rm

instance (Layer l inc) => Thunk LazyNonIncM l inc where
	new m = m >>= refLazyNonIncM
	{-# INLINE new #-}
	newc = refLazyNonIncM
	{-# INLINE newc #-}
	read = getLazyNonIncM
	{-# INLINE read #-}

instance (Layer l inc) => Input LazyNonIncM l inc where
	ref = refLazyNonIncM
	{-# INLINE ref #-}
	get = getLazyNonIncM
	{-# INLINE get #-}
	set = setLazyNonIncM
	{-# INLINE set #-}

instance (IncK inc a,Layer l inc,Thunk LazyNonIncM l inc,MData ctx (l inc) a
		, Sat (ctx (LazyNonIncM l inc a)),DeepTypeable (LazyNonIncM l inc a)
		) => MData ctx (l inc) (LazyNonIncM l inc a) where
	gfoldl ctx k z t = z new >>= flip k (read t)
	gunfold ctx k z c = z new >>= k
	toConstr ctx m = dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Control.Monad.Adapton.LazyNonIncM" [mkConstr ty "LazyNonIncM" [] Prefix]

instance (IncK inc a,Layer l inc,Thunk LazyNonIncL l inc,MData ctx (l inc) a
		, Sat (ctx (LazyNonIncL l inc a)),DeepTypeable (LazyNonIncL l inc a)
		) => MData ctx (l inc) (LazyNonIncL l inc a) where
	gfoldl ctx k z t = z new >>= flip k (read t)
	gunfold ctx k z c = z new >>= k
	toConstr ctx m = dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Control.Monad.Adapton.LazyNonIncL" [mkConstr ty "LazyNonIncL" [] Prefix]

instance (IncK inc a,Layer l inc,Thunk LazyNonIncU l inc,MData ctx (l inc) a
		, Sat (ctx (LazyNonIncU l inc a)),DeepTypeable (LazyNonIncU l inc a)
		) => MData ctx (l inc) (LazyNonIncU l inc a) where
	gfoldl ctx k z t = z new >>= flip k (read t)
	gunfold ctx k z c = z new >>= k
	toConstr ctx m = dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Control.Monad.Adapton.LazyNonIncU" [mkConstr ty "LazyNonIncU" [] Prefix]

