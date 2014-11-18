{-# LANGUAGE UndecidableInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Control.Monad.Incremental.LazyNonInc.Algorithm where

import Control.Monad.Incremental.LazyNonInc.Types
import Control.Monad.Incremental.LazyNonInc.Layers
import Control.Monad.Incremental
import Control.Monad.Ref
import Control.Monad.Trans
import Data.Unique
import Prelude hiding (const,read,mod)
import Data.WithClass.MData

instance (Layer l inc r m) => Thunk LazyNonIncU l inc r m where
	new = thunkLazyNonIncU
	{-# INLINE new #-}
	newc = thunkLazyNonIncU . return
	{-# INLINE newc #-}
	read = forceLazyNonIncU
	{-# INLINE read #-}

instance (MonadRef r m,Layer l inc r m) => Output LazyNonIncU l inc r m where
	thunk = thunkLazyNonIncU
	{-# INLINE thunk #-}
	force = forceLazyNonIncU
	{-# INLINE force #-}

--instance (MonadRef r m,Layer l inc r m) => Input LazyNonIncU l inc r m where
--	ref = thunkLazyNonIncU . return
--	{-# INLINE ref #-}
--	mod = thunkLazyNonIncU
--	{-# INLINE mod #-}
--	get = forceLazyNonIncU
--	{-# INLINE get #-}
--	set = setLazyNonIncU
--	{-# INLINE set #-}
--	-- thunks cannot support overwrite

thunkLazyNonIncU :: (Layer l inc r m,Layer l1 inc r m) => l1 inc r m a -> l inc r m (LazyNonIncU l1 inc r m a)
thunkLazyNonIncU c = inL $ do
	rc <- newRef c
	return $ LazyNonIncU rc

forceLazyNonIncU :: (Layer l inc r m) => LazyNonIncU l inc r m a -> l inc r m a
forceLazyNonIncU (LazyNonIncU rc) = do
	c <- inL $ readRef rc
	c

--setLazyNonIncU :: (Layer l inc r m) => LazyNonIncU l inc r m a -> a -> Outside inc r m ()
--setLazyNonIncU (LazyNonIncU rc) a = inL $ do
--	writeRef rc (return a)

refLazyNonIncL :: (Layer l inc r m) => a -> l inc r m (LazyNonIncL l inc r m a)
refLazyNonIncL a = modLazyNonIncL (return a)

modLazyNonIncL :: (Layer l inc r m) => l inc r m a -> l inc r m (LazyNonIncL l inc r m a)
modLazyNonIncL m = inL $ do
	dta <- newRef m
	return $ LazyNonIncL dta

setLazyNonIncL :: (Layer l inc r m) => LazyNonIncL l inc r m a -> a -> Outside inc r m ()
setLazyNonIncL l a = overwriteLazyNonIncL l (return a)

overwriteLazyNonIncL :: (Layer l inc r m) => LazyNonIncL l inc r m a -> l inc r m a -> Outside inc r m ()
overwriteLazyNonIncL (LazyNonIncL rm) m = inL $ do
	writeRef rm m

modifyLazyNonIncL :: (Layer l inc r m) => LazyNonIncL l inc r m a -> (a -> l inc r m a) -> Outside inc r m ()
modifyLazyNonIncL (LazyNonIncL (rm)) f = inL $ do
	m <- readRef rm
	writeRef rm $ m >>= f

getLazyNonIncInnerL :: (Layer Inside inc r m) => LazyNonIncL Inside inc r m a -> Inside inc r m a
getLazyNonIncInnerL (LazyNonIncL (rm)) = do
	m <- inL $ readRef rm
	a <- m
	inL $ writeRef rm $ return a
	return a
	
getLazyNonIncOuterL :: (Layer Outside inc r m) => LazyNonIncL Outside inc r m a -> Outside inc r m a
getLazyNonIncOuterL (LazyNonIncL (rm)) = do
	m <- inL $ readRef rm
	m

instance (Layer Inside inc r m) => Thunk LazyNonIncL Inside inc r m where
	new = modLazyNonIncL
	{-# INLINE new #-}
	newc = refLazyNonIncL
	{-# INLINE newc #-}
	read = getLazyNonIncInnerL
	{-# INLINE read #-}

instance (Layer Outside inc r m) => Thunk LazyNonIncL Outside inc r m where
	new = modLazyNonIncL
	{-# INLINE new #-}
	newc = refLazyNonIncL
	{-# INLINE newc #-}
	read = getLazyNonIncOuterL
	{-# INLINE read #-}

instance (MonadRef r m,Layer Inside inc r m) => Input LazyNonIncL Inside inc r m where
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

instance (MonadRef r m,Layer Outside inc r m) => Input LazyNonIncL Outside inc r m where
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

refLazyNonIncM :: (Layer l inc r m) => a -> l inc r m (LazyNonIncM l inc r m a)
refLazyNonIncM a = inL $ do
	dta <- newRef a
	return $ LazyNonIncM (dta)
	
setLazyNonIncM :: (Layer l inc r m) => LazyNonIncM l inc r m a -> a -> Outside inc r m ()
setLazyNonIncM (LazyNonIncM (rm)) a = inL $ do
	writeRef rm a
	
getLazyNonIncM :: (Layer l inc r m) => LazyNonIncM l inc r m a -> l inc r m a
getLazyNonIncM (LazyNonIncM (rm)) = inL $ readRef rm

instance (Layer l inc r m) => Thunk LazyNonIncM l inc r m where
	new m = m >>= refLazyNonIncM
	{-# INLINE new #-}
	newc = refLazyNonIncM
	{-# INLINE newc #-}
	read = getLazyNonIncM
	{-# INLINE read #-}

instance (MonadRef r m,Layer l inc r m) => Input LazyNonIncM l inc r m where
	ref = refLazyNonIncM
	{-# INLINE ref #-}
	get = getLazyNonIncM
	{-# INLINE get #-}
	set = setLazyNonIncM
	{-# INLINE set #-}

instance (Eq a,Layer l inc r m,Thunk LazyNonIncM l inc r m,MData ctx (l inc r m) a
		, Sat (ctx (LazyNonIncM l inc r m a)),DeepTypeable (LazyNonIncM l inc r m a)
		) => MData ctx (l inc r m) (LazyNonIncM l inc r m a) where
	gfoldl ctx k z t = z new >>= flip k (read t)
	gunfold ctx k z c = z new >>= k
	toConstr ctx m = dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Control.Monad.Adapton.LazyNonIncM" [mkConstr ty "LazyNonIncM" [] Prefix]

instance (Eq a,Layer l inc r m,Thunk LazyNonIncL l inc r m,MData ctx (l inc r m) a
		, Sat (ctx (LazyNonIncL l inc r m a)),DeepTypeable (LazyNonIncL l inc r m a)
		) => MData ctx (l inc r m) (LazyNonIncL l inc r m a) where
	gfoldl ctx k z t = z new >>= flip k (read t)
	gunfold ctx k z c = z new >>= k
	toConstr ctx m = dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Control.Monad.Adapton.LazyNonIncL" [mkConstr ty "LazyNonIncL" [] Prefix]

instance (Eq a,Layer l inc r m,Thunk LazyNonIncU l inc r m,MData ctx (l inc r m) a
		, Sat (ctx (LazyNonIncU l inc r m a)),DeepTypeable (LazyNonIncU l inc r m a)
		) => MData ctx (l inc r m) (LazyNonIncU l inc r m a) where
	gfoldl ctx k z t = z new >>= flip k (read t)
	gunfold ctx k z c = z new >>= k
	toConstr ctx m = dataTypeOf ctx m >>= (return . (flip indexConstr) 1)
	dataTypeOf ctx x = return ty
		where ty = mkDataType "Control.Monad.Adapton.LazyNonIncU" [mkConstr ty "LazyNonIncU" [] Prefix]

