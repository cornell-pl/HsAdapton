{-# LANGUAGE KindSignatures, MultiParamTypeClasses, FlexibleInstances #-}

-- -*- haskell-hugs-program-args: ("+." "-98") -*-

-- A monad of mutable circular lists.

module Control.Monad.Adaptive.CircularList where

import Control.Monad.Ref
import Control.Monad

newtype CircularList (m :: * -> *) r a = CL { deCL :: r (CircularList m r a, a ,CircularList m r a) }

instance EqRef r => Eq (CircularList m r a) where
	(CL r) == (CL r') = eqRef r r'

showCL :: (Ref m r,EqRef r) => (a -> String) -> CircularList m r a -> m String
showCL shw (CL xs) = do
	(_,x,r) <- readRef xs
	str <- showCL' shw r r
	return $ shw x ++ str

showCL' :: (Ref m r,EqRef r) => (a -> String) -> CircularList m r a -> CircularList m r a -> m String
showCL' shw lst (CL xs) = do
	(_,x,r) <- readRef xs
	if lst == r 
		then return ""
		else do
			str <- showCL' shw lst r
			return $ shw x ++ str

circularList :: Ref m r => a -> m (CircularList m r a)
circularList a = do
  r <- newRef (error "unset CircularList")
  let l = CL r
  writeRef r (l,a,l)
  return l

get :: Ref m r => CircularList m r a -> m (CircularList m r a, a,CircularList m r a)
get = readRef . deCL

set :: Ref m r => CircularList m r a -> (CircularList m r a, a,CircularList m r a) -> m ()
set = writeRef . deCL

update :: Ref m r => CircularList m r a -> a -> m ()
update l a = do
         (p,_,n) <- get l
         set l (p,a,n)

val :: Ref m r => CircularList m r a -> m a
val l = (\ (p,a,n) -> a) `liftM` get l

next :: Ref m r => CircularList m r a -> m (CircularList m r a)
next l = liftM (\(p,a,n) -> n) (get l)

previous :: Ref m r => CircularList m r a -> m (CircularList m r a)
previous l = (\ (p,a,n) -> p) `liftM` get l

insert :: Ref m r => CircularList m r a -> a -> m (CircularList m r a)
insert l a = do
  (p,b,n) <- get l
  n' <- CL `liftM` newRef (l,a,n)
  set l (p,b,n')
  nl <- next n'
  (_,nb,nn) <- get nl
  set nl (n',nb,nn)
  return n'

delete :: Ref m r => CircularList m r a -> m ()
delete l = do
  (p,_,n) <- get l
  (pp,a,_) <- get p
  set p (pp,a,n)
  (_,a',nn) <- get n
  set n (p,a',nn)
