{-# LANGUAGE UndecidableInstances, FlexibleInstances, MultiParamTypeClasses, KindSignatures #-}
-- A monad for manipulating ordered lists.  Follows the implementation
-- given in the appendix of O'Neill's and Burton's JFP paper, but
-- doesn't impose any fixed limit on the number of elements.

-- References:

-- Dietz and Sleator: "Two algorithms for maintaining order in a
-- list", in Proc. of 19th ACM Symposium of Theory of Computing, 1987.

-- O'Neill and Burton: "A New Method For Functional Arrays", Journal
-- of Functional Programming, vol7, no 5, September 1997.

module Control.Monad.Adaptive.OrderedList where

import Control.Monad
import Control.Monad.Adaptive.MonadUtil
import Control.Monad.Adaptive.Ref
import Control.Monad.Adaptive.CircularList hiding (delete,insert,next,update,previous)
import qualified Control.Monad.Adaptive.CircularList as CircularList

import Control.Monad.Reader (Reader(..),ReaderT(..))
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (State(..),StateT(..))
import qualified Control.Monad.State as State

import Data.Set (Set(..))
import qualified Data.Set as Set
import qualified Data.List as List

import System.IO.Unsafe(unsafePerformIO) -- for diagnostic

type Record m r a = CircularList m r (Bool,Integer,a)
--(deleted?,label,value)

-- A time-ordered, doubly linked list of all edges.
newtype OrderedList m r a b = OL { deOL :: (r Integer,r Integer,Record m r a) -> m b }

run :: Ref m r => OrderedList m r () b -> m b
run l = do
    base <- circularList (False,0,())
    s <- newRef 0
    mr <- newRef m
    deOL l (mr,s,base)
  where 
    m = 2^16

inM :: Ref m r => m b -> OrderedList m r a b
inM m = OL $ \e -> m

instance (Ref m r,MonadPlus m) => MonadPlus (OrderedList m r a) where
	mzero = inM mzero
	mplus (OL f1) (OL f2) = OL $ \k -> f1 k `mplus` f2 k

instance Ref m r => Monad (OrderedList m r a) where
  return a = inM (return a)
  (OL m) >>= f = OL $ \e -> m e >>= \a -> deOL (f a) e

instance Ref m r => Functor (OrderedList m r a) where 
  fmap f m = m >>= return . f

instance Ref m r => Ref (OrderedList m r a) r where
  newRef v     = inM (newRef v)
  readRef r    = inM (readRef r)
  writeRef r v = inM (writeRef r v)

mop a o b = op2 o a b
op2 f a b = op1 f a `ap` b
op1 f a = return f `ap` a

instance Eq (OrderedList m r a b) where { }
instance Show (OrderedList m r a b) where { }

instance (Ref m r, Num b) => Num (OrderedList m r a b) where
  (+)         = op2 (+)
  (-)         = op2 (-)
  (*)         = op2 (*)
  negate      = op1 negate
  abs         = op1 abs
  signum      = op1 signum
  fromInteger = return . fromInteger
--  fromInt     = return . fromInt

instance Ord (OrderedList m r a b) where { }
instance (Ref m r, Real b) => Real (OrderedList m r a b) where { }
instance Enum (OrderedList m r a b) where { }

instance (Ref m r, Integral b) => Integral (OrderedList m r a b) where
  rem = op2 rem
  div = op2 div
  mod = op2 mod

-- initial time
base :: Ref m r => OrderedList m r a (Record m r a)
base = OL $ \(m,n,b) -> return b

bigM :: Ref m r => OrderedList m r a Integer
bigM = OL $ \(m,n,b) -> readRef m

size :: Ref m r => OrderedList m r a Integer
size = OL $ \(m,n,b) -> readRef n

adjsize :: Ref m r => Integer -> OrderedList m r a ()
adjsize i = OL $ \(m,n,b) -> do s <- readRef n
                                writeRef n (s+i)

setSize :: Ref m r => Integer -> OrderedList m r a ()
setSize n' = OL $ \(m,n,b) -> writeRef n n'

record :: Ref m r => Record m r a -> OrderedList m r a (Bool,Integer,a)
record r = inM (val r)

rval :: Ref m r => Record m r a -> OrderedList m r a a
rval r = fmap (\ (d,i,a) -> a) (record r)

next :: Ref m r => Record m r a -> OrderedList m r a (Record m r a)
next r = inM (CircularList.next r)

previous :: Ref m r => Record m r a -> OrderedList m r a (Record m r a)
previous r = inM (CircularList.previous r)

s x = next x

-- label
label :: Ref m r => Record m r a -> OrderedList m r a Integer
label r = fmap (\ (d,i,a) -> i) (record r)

-- gap
gap e f = (label f - label e) `mod` bigM

isSplicedOut :: Ref m r => Record m r a -> OrderedList m r a Bool
isSplicedOut r = fmap (\ (d,i,a) -> d) (record r)

lbase :: Ref m r => OrderedList m r a Integer
lbase = base >>= label

gstar :: Ref m r => Record m r a -> Record m r a -> OrderedList m r a Integer
gstar e f = ifM (mop (label e) (==) (label f))
             bigM
             (gap e f)

-- be aware that the order of records may be different from normal Integer order!!
order :: Ref m r => Record m r a -> Record m r a -> OrderedList m r a Ordering
order x y = base >>= \b -> return (compare) `ap` gap b x `ap` gap b y



update :: Ref m r => ((Bool,Integer)->(Bool,Integer)) -> 
                     Record m r a -> OrderedList m r a ()
update f r = do
   (d,i,a) <- record r
   let (d',i') = f (d,i)
   inM (CircularList.update r (d',i',a))
   
delete :: Ref m r => Record m r a -> OrderedList m r a ()
delete r = unlessM (isSplicedOut r) $ do -- unless it has already been deleted
             ifM (mop lbase (==) (label r))
               (error "OrderedList: cannot delete base element")
               (do inM (CircularList.delete r)
                   update (\ (_,i) -> (True,i)) r -- mark as deleted
                   adjsize (-1)
                   checkinvariant)

spliceOut :: Ref m r => Record m r a -> Record m r a -> OrderedList m r a ()
spliceOut r s = next r >>= spl where
  spl r = do 
    unlessM (mop lbase (==) (label r)) $
        whenM ((==LT) `fmap` order r s)
              (do r' <- next r
                  delete r
                  spl r')

increaseBigM :: Ref m r => OrderedList m r a ()
increaseBigM = do OL $ \(m,n,b) -> mapRef (*2) m

-- has to rearrange the existing nodes
insertAfter :: (Ref m r) => Record m r a -> a -> OrderedList m r a (Record m r a)
insertAfter r a = ifM (isSplicedOut r) (label r >>= \i -> error $ "OrderedList: cannot insert after deleted node "++show i) $ do
	whenM (mop bigM (<=) (4*(size+1)*(size+1))) increaseBigM
	r' <- s r
	d <- gstar r r'
	unless (d > 1) (renumber r)
	li <- (label r + (gstar r r' `div` 2)) `mod` bigM
	inM (CircularList.insert r (False,li,a))
	adjsize 1
	checkinvariant
	next r

insertBefore :: (Ref m r) => Record m r a -> a -> OrderedList m r a (Record m r a)
insertBefore r a = do
	p <- previous r
	insertAfter p a

renumber :: Ref m r => Record m r a -> OrderedList m r a ()
renumber e = do
   let getj j e0 ej = do
          ifM (mop (gap e0 ej) (>) (return (j * j)))
            (return (j,ej)) $ do
            ej' <- s ej
            ifM (mop (label ej') (==) (label e))
              (return (j,ej)) $ do
              getj (j+1) e0 ej'
   (j,sje) <- s e >>= getj 1 e
   d <- gstar e sje
   le <- label e
   m <- bigM
   let ren k ek | k == j     = return ()
                | otherwise  = do
          update (const (False,(le + ((k * d) `div` j)) `mod` m)) ek
          s ek >>= ren (k+1)
   s e >>= ren 1

checkinvariant :: Ref m r => OrderedList m r a ()
checkinvariant = return () --printinvariant

printinvariant :: Ref m r => OrderedList m r a ()
printinvariant = prall >> base >>= inv
  where inv r = do
             r' <- s r
             unlessM (mop lbase (==) (label r')) $ do
               ifM (mop (order r r') (==) (return LT))
                   (inv r')
                   (error "invariant")

-- prints the ordering of the existing identifiers
prall :: Ref m r => OrderedList m r a ()
prall = uprint "prall:" >> base >>= pr where
  pr r = do
    x <- label r
    b <- isSplicedOut r
    uprint (show x)
    r' <- s r
    unlessM (mop (base >>= order r') (==) (return EQ))
        (pr r')

uprint s = OL$ (\s' -> unsafePerformIO (putStrLn s) `seq` return ())
