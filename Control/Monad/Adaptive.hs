-- -*- haskell-hugs-program-args: ("+." "-98") -*-
{-# LANGUAGE TupleSections, RankNTypes, ScopedTypeVariables, ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances, DoAndIfThenElse, UndecidableInstances, FunctionalDependencies, KindSignatures, DataKinds, GADTs #-}

-- A monadic variant of the library from "Adaptive Functional
-- Programming", by Acar, Blelloch and Harper (POPL 2002).

-- Magnus Carlsson, magnus@cse.ogi.edu

-- The whole code under Control.Monad.Adaptive is part of the Adaptive library (http://hackage.haskell.org/package/Adaptive), slightly modified for our purposes.

module Control.Monad.Adaptive where

import Prelude 
import Control.Monad
import Control.Monad.Adaptive.MonadUtil
import Control.Monad.Adaptive.Ref
import qualified Control.Monad.Adaptive.OrderedList as OL
import Control.Monad.Adaptive.OrderedList(OrderedList)
import qualified Control.Monad.Adaptive.PriorityQueue as PQ
import qualified Control.Monad.Adaptive.CircularList as CL
import Control.Monad.Adaptive.PriorityQueue(PriorityQueue)	
import Control.Monad.Writer
import Data.Maybe
import Data.List

import Debug.Trace

class InM m' where
   inM :: Ref m r => m a -> m' m r a

class (InM n,InOL n,Monad (n m r), Ref m r) => NewMod n m r where
	newModBy :: (Show a) => (a -> a -> Bool) -> Changeable m r a -> n m r (Modifiable m r a)
	change :: Ref m r => Modifiable m r a -> a -> n m r ()
	changeMod ::  (Ref m r) => Modifiable m r a -> Changeable m r a -> n m r (Modifiable m r a)
	readMod   :: (Ref m r) => Modifiable m r a -> n m r a
	adIn :: (Ref m r) => Adaptive m r a -> n m r a
	chIn :: (Ref m r) => Changeable m r a -> n m r a

newMod :: (Show a,Eq a, NewMod n m r) => Changeable m r a -> n m r (Modifiable m r a)
newMod = newModBy (==)

newModA :: (Show a,Ref m r,Eq a) => Changeable m r a -> Adaptive m r (Modifiable m r a)
newModA = newMod

newModC :: (Show a,Ref m r,Eq a) => Changeable m r a -> Changeable m r (Modifiable m r a)
newModC = newMod

data Recomp m r = Edge { ori :: Int, dest :: Int, reader :: Adaptive m r Int, timeSpan :: TimeSpan m r } -- edge in the DDG
-- source and destination implicit in the reader: it starts by reading the source, and ends by writing to the destination.

type TimeStamp m r = OL.Record m r () -- timestamp

-- | State of the computation (global variables in the ML code)
newtype Adaptive m r a = Ad { deAd :: r (PriorityQueue (Recomp m r)) -> r (m (TimeStamp m r)) -> OrderedList m r () a }
-- args: priority queue of changed input edges, an expression the reads the current time (possibly from some other reference)
-- state: ordered list for managing timestamps
-- return: value

data Changeable m r a = Ch { deCh :: (a -> Adaptive m r Int) -> Adaptive m r Int } -- a continuation

data Modifiable m r a = Node { value :: r a, write :: r (a -> Adaptive m r ()), outEdges :: r [Recomp m r], lastModified :: r (TimeStamp m r) } -- node in the DDG

data TimeSpan m r = TimeSpan { start :: TimeStamp m r, stop :: TimeStamp m r }
type TimeSpans m r = [TimeSpan m r]

instance (Ref m r,MonadPlus m) => MonadPlus (Adaptive m r) where
	mzero = Ad $ \pq ct -> OL.inM mzero
	mplus (Ad f1) (Ad f2) = Ad $ \pq ct -> f1 pq ct `mplus` f2 pq ct

instance (Ref m r,MonadPlus m) => MonadPlus (Changeable m r) where
	mzero = Ch $ \k -> mzero
	mplus (Ch f1) (Ch f2) = Ch $ \k -> f1 k `mplus` f2 k

inAd :: (Ref m r) => Adaptive m r a -> Changeable m r a
inAd m = Ch $ \k -> (m >>= k)

class InOL m' where
  inOL :: Ref m r => OrderedList m r () b -> m' m r b

instance InOL Adaptive where
  inOL m = Ad $ \pq ct -> m

instance InOL (Changeable) where
  inOL m = inAd (inOL m)

instance (Ref m r) => Ref (Changeable m r) r where
  newRef v     = inM $ newRef v
  readRef x    = inM $ readRef x
  writeRef x v = inM $ writeRef x v

instance (Ref m r) => Monad (Changeable m r) where
  return a   = Ch $ \k -> k a
  Ch m >>= f = Ch $ \k -> m $ \a -> deCh (f a) k

instance (Ref m r) => Functor (Changeable m r) where
  fmap f m = m >>= return . f

instance Ref m r => Ref (Adaptive m r) r where
  newRef v     = inM $ newRef v
  readRef x    = inM $ readRef x
  writeRef x v = inM $ writeRef x v

instance Ref m r => Monad (Adaptive m r) where
  return a   = Ad $ \pq ct -> return a
  Ad m >>= f = Ad $ \pq ct -> m pq ct >>= \a -> deAd (f a) pq ct

instance Ref m r => Functor (Adaptive m r) where
  fmap f m = m >>= return . f

readModC :: (Ref m r) => Modifiable m r a -> Changeable m r a
readModC (Node r chg es lastmod) = do
	let ori = refId r
	start <- inAd stepTime
	(b1,starttime,_) <- inM $ CL.val start
	trace ("reading " ++ show ori ++ "@@" ++ show starttime) $ Ch $ \f -> do
		let run = do
			dest <- readRef r >>= f -- start by reading the input ref r, and end by writing to the output ref dest
			now <- readCurrentTime
			(b2,nowtime,_) <- inM $ CL.val now
			test <- inOL $ OL.order start now
			if (test == GT) then error $ show ori ++" "++show (dest) ++ ": start time " ++ show starttime ++ " should be smaller than stop time " ++ show nowtime
			else trace ("added edge " ++ " " ++ show ori ++ "@" ++ show starttime ++ " " ++ show (dest) ++ "@"++show nowtime) $
				mapRef (Edge ori dest run (TimeSpan start now) :) es -- add the new reading expression as an outgoing edge
			return dest
		run

pqRef :: Ref m r => Adaptive m r (r (PriorityQueue (Recomp m r)))
pqRef = Ad $ \pq ct -> return pq

readPq :: Ref m r => Adaptive m r (PriorityQueue (Recomp m r))
readPq = pqRef >>= readRef
writePq a = pqRef >>= flip writeRef a

ctRef :: Ref m r => Adaptive m r (r (m (TimeStamp m r)))
ctRef = Ad $ \pq ct -> return ct
readCurrentTimeExpr :: Ref m r => Adaptive m r (m (TimeStamp m r))
readCurrentTimeExpr = ctRef >>= readRef
readCurrentTime :: Ref m r => Adaptive m r (TimeStamp m r)
readCurrentTime = readCurrentTimeExpr >>= inM

writeCurrentTimeExpr :: Ref m r => m (TimeStamp m r) -> Adaptive m r ()
writeCurrentTimeExpr m = ctRef >>= flip writeRef m

writeCurrentTime str t = do
	isSplicedOut <- inOL (OL.isSplicedOut t)
	if isSplicedOut
		then do
			(_,time,_) <- inM $ CL.val t
			error $ str ++" tried to write the spliced out time "++show time++" as current!!!!"
		else do writeCurrentTimeExpr (return t)
	
stepTime :: Ref m r => Adaptive m r (TimeStamp m r)
stepTime = do
    readCurrentTime >>= inOL . flip OL.insertAfter () >>= writeCurrentTime "stepTime"
    readCurrentTime

instance InM (Changeable) where
  inM m = Ch $ \k -> (inM m >>= k)

chgBind :: Ref m r => Adaptive m r () -> Changeable m r a -> Changeable m r a
chgBind a (Ch m) = Ch $ \f -> m (\x -> do { i <- f x; a ; return i })

chgRet1 :: Ref m r => Int -> Changeable m r a -> Changeable m r a
chgRet1 i (Ch m) = Ch $ \f -> m ((>> return i) . f)

chgRet2 :: Ref m r => Int -> Changeable m r a -> Changeable m r a
chgRet2 i (Ch m) = Ch $ \f -> m f >> return i

instance InM Adaptive where
  inM m = Ad $ \pq ct -> OL.inM m

propagate :: Ref m r => Adaptive m r ()
propagate = do
	let prop = do
		pq <- readPq
		case PQ.min pq of
			Nothing -> return ()
			Just (Edge ori dest reader timespan,pq') -> do
				writePq pq'
				isSplicedOut <- inOL (OL.isSplicedOut (start timespan))
				(_,starttime,_) <- inM $ CL.val (start timespan)
				(_,stoptime,_) <- inM $ CL.val (stop timespan)
				if isSplicedOut then trace ("deleted "++show ori ++"@"++ show starttime++" "++ show (dest)++"@"++ show stoptime) $ return ()
				else trace ("reading interval " ++show ori ++"@"++ show starttime++" "++ show (dest)++"@"++ show stoptime) $ do
					inOL (OL.spliceOut (start timespan) (stop timespan)) -- splices out the interval
					writeCurrentTime "propagate1" (start timespan)
					reader >> return ()
				prop
	readTime <- readCurrentTimeExpr
--	now <- readCurrentTime
	prop
	inM readTime >>= writeCurrentTime "propagate2"
--	writeCurrentTime "propagate2" now

run :: Ref m r => Adaptive m r a -> m a
run (Ad m) = OL.run $ do 
   pq  <- newRef PQ.empty -- initialize priority queue
   ct  <- OL.base >>= newRef . return -- initialize current time
   m pq ct

inCh :: (Ref m r) => Changeable m r a -> Adaptive m r a
inCh (Ch f :: Changeable m r a) = do
	(x :: r a) <- newRef (error "inCh") -- we need an intermediate reference to get the value of @a@ out
	trace ("new middle ref " ++ show (refId x)) $ f $ \v -> do
		writeRef x v
		now <- readCurrentTime
		(_,nowtime,_) <- inM $ CL.val now
		trace ("new middle time " ++ show (refId x) ++ "@"++show nowtime) $ return (refId x)
	readRef x

instance (RefId r,EqRef r) => Eq (Modifiable m r a) where
   n1 == n2 = eqRef (value n1) (value n2)

instance (Ref m r) => NewMod (Changeable) m r where
  adIn = inAd
  chIn = id
  change = changeC
  changeMod = changeModC
  readMod = readModC
  newModBy cmp ch = inAd $ newModByA cmp ch

instance Ref m r => NewMod Adaptive m r where
 adIn = id
 chIn = inCh
 change = changeA
 changeMod = changeModA
 readMod = inCh . readModC
 newModBy = newModByA

newModByWithA :: (Show a,Ref m r) => (a -> a -> Bool) -> Changeable m r (a,b) -> Adaptive m r (Modifiable m r a,b)
newModByWithA cmp c = do
 m <- newRef (error "no value") -- value
 changeR <- newRef (error "changeR") -- writer
 es <- newRef [] -- outgoing edges
 lastmod <- newRef (error "lastmod")
 let writeFirst v = do -- write in ML code
       writeRef m v
       now <- stepTime
       (_,nowtime,_) <- inM $ CL.val now
       writeRef lastmod now
       trace ("writing first ref " ++ show (refId m) ++ " @ " ++ show nowtime) $ writeRef changeR writeAgain
     writeAgain v = do -- change in ML code. the argument @t@ is the creation time of the modifiable
       v' <- readRef m
       if (cmp v' v) then trace ("same value at " ++ show (refId m)) $ return ()
       else do -- if value has changed, add the out-edges of the target to the queue to propagate that change.
         trace ("writing again ref " ++ show (refId m) ++ " " ++ show v) $ writeRef m v -- change the updated value (to the same reference)
         readRef es >>= insertPQ -- add out-edges to the PQ
         writeRef es [] -- keep BX-related out-edges
       writeCurrentTimeExpr $ readRef lastmod
 writeRef changeR writeFirst -- define writer
 b <- inCh $ chgRet1 (refId m) $ do -- create the new modifiable
	(v,b) <- c -- run the changeable (adds read edges)
	run <- readRef changeR
	inAd $ run v -- write to the output modifiable
	return b
 return (Node m changeR es lastmod,b)

newModByA :: (Show a,Ref m r) => (a -> a -> Bool) -> Changeable m r a -> Adaptive m r (Modifiable m r a)
newModByA cmp c = liftM fst $ newModByWithA cmp (liftM (,()) c)

newModWithA :: (Show a,Eq a,Ref m r) => Changeable m r (a,b) -> Adaptive m r (Modifiable m r a,b)
newModWithA c = newModByWithA (==) c

newModWithC :: (Show a,Eq a,Ref m r) => Changeable m r (a,b) -> Changeable m r (Modifiable m r a,b)
newModWithC c = inAd $ newModWithA c

insertPQ :: Ref m r => [Recomp m r] -> Adaptive m r ()
insertPQ es = do
   pqR <- pqRef
   readRef pqR >>= ins es >>= writeRef pqR
  where
  ins []     pq = return pq
  ins (e:es) pq = PQ.insertM (\x y -> inOL $ 
                              OL.order (start $ timeSpan x) (start $ timeSpan y))
                             e pq >>= ins es

changeModWithC :: Ref m r => Modifiable m r a -> Changeable m r (a,b) -> Changeable m r (Modifiable m r a,b)
changeModWithC m c = inAd $ changeModWithA m c

changeModWithA :: Ref m r => Modifiable m r a -> Changeable m r (a,b) -> Adaptive m r (Modifiable m r a,b)
changeModWithA m@(Node r changeR es lastmod) c = inCh $ chgRet1 (refId r) $ c >>= \(x,s) -> changeC1 m x >> return (m,s)

changeModC :: (Ref m r) => Modifiable m r a -> Changeable m r a -> Changeable m r (Modifiable m r a)
changeModC m c = inAd $ changeModA m c

changeModA :: (Ref m r) => Modifiable m r a -> Changeable m r a -> Adaptive m r (Modifiable m r a)
changeModA m c = liftM fst $ changeModWithA m (liftM (,()) c)

-- we need to save the current time
-- changeMod is always used to change a modifiable on the other side, so it will issue propagation of the opposite transformation, that was logged at a different time.
changeC1 :: (Ref m r) => Modifiable m r a -> a -> Changeable m r ()
changeC1 (Node r changeR es lastmod) a = inAd $ do
	chg <- readRef changeR
	t <- stepTime
	(_,time,_) <- inM $ CL.val t
	writeRef lastmod t
	trace ("changingC1 " ++ show (refId r) ++"@"++ show time) $ chg a
	writeCurrentTimeExpr $ readRef lastmod

-- as in the original Adaptive
changeA :: Ref m r => Modifiable m r a -> a -> Adaptive m r ()
changeA (Node r changeR es lastmod) a = do
	chg <- readRef changeR
	t <- readCurrentTime
	(_,time,_) <- inM $ CL.val t
	trace ("changingC " ++ show (refId r) ++"@"++ show time) $ chg a

changeC :: (Ref m r) => Modifiable m r a -> a -> Changeable m r ()
changeC m a = inAd $ changeA m a


--	change :: Ref m r => Modifiable m r a -> a -> n m r ()
--	changeMod ::  (Eq a,Ref m r) => Modifiable m r a -> Changeable m r a -> n m r (Modifiable m r a)



