{-# LANGUAGE UndecidableInstances, FlexibleContexts, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses #-}

module Control.Monad.Adaptive where

import Control.Monad.Ref
import Control.Monad.Adaptive.DataTypes
import qualified Control.Monad.Adaptive.CircularList as CL
import Control.Monad.Adaptive.MonadUtil
import Control.Monad.Adaptive.PriorityQueue (PriorityQueue)
import qualified Control.Monad.Adaptive.PriorityQueue as PQ
import Control.Monad.Adaptive.OrderedList (OrderedList)
import qualified Control.Monad.Adaptive.OrderedList as OL

import Debug.Trace

run :: Ref m r => Adaptive m r a -> m a
run (Ad m) = OL.run $ do 
	pq  <- OL.inM $ newRef PQ.empty -- initialize priority queue
	ct  <- OL.base >>= OL.inM . newRef . return -- initialize current time
	(_,x) <- m 0 pq ct
	return x

-- * Classes

class (InM n,InOL n,Monad (n m r), Ref m r) => NewMod n m r where
	adIn :: Adaptive m r a -> n m r a
	chIn :: Changeable m r a -> n m r a
	newMod :: Eq a => Changeable m r a -> n m r (Modifiable m r a)
	readMod :: Modifiable m r a -> n m r a
	
	-- | Bidirectional primitive
	changeMod ::  (Ref m r) => Modifiable m r a -> Changeable m r a -> n m r (Modifiable m r a)

instance (Ref m r) => NewMod (Changeable) m r where
	adIn = inAd
	chIn = id
	readMod = readModC
	newMod ch = inAd $ newModA ch
	changeMod = changeModC

instance (Ref m r) => NewMod Adaptive m r where
	adIn = id
	chIn = inCh
	readMod = inCh . readModC
	newMod = newModA
	changeMod = changeModA

change :: NewMod Adaptive m r => Modifiable m r a -> a -> Adaptive m r ()
change (Node r changeR es lastmod) a = do
	chg <- readRef changeR
	t <- readCurrentTime
	(_,time,_) <- inM $ CL.val t
	trace ("changingC " ++ show (refId r) ++"@"++ show time) $ chg a

propagate :: NewMod Adaptive m r => Adaptive m r ()
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

readModC :: (NewMod Changeable m r) => Modifiable m r a -> Changeable m r a
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


newModA :: (Eq a,NewMod Adaptive m r) => Changeable m r a -> Adaptive m r (Modifiable m r a)
newModA c = do
 m <- newUniqueRef (error "no value") -- value
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
       if (v' == v) then trace ("same value at " ++ show (refId m)) $ return ()
       else do -- if value has changed, add the out-edges of the target to the queue to propagate that change.
         trace ("writing again ref " ++ show (refId m) ++ " ") $ writeRef m v -- change the updated value (to the same reference)
         readRef es >>= insertPQ -- add out-edges to the PQ
         writeRef es [] -- keep BX-related out-edges
       writeCurrentTimeExpr $ readRef lastmod
 writeRef changeR writeFirst -- define writer
 inCh $ chgRet1 (refId m) $ do -- create the new modifiable
	v <- c -- run the changeable (adds read edges)
	run <- readRef changeR
	inAd $ run v -- write to the output modifiable
	return ()
 return (Node m changeR es lastmod)

changeModC :: NewMod Changeable m r => Modifiable m r a -> Changeable m r a -> Changeable m r (Modifiable m r a)
changeModC m c = inAd $ changeModA m c

changeModA :: NewMod Adaptive m r => Modifiable m r a -> Changeable m r a -> Adaptive m r (Modifiable m r a)
changeModA m@(Node r changeR es lastmod) c = inCh $ chgRet1 (refId r) $ c >>= \x -> changeC1 m x >> return m

-- * Auxiliary functions

insertPQ :: NewMod Adaptive m r => [Recomp m r] -> Adaptive m r ()
insertPQ es = do
   pqR <- pqRef
   inM (readRef pqR) >>= ins es >>= inM . writeRef pqR
  where
  ins []     pq = return pq
  ins (e:es) pq = PQ.insertM (\x y -> inOL $ 
                              OL.order (start $ timeSpan x) (start $ timeSpan y))
                             e pq >>= ins es

chgBind :: Ref m r => Adaptive m r () -> Changeable m r a -> Changeable m r a
chgBind a (Ch m) = Ch $ \f -> m (\x -> do { i <- f x; a ; return i })

chgRet1 :: Ref m r => Int -> Changeable m r a -> Changeable m r a
chgRet1 i (Ch m) = Ch $ \f -> m ((>> return i) . f)

chgRet2 :: Ref m r => Int -> Changeable m r a -> Changeable m r a
chgRet2 i (Ch m) = Ch $ \f -> m f >> return i

-- we need to save the current time
-- changeMod is always used to change a modifiable on the other side, so it will issue propagation of the opposite transformation, that was logged at a different time.
changeC1 :: (NewMod Changeable m r) => Modifiable m r a -> a -> Changeable m r ()
changeC1 (Node r changeR es lastmod) a = inAd $ do
	chg <- inM $ readRef changeR
	t <- stepTime
	(_,time,_) <- inM $ CL.val t
	inM $ writeRef lastmod t
	trace ("changingC1 " ++ show (refId r) ++"@"++ show time) $ chg a
	writeCurrentTimeExpr $ readRef lastmod
