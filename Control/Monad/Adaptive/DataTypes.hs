{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses #-}

module Control.Monad.Adaptive.DataTypes where

import Control.Monad.Ref
import qualified Control.Monad.Adaptive.CircularList as CL
import Control.Monad.Adaptive.MonadUtil
import Control.Monad.Adaptive.PriorityQueue (PriorityQueue)
import qualified Control.Monad.Adaptive.PriorityQueue as PQ
import Control.Monad.Adaptive.OrderedList (OrderedList)
import qualified Control.Monad.Adaptive.OrderedList as OL
import Control.Monad.Trans
import Control.Monad.Box
import Data.IORef

import Debug.Trace

data Recomp m r = Edge { ori :: Int, dest :: Int, reader :: Adaptive m r Int, timeSpan :: TimeSpan m r } -- edge in the DDG
-- source and destination implicit in the reader: it starts by reading the source, and ends by writing to the destination.

type TimeStamp m r = OL.Record m r () -- timestamp

-- | State of the computation (global variables in the ML code)
newtype Adaptive m r a = Ad { deAd :: Id -> r (PriorityQueue (Recomp m r)) -> r (m (TimeStamp m r)) -> OrderedList m r () (Id,a) }
-- args: priority queue of changed input edges, an expression the reads the current time (possibly from some other reference)
-- state: ordered list for managing timestamps
-- return: value

data Changeable m r a = Ch { deCh :: (a -> Adaptive m r Int) -> Adaptive m r Int } -- a continuation

data Modifiable m r a = Node { value :: IdRef r a, write :: r (a -> Adaptive m r ()), outEdges :: r [Recomp m r], lastModified :: r (TimeStamp m r) } -- node in the DDG

data TimeSpan m r = TimeSpan { start :: TimeStamp m r, stop :: TimeStamp m r }
type TimeSpans m r = [TimeSpan m r]

class InOL m' where
  inOL :: Ref m r => OrderedList m r () b -> m' m r b

instance InOL Adaptive where
  inOL m = Ad $ \count pq ct -> m >>= \x -> return (count,x)

instance InOL (Changeable) where
  inOL m = inAd (inOL m)

instance (EqRef r) => Eq (Modifiable m r a) where
   n1 == n2 = eqRef (value n1) (value n2)

instance InM (Changeable) where
  inM m = Ch $ \k -> (inM m >>= k)

instance InM Adaptive where
  inM m = Ad $ \count pq ct -> OL.inM m >>= \x -> return (count,x)

instance Ref m r => Ref (Adaptive m r) (IdRef r) where
	newRef v = error "newIdRef Adaptive"
	readRef (IdRef _ r) = inM $ readRef r
	writeRef (IdRef _ r) v = inM (writeRef r v)

instance Ref m r => Ref (Changeable m r) (IdRef r) where
	newRef v = error "newIdRef Changeable"
	readRef (IdRef _ r) = inM $ readRef r
	writeRef (IdRef _ r) v = inM (writeRef r v)

instance Ref m r => UniqueRef (Adaptive m r) (IdRef r) where
	newUniqueRef v = do
		r <- inM (newRef v)
		Ad $ \count pq time -> return (count+1,IdRef count r)

--	newUniqueRef v = do
--		IdRef _ r <- inM $ newRef v
--		Ch $ \k -> (Ad $ \count pq time -> return (succ count,IdRef count r)) >>= k

instance (Ref m r) => Monad (Changeable m r) where
  return a   = Ch $ \k -> k a
  Ch m >>= f = Ch $ \k -> m $ \a -> deCh (f a) k

instance (Ref m r) => Functor (Changeable m r) where
  fmap f m = m >>= return . f

instance Ref m r => Ref (Adaptive m r) r where
	newRef v = inM $ newRef v
	readRef x = inM $ readRef x
	writeRef r v = inM $ writeRef r v

instance Ref m r => Ref (Changeable m r) r where
	newRef v = inM $ newRef v
	readRef x = inM $ readRef x
	writeRef r v = inM $ writeRef r v

instance Ref m r => Monad (Adaptive m r) where
  return a   = Ad $ \count pq ct -> return (count,a)
  Ad m >>= f = Ad $ \count pq ct -> m count pq ct >>= \(count',a) -> deAd (f a) count' pq ct

instance Ref m r => Functor (Adaptive m r) where
  fmap f m = m >>= return . f

class InM l where
   inM :: Ref m r => m a -> l m r a

instance (Ref m r,MonadIO m) => MonadIO (Adaptive m r) where
	liftIO = inM . liftIO

instance (Ref m r,MonadIO m) => MonadIO (Changeable m r) where
	liftIO = inM . liftIO

inCh :: (Ref m r) => Changeable m r a -> Adaptive m r a
inCh (Ch f :: Changeable m r a) = do
	(x :: IdRef r a) <- newUniqueRef (error "inCh") -- we need an intermediate reference to get the value of @a@ out
	trace ("new middle ref " ++ show (refId x)) $ f $ \v -> do
		writeRef x v
		now <- readCurrentTime
		(_,nowtime,_) <- inM $ CL.val now
		trace ("new middle time " ++ show (refId x) ++ "@"++show nowtime) $ return (refId x)
	readRef x

inAd :: (Ref m r) => Adaptive m r a -> Changeable m r a
inAd m = Ch $ \k -> (m >>= k)

pqRef :: Ref m r => Adaptive m r (r (PriorityQueue (Recomp m r)))
pqRef = Ad $ \count pq ct -> return (count,pq)

readPq :: Ref m r => Adaptive m r (PriorityQueue (Recomp m r))
readPq = pqRef >>= inM . readRef
writePq a = pqRef >>= flip writeRef a

ctRef :: Ref m r => Adaptive m r (r (m (TimeStamp m r)))
ctRef = Ad $ \count pq ct -> return (count,ct)
readCurrentTimeExpr :: Ref m r => Adaptive m r (m (TimeStamp m r))
readCurrentTimeExpr = ctRef >>= inM . readRef
readCurrentTime :: Ref m r => Adaptive m r (TimeStamp m r)
readCurrentTime = readCurrentTimeExpr >>= inM

writeCurrentTimeExpr :: Ref m r => m (TimeStamp m r) -> Adaptive m r ()
writeCurrentTimeExpr m = ctRef >>= \r -> inM $ writeRef r m

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

instance (Ref m IORef,Box IORef m) => Box IORef (Changeable m IORef) where
	box v = inM $ box v
	unbox k = inM $ unbox k
	inbox k f = mapRefM f k


