
-- A naive priority queue implementation, with an insert operation
-- that uses a monadic comparison operation.

module Control.Monad.Adaptive.PriorityQueue(
  PriorityQueue,
  empty,
  insert,
  insertM,
  min
  ) where

import Prelude hiding(min)

import qualified Data.List(insert)
import Control.Monad(ap)

-- Export:
empty   :: PriorityQueue a
insert  :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertM :: Monad m => 
          (a -> a -> m Ordering) -> a -> PriorityQueue a -> m (PriorityQueue a)
min     :: PriorityQueue a -> Maybe (a, PriorityQueue a)

-- Local

type PriorityQueue a = [a]

empty = []

insert a l = (Data.List.insert a l)

insertM cmp a l = ins l
  where ins [] = return [a]
        ins (b:l) = do o <- cmp a b
                       case o of LT -> return (a:b:l)
                                 _  -> return (b:) `ap` ins l

min [] = Nothing
min (x:xs) = Just (x,xs)
