module Language.Solver.Worklist(Queue(..), queueToList) where

import Data.Sequence
import qualified Data.Sequence as Seq
import qualified Data.List as List


-- | Queue abstraction for different search strategies
class Queue q where
  emptyQueue :: q a
  enqueue :: a -> q a -> q a
  dequeue :: q a -> Maybe (a, q a)

-- | Returns the elements of the queue in the order of dequeue
queueToList :: Queue q => q a -> [a]
queueToList = List.unfoldr dequeue

-- | List-based stack (LIFO - depth-first search)
instance Queue [] where
  emptyQueue = []
  enqueue x xs = x : xs
  dequeue [] = Nothing
  dequeue (x : xs) = Just (x, xs)

-- | Seq-based FIFO queue (breadth-first search)
instance Queue Seq where
  emptyQueue = Seq.empty
  enqueue x q = q Seq.|> x
  dequeue q = case Seq.viewl q of
    Seq.EmptyL -> Nothing
    x Seq.:< rest -> Just (x, rest)

