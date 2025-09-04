{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Graph
  ( -- * Graph data types
    Graph(..)
  , UnlabeledGraph
    -- * Graph construction
  , empty
  , addEdge
  , addNode
    -- * Graph queries
  , neighbours
    -- * Graph algorithms
  , isReachable
  , topSort
  , hasCycle
  ) where

import Control.Monad.State.Class
import Control.Monad.Error.Class
import Control.Monad.Extra
import Control.Monad.State
import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isNothing)

------------------------------------------------------------
-- Graph data type
------------------------------------------------------------

-- | Adjecency-list based directed graph
newtype Graph k e = Graph { getAdjList :: Map k (Set (k, e)) }
                  deriving (Ord, Eq, Show)

-- | A graph without labels on its edges
type UnlabeledGraph k = Graph k ()

-- | Create an empty graph
empty :: Graph k e
empty = Graph Map.empty

-- | Add an edge to the graph
addEdge :: (Ord k, Ord e) => e -> k -> k -> Graph k e -> Graph k e
addEdge lbl from to =
  Graph . Map.insertWith Set.union from (Set.singleton (to, lbl)) . getAdjList

-- | Adds the given node to the graph
addNode :: (Ord k, Ord e) => k -> Graph k e -> Graph k e
addNode n = Graph . Map.insertWith Set.union n Set.empty . getAdjList

-- | Get the neighbours of the given node
neighbours :: (Ord k, Ord e) => k -> Graph k e -> Set (k, e)
neighbours from = fromMaybe Set.empty . Map.lookup from . getAdjList


------------------------------------------------------------
-- Basic graph algorithms
------------------------------------------------------------

-- | Escape value for exiting traversal functions early
newtype Escape v = Escape { getResult :: v }

-- | Basic monad for traversing graphs containing a visited set
type MonadTraverse k v m = (MonadState (Set k) m, MonadError (Escape v) m)

-- | Performs a DFT for checking whether the second argument is reachable from the first argument
isReachable' :: (Ord k, Ord e, MonadTraverse k Bool m )=> k -> k -> Graph k e -> m ()
isReachable' from to g = go from
  where
    go current = do
      ifM (gets (Set.member current))
        (return ())
        (do modify (Set.insert current)
            if current == to
              then throwError (Escape True)
              else mapM_ (go . fst) (neighbours current g))

-- | Check if the second argument is reachable from the first argument
isReachable :: (Ord k, Ord e) => k -> k -> Graph k e -> Bool
isReachable from to g =
  either getResult (const False) $
    evalState (runExceptT (isReachable' from to g)) Set.empty

-- | Topological sort using Kahn's algorithm 
-- Returns Nothing if the graph has cycles, otherwise returns topologically sorted nodes
topSort :: (Ord k, Ord e) => Graph k e -> Maybe [k]
topSort g =
  let allNodes = Map.keys (getAdjList g)
      inDegree = computeInDegrees g allNodes
      zeroInDegree = filter (\n -> Map.findWithDefault 0 n inDegree == 0) allNodes
  in kahnSort g inDegree zeroInDegree []

-- | Compute in-degrees for all nodes
computeInDegrees :: (Ord k, Ord e) => Graph k e -> [k] -> Map k Int
computeInDegrees g nodes =
  let initialDegrees = Map.fromList [(n, 0) | n <- nodes]
  in foldl addIncomingEdges initialDegrees nodes
  where
    addIncomingEdges degrees node =
      let outgoing = Set.map fst $ neighbours node g
      in Set.foldl (\acc target -> Map.insertWith (+) target 1 acc) degrees outgoing

-- | Kahn's algorithm implementation
kahnSort :: (Ord k, Ord e) => Graph k e -> Map k Int -> [k] -> [k] -> Maybe [k]
kahnSort g inDegrees queue result
  | null queue =
      if length result == Map.size (getAdjList g)
      then Just (reverse result)
      else Nothing  -- Cycle detected, since not all nodes had in-degree zero
  | otherwise =
      case queue of
        [] -> error "impossible case - queue is not null but pattern match failed"
        (node:restQueue) ->
          let outgoing = Set.map fst $ neighbours node g
              newInDegrees = Set.foldl (flip (Map.adjust (\x -> x - 1))) inDegrees outgoing
              newZeroNodes = Set.filter (\n -> Map.findWithDefault 0 n newInDegrees == 0) outgoing
              newQueue = restQueue ++ Set.toList newZeroNodes
          in kahnSort g newInDegrees newQueue (node : result)

-- | Check if the graph has cycles
hasCycle :: (Ord k, Ord e) => Graph k e -> Bool
hasCycle g = isNothing (topSort g)
