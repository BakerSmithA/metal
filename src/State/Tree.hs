module State.Tree
( Tree
, dfFlattenTree
) where

import Data.Set as Set

-- Mapping from a node to nodes branching from this node, and the data stored
-- at this node in the tree.
type Tree m a b = (a -> m ([a], b))

-- Used to keep track of nodes that have been visited, and hence whether there
-- are cycles.
type VisitedNodes a = Set a

-- Uses Depth First Search to flatten all the data contained in the tree.
dfFlattenTree :: (Monad m, Eq a, Ord a) => Tree m a b -> [a] -> m [b]
dfFlattenTree tree start = fmap fst (dfFlattenTreeCycle Set.empty tree start)

-- Flattens the tree using DFS whilst checking for cycles. If cycles are
-- encountered the nodes will not be visited again.
dfFlattenTreeCycle :: (Monad m, Eq a, Ord a) => VisitedNodes a -> Tree m a b -> [a] -> m ([b], VisitedNodes a)
dfFlattenTreeCycle visited _ [] = return ([], visited)
dfFlattenTreeCycle visited tree (node:restPaths) = do
    if node `elem` visited then do
        dfFlattenTreeCycle visited tree restPaths
    else do
        let visited' = Set.insert node visited

        (branches, nodeData) <- tree node
        (childrenData, childrenVisited) <- dfFlattenTreeCycle visited' tree branches
        (restData, restVisited) <- dfFlattenTreeCycle childrenVisited tree restPaths
        let allData = childrenData ++ [nodeData] ++ restData

        return (allData, restVisited)
