-- |Supplies functions for traversing trees and finding the shortest paths between Weighted nodes.
module Pathing (module Pathing)
    where
    import Graph
    import Data.List (sortBy)

    -- |Prioritises edges of a graph.
    prioritise :: (Ord a) => Graph a -> Graph a
    prioritise = sortBy $ \x y -> compare (snd x) (snd y)

    -- |Computes the depth-first traversal of a graph.
    depthFirst :: (Ord a) => Graph a -> a -> Graph a
    depthFirst = traversal (\old new -> prioritise new ++ old)

    -- |Computes the breadth-first traversal of a graph.
    breadthFirst :: (Ord a) => Graph a -> a -> Graph a
    breadthFirst = traversal (\old new -> old ++ prioritise new)

    -- |Computes the best-first traversal of a graph.
    bestFirst :: (Ord a) => Graph a -> a -> Graph a
    bestFirst = traversal (\old new -> prioritise $ old ++ new)

    -- |Computes the traversal of a graph, where `f` details how to concatenate the current and new frontiers.
    traversal :: (Eq a) => (Graph a -> Graph a -> Graph a) -> Graph a -> a -> Graph a
    traversal f r root = search ([] `f` branches r root) []
        where
        search [] visits = reverse visits
        search ((e@(_, v)) : es) visits = if v == root || any ((== v) . snd) visits
            then search es visits
            else search frontier (e : visits)
            where
            frontier = es `f` branches r v
