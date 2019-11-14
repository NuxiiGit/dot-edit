-- |Supplies a predicate for finding the shortest path between two weighted nodes on a graph.
module ShortestPath (Weighted,
        distance,
        dijkstra's)
    where
    import Pathing

    -- |A class which is used to detail weighted edge information.
    class Weighted a
        where
        distance :: a -> a -> Float
        distance _ _ = 1.0
    
    -- |Predicate for computing the A* traversal of a graph. A.k.a. best-first traversal with a heuristic.
    dijkstra's :: (Ord a, Weighted a) => [Trans a] -> [Trans a] -> [Trans a]
    dijkstra's = bestf weight
        where
        weight (Nothing, _) = 0.0
        weight (Just parent, x) = parent `distance` x