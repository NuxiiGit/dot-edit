-- |Supplies functions for traversing trees and finding the shortest paths between Weighted nodes.
module Pathing (module Pathing)
    where
    import Graph
    import Data.List (sortBy)

    -- |Computes the best-first traversal of a graph, using `f` to sort the frontier each step.
    bestFirst :: (Eq a) => (a -> a -> Ordering) -> Graph a -> a -> [a]
    bestFirst order r root = search [root] []
        where
        search [] visits = visits
        search (v : vs) visits = if v `elem` visits
            then search vs visits
            else search frontier (v : visits)
            where
            frontier = sortBy order (vs ++ neighbours r v)

{-
    -- |Predicate for computing the depth-first traversal of a tree.
    depthf :: (Ord a) => [Tree a] -> [Tree a]
    depthf = sortBy order
        where
        order (Node d v _) (Node d' v' _)
            | d < d' = GT
            | d > d' = LT
            | v < v' = LT
            | v > v' = GT
            | otherwise = EQ

    -- |Predicate for computing the breadth-first traversal of a tree.
    breadthf :: (Ord a) => [Tree a] -> [Tree a]
    breadthf = sortBy order
        where
        order (Node d v _) (Node d' v' _)
            | d < d' = LT
            | d > d' = GT
            | v < v' = LT
            | v > v' = GT
            | otherwise = EQ

    -- |Computes the best-first traversal using `f` to sort the frontier each step.
    traversal :: (Eq a) => ([Tree a] -> [Tree a]) -> Tree a -> [a]
    traversal f t = reverse $ search [t] []
        where
        search [] visits = visits
        search (Node _ v neighbours : ts) visits = if visited v
            then search ts visits
            else search frontier $ v : visits
            where
            frontier = f $ filter (\(Node _ v _) -> not . visited $ v) neighbours ++ ts
            visited = (`elem` visits)
-}
