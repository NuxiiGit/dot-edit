module Pathing (depthf, breadthf) where
    import Node
    import Graph
    import Data.List (sort)

    -- |Computes the depth-first traversal of this graph.
    depthf :: (Node a) => a -> Graph a -> [a]
    depthf = traversal (++)

    -- |Computes the depth-first traversal of this graph.
    breadthf :: (Node a) => a -> Graph a -> [a]
    breadthf = traversal (\xs ys -> ys ++ xs)
    
    -- |Computes a traversal using this function to construct the frontier in the next step.
    traversal :: (Node a) => ([a] -> [a] -> [a]) -> a -> Graph a -> [a]
    traversal f x r = search [x] [] where
        search [] visits = visits
        search (x : xs) visits = if x `elem` visits
                then search xs visits
                else search frontier (visits ++ [x]) where
            frontier = f (sort $ neighbours x r) xs