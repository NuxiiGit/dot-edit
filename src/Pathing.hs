module Pathing (depthf) where
    import Node
    import Graph
    import Data.List (sort)

    -- |Computes the depth-first traversal of this graph.
    depthf :: (Node a) => a -> Graph a -> [a]
    depthf x r = search [x] [] where
        search [] visits = visits
        search (x : xs) visits = if x `elem` visits
                then search xs visits
                else search frontier (visits ++ [x]) where
            frontier = sort (neighbours x r) ++ xs