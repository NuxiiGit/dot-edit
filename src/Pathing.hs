module Pathing (depthf) where
    import Node
    import Graph

    -- |Computes the depth-first traversal of this graph.
    depthf :: (Node a) => a -> Graph a -> [a]
    depthf x r = search [x] [] where
        search [] visits = visits
        search (x : xs) visits = search frontier (visits ++ [x]) where
            frontier = xs ++ [x | x <- neighbours x r,
                    x `notElem` xs && x `notElem` visits]