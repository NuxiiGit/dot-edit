module Graph (Graph,
        add, remove, neighbours,
        depthf) where
    import Relation
    
    -- |A type alias which describes graph information.
    type Graph a = Relation a

    -- |Adds a new edge to the graph.
    add :: Eq a => a -> a -> Graph a -> Graph a
    add a b r
        | (a, b) `notElem` r = (a, b) : r
        | otherwise = r

    -- |Removes a node from the graph.
    remove :: Eq a => a -> Graph a -> Graph a
    remove x = filter (\(a, b) -> a /= x && b /= x)

    -- |Returns the list of neighbours of this node.
    neighbours :: Eq a => a -> Graph a -> [a]
    neighbours x r = [b | (a, b) <- r, a == x]

    -- |Returns the depth-first traversal of this graph.
    depthf :: Eq a => a -> Graph a -> [a]
    depthf x r = search [x] [] where
        search [] visits = visits
        search (x : xs) visits = search frontier (visits ++ [x]) where
            frontier = xs ++ [x | x <- neighbours x r,
                    x `notElem` xs && x `notElem` visits]

    -- |Returns the best-first traversal of this graph.
    bestf :: Eq a => (a -> Int) -> a -> Graph a -> [a]
    bestf f x r = search [x] [] where
        search [] visits = visits
        search frontier visits = search frontier' visits' where
            i = bestIndex f frontier
            node = frontier !! i
            frontier' = (deleteAt i frontier) ++ [x | x <- neighbours node r,
                    x `notElem` frontier && x `notElem` visits]
            visits' = visits ++ [node]
        bestIndex f xs = i where
            weights = zip (map f xs) [0..]
            (_, i) = minimum weights
        deleteAt i xs = [x | (x, j) <- zip xs [0..], i /= j]

