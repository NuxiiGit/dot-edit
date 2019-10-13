module Graph (Graph,
        add, remove, neighbours) where
    import Relation
    
    -- |A type alias which describes graph information.
    type Graph a = Relation a

    -- |Adds a new edge to the graph.
    add :: Eq a => a -> a -> Graph a -> -> Graph a
    add a b r
        | (a, b) `notElem` r = (a, b) : r
        | otherwise = r

    -- |Removes a node from the graph.
    remove :: Eq a => a -> Graph a -> Graph a
    remove x = filter (\(a, b) -> a /= x && b /= x)

    -- |Returns the list of neighbours of this node.
    neighbours :: Eq a => a -> Graph a -> [a]
    neighbours x r = [b | (a, b) <- r, a == x]