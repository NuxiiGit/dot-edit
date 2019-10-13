module Graph (Graph,
        add, remove, neighbours) where
    import Relation
    
    -- |A type alias which describes graph information.
    type Graph a = Relation a

    -- |Adds a new edge to the graph.
    add :: Eq a => Graph a -> a -> a -> Graph a
    add r a b
        | (a, b) `notElem` r = (a, b) : r
        | otherwise = r

    -- |Removes a node from the graph.
    remove :: Eq a => Graph a -> a -> Graph a
    remove r x = filter (\(a, b) -> a /= x && b /= x) r

    -- |Returns the list of neighbours of this node.
    neighbours :: Ord a => Graph a -> a -> [a]
    neighbours r x = [b | (a, b) <- r, a == x]