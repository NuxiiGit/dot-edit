module Graph (Graph,
        add, remove) where
    import Relation
    
    -- |A type alias which describes graph information.
    type Graph a = Relation a

    -- |Adds a new edge to the graph.
    add :: Eq a => Graph a -> a -> a -> Graph a
    add r a b = set $ (a, b) : r

    -- |Removes a node from the graph.
    remove :: Eq a => Graph a -> a -> Graph a
    remove r x = filter (\(a, b) -> a /= x && b /= x) r