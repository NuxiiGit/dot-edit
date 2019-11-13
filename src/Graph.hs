-- |Suppies functions for generating and manipulating graph structures. Assumes all graphs have no duplicate elements.
module Graph (Graph, Edge,
        {-add, remove, neighbours,
        symmetric, reflexive, transitive,
        compose, transpose,-}
        domain, permutation, isSup, isSub,
        union, dunion, intersection, difference)
    where
    import Data.List (nub, concat)
    
    -- |A type alias which describes graph structure.
    type Graph a = [Edge a]

    -- |A type alias for transitions between two nodes.
    type Edge a = (a, a)

    -- |Computes the domain of this graph.
    domain :: (Eq a) => Graph a -> [a]
    domain r = (nub . concat) [[a, b] | (a, b) <- r]

    -- |Returns whether a graph is a permutation of another.
    permutation :: (Eq a) => Graph a -> Graph a -> Bool
    permutation r s = r `isSub` s && r `isSup` s

    -- |Returns whether a graph is a superset of another.
    isSup :: (Eq a) => Graph a -> Graph a -> Bool
    isSup r s = s `isSub` r

    -- |Returns whether a graph is a subset of another.
    isSub :: (Eq a) => Graph a -> Graph a -> Bool
    isSub r s = all (`elem` s) r

    -- |Computes the union (OR) of two graphs.
    union :: (Eq a) => Graph a -> Graph a -> Graph a
    union r s = dunion r s ++ intersection r s

    -- |Computes the symmetric difference (XOR) of two graphs.
    dunion :: (Eq a) => Graph a -> Graph a -> Graph a
    dunion r s = difference r s ++ difference s r

    -- |Computes the intersection (AND) of two graphs.
    intersection :: (Eq a) => Graph a -> Graph a -> Graph a
    intersection r s = [x | x <- r, x `elem` s]

    -- |Computes the set difference of two graphs.
    difference :: (Eq a) => Graph a -> Graph a -> Graph a
    difference r s = [x | x <- r, x `notElem` s]

    ---old---

    {-
    -- |Adds a new edge to the graph.
    add :: (Node a) => Graph a -> (a, a) -> Graph a
    add r n = r `union` [n]
    
    -- |Removes a node from the graph. Any connected edges are also removed.
    remove :: (Node a) => Graph a -> a -> Graph a
    remove r x = filter (\(a, b) -> a /= x && b /= x) r

    -- |Computes the list of neighbours of this node.
    neighbours :: (Node a) => Graph a -> a -> [a]
    neighbours r x = [b | (a, b) <- r, a == x]

    -- |Computes the symmetric closure of this graph.
    symmetric :: (Node a) => Graph a -> Graph a
    symmetric r = r `union` transpose r

    -- |Computes the reflexive closure of this graph.
    reflexive :: (Node a) => Graph a -> Graph a
    reflexive r = r `union` [(x, x) | x <- domain r]

    -- |Computes the transitive closure of this graph.
    transitive :: (Node a) => Graph a -> Graph a
    transitive r = if r `permutation` r'
        then r
        else transitive r'
        where 
        r' = r `union` (r `compose` r)

    -- |Computes the composition of two graphs.
    compose :: (Node a) => Graph a -> Graph a -> Graph a
    compose r s = [(a, c) | (a, b) <- r, (b', c) <- s, b == b']

    -- |Computes the transpose of this graph.
    transpose :: (Node a) => Graph a -> Graph a
    transpose r = [(y, x) | (x, y) <- r]-}