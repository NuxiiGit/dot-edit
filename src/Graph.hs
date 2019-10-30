module Graph (Graph,
        add, remove, neighbours,
        symmetric, reflexive, transitive,
        compose, transpose,
        domain, permutation, supset, subset,
        union, dunion, intersection, difference, set) where
    import Node
    
    -- |A type alias which describes graph structure.
    type Graph a = [(a, a)]

    -- |Adds a new edge to the graph.
    add :: (Node a) => (a, a) -> Graph a -> Graph a
    add n = union [n]
    
    -- |Removes a node from the graph.
    -- Any connected edges are also removed.
    remove :: (Node a) => a -> Graph a -> Graph a
    remove x = filter (\(a, b) -> a /= x && b /= x)

    -- |Computes the list of neighbours of this node.
    neighbours :: (Node a) => a -> Graph a -> [a]
    neighbours x r = [b | (a, b) <- r, a == x]

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
            else transitive r' where 
        r' = r `union` (r `compose` r)

    -- |Computes the composition of two graphs.
    compose :: (Node a) => Graph a -> Graph a -> Graph a
    compose r s = [(a, c) | (a, b) <- r, (b', c) <- s, b == b']

    -- |Computes the transpose of this graph.
    transpose :: (Node a) => Graph a -> Graph a
    transpose r = [(y, x) | (x, y) <- r]

    -- |Computes the domain of this graph.
    domain :: (Node a) => Graph a -> [a]
    domain = flatten [] where
        flatten xs [] = xs
        flatten xs ((a, b) : ys) = flatten abxs ys where
            bxs = if b `notElem` xs
                    then b : xs
                    else xs
            abxs = if a `notElem` bxs
                    then a : bxs
                    else bxs

    -- |Returns whether a graph is a permutation of another.
    permutation :: (Node a) => Graph a -> Graph a -> Bool
    permutation r s = r `subset` s && r `supset` s

    -- |Returns whether a graph is a superset of another.
    supset :: (Node a) => Graph a -> Graph a -> Bool
    supset r s = s `subset` r

    -- |Returns whether a graph is a subset of another.
    subset :: (Node a) => Graph a -> Graph a -> Bool
    subset [] _ = True
    subset (x : xs) s = x `elem` s && subset xs s

    -- |Computes the union (OR) of two graphs.
    union :: (Node a) => Graph a -> Graph a -> Graph a
    union r s = set $ r ++ s

    -- |Computes the symmetric difference (XOR) of two graphs.
    dunion :: (Node a) => Graph a -> Graph a -> Graph a
    dunion r s = set [x | x <- r `union` s, x `notElem` (r `intersection` s)]

    -- |Computes the intersection (AND) of two graphs.
    intersection :: (Node a) => Graph a -> Graph a -> Graph a
    intersection r s = set [x | x <- r, x `elem` s]

    -- |Computes the set difference of two graphs.
    difference :: (Node a) => Graph a -> Graph a -> Graph a
    difference r s = set [x | x <- r, x `notElem` s]

    -- |Removes duplicate elements from the graph.
    set :: (Node a) => Graph a -> Graph a
    set = merge [] where
        merge xs [] = xs
        merge xs (y : ys) = merge yxs ys where
            yxs = if y `notElem` xs
                    then (y : xs)
                    else xs