module Graph (Graph,
        add, remove,
        symmetric, reflexive, transitive,
        compose, transpose,
        domain, permutation, supset, subset,
        union, dunion, intersection, difference, set) where

    -- |A type alias which describes relation information.
    type Graph a = [(a, a)]

    -- |Adds a new edge to the graph.
    add :: Eq a => Graph a -> a -> a -> Graph a
    add r a b
            | (a, b) `elem` r = r
            | otherwise = (a, b) : r

    -- |Removes a node from the graph.
    remove :: Eq a => Graph a -> a -> Graph a
    remove r x = filter (\(a, b) -> a /= x && b /= x) r

    -- |Computes the symmetric closure of this relation.
    symmetric :: Eq a => Graph a -> Graph a
    symmetric r = r `union` (transpose r)

    -- |Computes the reflexive closure of this relation.
    reflexive :: Eq a => Graph a -> Graph a
    reflexive r = r `union` [(x, x) | x <- domain r]

    -- |Computes the transitive closure of this relation.
    transitive :: Eq a => Graph a -> Graph a
    transitive r
            | r `permutation` r' = r
            | otherwise = transitive r' where 
        r' = r `union` (r `compose` r)

    -- |Computes the composition of two relations.
    compose :: Eq a => Graph a -> Graph a -> Graph a
    compose r s = [(x, z) | (x, y) <- r, (y', z) <- s, y == y']

    -- |Returns the transpose of this relation.
    transpose :: Eq a => Graph a -> Graph a
    transpose r = [(y, x) | (x, y) <- r]

    -- |Returns the domain of this relation.
    domain :: Eq a => Graph a -> [a]
    domain = flatten [] where
        flatten xs [] = xs
        flatten xs ((a, b) : ys)
                | a `notElem` xs && b `notElem` xs = flatten (a : b : xs) ys
                | a `notElem` xs = flatten (a : xs) ys
                | b `notElem` xs = flatten (b : xs) ys
                | otherwise = flatten xs ys

    -- |Returns whether a relation is a permutation of another.
    permutation :: Eq a => Graph a -> Graph a -> Bool
    permutation r s = r `subset` s && r `supset` s

    -- |Returns whether a relation is a subset of another.
    supset :: Eq a => Graph a -> Graph a -> Bool
    supset r s = s `subset` r

    -- |Returns whether a relation is a subset of another.
    subset :: Eq a => Graph a -> Graph a -> Bool
    subset [] _ = True
    subset (x : xs) s = x `elem` s && subset xs s

    -- |Returns the union (AND) of two relations.
    union :: Eq a => Graph a -> Graph a -> Graph a
    union r s = set $ r ++ s

    -- |Returns the disjunctive union (XOR) of two relations.
    dunion :: Eq a => Graph a -> Graph a -> Graph a
    dunion r s = set [x | x <- r `union` s, x `notElem` (r `intersection` s)]

    -- |Returns the intersection (OR) of two relations.
    intersection :: Eq a => Graph a -> Graph a -> Graph a
    intersection r s = set [x | x <- r, x `elem` s]

    -- |Returns the set difference of two relations.
    difference :: Eq a => Graph a -> Graph a -> Graph a
    difference r s = set [x | x <- r, x `notElem` s]

    -- |Removes duplicate elements from the relation.
    set :: Eq a => Graph a -> Graph a
    set = merge [] where
        merge xs [] = xs
        merge xs (y : ys)
                | y `notElem` xs = merge (y : xs) ys 
                | otherwise = merge xs ys