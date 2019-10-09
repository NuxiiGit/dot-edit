module Graph (Graph,
        add, remove,
        symmetric, reflexive, compose, transitive,
        domain, permutation, subset) where

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
symmetric r = r ++ [(y, x) | (x, y) <- r, (y, x) `notElem` r]

-- |Computes the reflexive closure of this relation.
reflexive :: Eq a => Graph a -> Graph a
reflexive r = r ++ [(x, x) | x <- domain r, (x, x) `notElem` r]

-- |Computes the composition of two relations.
compose :: Eq a => Graph a -> Graph a -> Graph a
compose r s = [(x, w) | (x, y) <- r, (z, w) <- s, y == z]

-- |Computes the transitive closure of this relation.
transitive :: Eq a => Graph a -> Graph a
transitive r = undefined

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
permutation r s = r `subset` s && s `subset` r

-- |Returns whether a relation is a subset of another.
subset :: Eq a => Graph a -> Graph a -> Bool
subset [] _ = True
subset (x : xs) s = x `elem` s && subset xs s