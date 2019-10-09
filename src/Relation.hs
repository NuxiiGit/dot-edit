module Relation (Relation, symmetric, reflexive, subset, permutation) where

-- |A type alias which describes a relation information.
type Relation a = [(a, a)]

-- |Computes the symmetric closure for this graph.
symmetric :: Eq a => Relation a -> Relation a
symmetric r = r ++ [(y, x) | (x, y) <- r, (y, x) `notElem` r]

-- |Computes the reflexive closure for this graph.
reflexive :: Eq a => Relation a -> Relation a
reflexive r = r ++ [(x, x) | x <- xs, (x, x) `notElem` r] where
    xs = flatten r []
    flatten [] ys = ys
    flatten ((a, b) : xs) ys
      | a `notElem` ys && b `notElem` ys = flatten xs (a : b : ys)
      | a `notElem` ys = flatten xs (a : ys)
      | b `notElem` ys = flatten xs (b : ys)
      | otherwise = flatten xs ys

-- |Returns whether a relation is a subset of another.
subset :: Eq a => Relation a -> Relation a -> Bool
subset [] _ = True
subset (x : xs) ys = x `elem` ys && subset xs ys

-- |Returns whether a relation is a permutation of another.
permutation :: Eq a => Relation a -> Relation a -> Bool
permutation r s = r `subset` s && s `subset` r