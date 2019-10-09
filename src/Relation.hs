module Relation (Relation, symmetric) where

-- |A type alias which describes a relation information.
type Relation a = [(a, a)]

-- |Computes the symmetric closure for this graph.
symmetric :: Eq a => Relation a -> Relation a
symmetric r = r ++ [(y, x) | (x, y) <- r, not ((y, x) `elem` r)]