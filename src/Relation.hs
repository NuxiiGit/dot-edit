module Relation (Relation, symmetric, reflexive) where

-- |A type alias which describes a relation information.
type Relation a = [(a, a)]

-- |Computes the symmetric closure for this graph.
symmetric :: Eq a => Relation a -> Relation a
symmetric r = r ++ [(y, x) | (x, y) <- r, (y, x) `notElem` r]

-- |Computes the reflexive closure for this graph.
reflexive :: Eq a => Relation a -> Relation a
reflexive r = r
        ++ [(x, x) | (x, _) <- r, (x, x) `notElem` r]
        ++ [(y, y) | (_, y) <- r, (y, y) `notElem` r]