module Pathing (depthf, breadthf, bestf, traversal) where
    import Node
    import Graph
    import Data.List (sort, find)

    -- |Predicate for computing the depth-first traversal of a graph.
    depthf :: (Node a) => [a] -> [a] -> [a]
    depthf = (++)

    -- |Predicate for computing the breadth-first traversal of a graph.
    breadthf :: (Node a) => [a] -> [a] -> [a]
    breadthf = (\xs ys -> ys ++ xs)
    
    -- |Predicate for computing best-first traversal of a graph.
    bestf :: (Node a) => (a -> Float) -> [a] -> [a] -> [a]
    bestf f = (\xs ys -> organise $ ys ++ xs) where
        organise = map (\(_, x) -> x) . sort . map (\x -> (f x, x))

    -- |Computes a traversal using this function to construct the frontier in the next step.
    traversal :: (Node a) => ([a] -> [a] -> [a]) -> Graph a -> a -> [a]
    traversal f r x = search [x] [] where
        search [] visits = visits
        search (x : xs) visits = if visited x
                then search xs visits
                else search frontier (visits ++ [x]) where
            frontier = f (sort $ neighbours r x) xs
            visited = (`elem` visits)

