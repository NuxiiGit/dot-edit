module Pathing (Trans,
        depthf, breadthf, bestf,
        traversal, path) where
    import Node
    import Graph
    import Data.List (sort, find)

    -- |A type alias for transitions between two elements.
    type Trans a = (Maybe a, a)

    -- |Predicate for computing the depth-first traversal of a graph.
    depthf :: (Node a) => [Trans a] -> [Trans a] -> [Trans a]
    depthf = (++)

    -- |Predicate for computing the breadth-first traversal of a graph.
    breadthf :: (Node a) => [Trans a] -> [Trans a] -> [Trans a]
    breadthf = (\xs ys -> ys ++ xs)
    
    -- |Predicate for computing best-first traversal of a graph.
    bestf :: (Node a) => (Trans a -> Float) -> [Trans a] -> [Trans a] -> [Trans a]
    bestf f = (\xs ys -> organise $ ys ++ xs) where
        organise = map (\(_, x) -> x) . sort . map (\x -> (f x, x))

    -- |Computes a traversal using `f` to construct the frontier in the next step.
    traversal :: (Node a) => ([Trans a] -> [Trans a] -> [Trans a]) -> Graph a -> a -> [a]
    traversal f r x = search [(Nothing, x)] [] where
        search [] visits = visits
        search ((_, x) : xs) visits = if visited x
                then search xs visits
                else search frontier (visits ++ [x]) where
            frontier = f (map (\child -> (Just x, child)) (sort $ neighbours r x)) xs
            visited = (`elem` visits)

    -- |Computes the first path of a traversal using `f` to construct the frontier in the next step.
    path :: (Node a) => ([Trans a] -> [Trans a] -> [Trans a]) -> Graph a -> a -> a -> [a]
    path f r x t = search [(Nothing, x)] [] where
        search [] _ = []
        search ((parent, x) : xs) visits
                | visited x = search xs visits
                | x == t = reverse (x : walk parent)
                | otherwise = search frontier ((parent, x) : visits) where
            frontier = f (map (\child -> (Just x, child)) (sort $ neighbours r x)) xs
            visited x = any (\(_, child) -> child == x) visits
            walk Nothing = []
            walk (Just x) = x : walk parent where
                parent = case find (\(_, child) -> child == x) visits of
                        Nothing -> Nothing
                        Just (parent, _) -> parent