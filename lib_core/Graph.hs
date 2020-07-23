-- |Suppies functions for generating and manipulating graph structures. Assumes all graphs have no duplicate elements.
module Graph (module Graph)
    where
    import Data.List (nub, concat)

    -- |A type alias which describes graph structure.
    type Graph a = [Edge a]

    -- |A type alias for transitions between two nodes.
    type Edge a = (a, a)

    -- |Generates a graph from a path.
    graphify :: [a] -> Graph a
    graphify (x : xs@(x' : _)) = (x, x') : graphify xs
    graphify _ = []

    -- |Adds a new edge to the graph.
    add :: (Eq a) => Graph a -> Edge a -> Graph a
    add r e = union r [e]

    -- |Deletes an edge from the graph.
    remove :: (Eq a) => Graph a -> Edge a -> Graph a
    remove r e = [e' | e' <- r, e' /= e]

    -- |Removes a node from the graph. Any connected edges are also removed.
    deleteNode :: (Eq a) => Graph a -> a -> Graph a
    deleteNode r v = [(u, w) | (u, w) <- r, u /= v && w /= v]

    -- |Computes a list of edges for this node.
    branches :: (Eq a) => Graph a -> a -> [Edge a]
    branches r v = map (\x -> (v, x)) (neighbours r v)

    -- |Computes the list of neighbours of this node.
    neighbours :: (Eq a) => Graph a -> a -> [a]
    neighbours r v = [w | (u, w) <- r, u == v]

    -- |Returns `True` if this graph is directed.
    isDirected :: (Eq a) => Graph a -> Bool
    isDirected r = not $ permutation r $ transpose r

    -- |Computes the left-most antisymmetric closure of this graph.
    antisymmetric :: (Eq a) => Graph a -> Graph a
    antisymmetric r = accumulate r []
        where
        accumulate [] s = s
        accumulate (x@(a, b) : xs) s = if (b, a) `notElem` s
            then accumulate xs (s ++ [x])
            else accumulate xs s

    -- |Computes the symmetric closure of this graph.
    symmetric :: (Eq a) => Graph a -> Graph a
    symmetric r = union r $ transpose r

    -- |Computes the reflexive closure of this graph.
    reflexive :: (Eq a) => Graph a -> Graph a
    reflexive r = union r [(x, x) | x <- domain r]

    -- |Computes the transitive closure of this graph.
    transitive :: (Eq a) => Graph a -> Graph a
    transitive r = if permutation r' r
        then r
        else transitive r'
        where 
        r' = union r $ compose r r

    -- |Computes the composition of two graphs.
    compose :: (Eq a) => Graph a -> Graph a -> Graph a
    compose r s = [(v, w) | (v, u) <- r, (u', w) <- s, u == u']

    -- |Computes the transpose of this graph.
    transpose :: Graph a -> Graph a
    transpose r = [(u, v) | (v, u) <- r]

    -- |Computes the domain of this graph.
    domain :: (Eq a) => Graph a -> [a]
    domain r = nub . concat $ [[v, u] | (v, u) <- r]

    -- |Returns whether a graph is a permutation of another.
    permutation :: (Eq a) => Graph a -> Graph a -> Bool
    permutation r s = isSub r s && isSup r s

    -- |Returns whether a graph is a superset of another.
    isSup :: (Eq a) => Graph a -> Graph a -> Bool
    isSup r s = isSub s r

    -- |Returns whether a graph is a subset of another.
    isSub :: (Eq a) => Graph a -> Graph a -> Bool
    isSub r s = all (`elem` s) r

    -- |Computes the symmetric difference (XOR) of two graphs.
    dunion :: (Eq a) => Graph a -> Graph a -> Graph a
    dunion r s = difference (union r s) (intersection r s)

    -- |Computes the union (OR) of two graphs.
    union :: (Eq a) => Graph a -> Graph a -> Graph a
    union r s = difference r s ++ s

    -- |Computes the intersection (AND) of two graphs.
    intersection :: (Eq a) => Graph a -> Graph a -> Graph a
    intersection r s = [x | x <- r, x `elem` s]

    -- |Computes the set difference of two graphs.
    difference :: (Eq a) => Graph a -> Graph a -> Graph a
    difference r s = [x | x <- r, x `notElem` s]
