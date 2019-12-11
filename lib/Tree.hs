-- |Suppies functions for generating and manipulating tree structures.
module Tree (Tree (..),
        treeify)
    where
    import Graph
    
    -- |A data structure which recursively defines a tree.
    data Tree a = Node Int a [Tree a]
            deriving Show

    -- |Converts the graph into a tree.
    treeify :: (Eq a) => Graph a -> a -> Tree a
    treeify r v = make r v 0
        where
        make r v depth = Node depth v [make r v' (succ depth) | v' <- neighbours r v]