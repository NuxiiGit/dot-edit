module Node (Node) where

    -- |A class which is used to detail node information.
    class (Ord a) => Node a where
        distance :: a -> a -> Float
        distance _ _ = 1.0