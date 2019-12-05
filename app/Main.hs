import Graph
import Tree
import Pathing
import ShortestPath


main :: IO ()
main = do
    c <- getLine
    putStrLn $ c

instance Weighted Int
    where

numbers :: Graph Int
numbers = [(3, 1), (0, 2), (2, 4), (3, 4), (3, 0)]

cyclic :: Graph Int
cyclic = [(0, 1), (1, 2), (2, 3), (3, 4), (0, 3)]