import Node
import Graph
import Pathing

main :: IO ()
main = do
    c <- getLine
    putStrLn $ c

instance Node Int where

numbers :: Graph Int
numbers = [(3, 1), (1, 2), (2, 4), (3, 4), (3, 2)]

cyclic :: Graph Int
cyclic = [(0, 1), (1, 2), (2, 3), (3, 4), (0, 3)]