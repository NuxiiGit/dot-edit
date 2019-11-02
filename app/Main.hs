import Node
import Graph
import Pathing

main :: IO ()
main = do
    c <- getLine
    putStrLn $ c

instance Node Int where

numbers :: Graph Int
numbers = [(1, 4), (3, 1), (3, 2), (3, 4)]

cyclic :: Graph Int
cyclic = [(3, 1), (1, 2), (2, 4), (3, 4)]