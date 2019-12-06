import Graph
import Tree
import Pathing

main :: IO ()
main = do
    c <- getLine
    putStrLn $ c

numbers :: Graph Int
numbers = [(3, 1), (0, 2), (2, 4), (3, 4), (3, 0)]

cyclic :: Graph Int
cyclic = [(0, 1), (1, 2), (2, 3), (3, 4), (0, 3)]