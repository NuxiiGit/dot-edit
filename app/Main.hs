import Relation
import Graph

main :: IO ()
main = do
    c <- getLine
    putStrLn $ c ++ ":"
    putStrLn $ show letters

letters :: Graph String
letters = [("A", "B"), ("B", "C"), ("C", "D")]

numbers :: Graph Int
numbers = [(1, 2), (1, 3), (3, 2), (3, 4)]