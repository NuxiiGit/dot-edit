import Relation
import Graph

main :: IO ()
main = do
    c <- getLine
    putStrLn $ c ++ ":"
    putStrLn $ show letters

letters :: Graph String
letters = [("A", "B"), ("B", "C"), ("C", "D")]