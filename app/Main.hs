import Graph
import Dot

import System.IO
import System.Directory
import Control.Monad

main :: IO ()
main = do
    context <- readFile "resources/test.txt"
    let dir = "bin/graphs/"
    createDirectoryIfMissing True dir
    writeFile (dir ++ "graph.dot") context


numbers :: Graph Int
numbers = [(3, 1), (0, 2), (2, 4), (3, 4), (3, 0)]

cyclic :: Graph Int
cyclic = [(0, 1), (1, 2), (2, 3), (3, 4), (0, 3)]