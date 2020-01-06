import Graph
import Dot

import System.IO
import System.Directory
import System.FilePath
import System.Environment
import Control.Monad

main :: IO ()
main = do
    source : _ <- getArgs
    putStrLn $ "reading from source file '" ++ source ++ "'"
    context <- readFile source
    let g = decode context
    let dir = "bin/graph/"
    let dest = dir ++ takeFileName source
    putStrLn $ "writing to destination file '" ++ dest ++ "'"
    createDirectoryIfMissing True dir
    writeFile dest $ encode g