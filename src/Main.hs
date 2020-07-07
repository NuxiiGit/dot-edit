import Graph
import Dot

import System.IO
import System.Directory
import System.FilePath
import System.Environment
import Control.Monad

main :: IO ()
main = do
    args <- getArgs
    case args of
        source : _ -> do
            putStrLn $ "reading from source file '" ++ source ++ "'"
            context <- readFile source
            let g = decode context
            putStrLn $ "graph:\n" ++ show g
            let dir = "bin/graph/"
            let dest = dir ++ takeFileName source
            putStrLn $ "writing to destination file '" ++ dest ++ "'"
            createDirectoryIfMissing True dir
            writeFile dest $ encode g
        [] -> putStrLn "please supply a source file"
