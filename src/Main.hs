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
        source : dest : args -> do
            g <- readGraph source
            writeGraph dest g
        _ -> putStrLn "please supply a source and destination path"

readGraph :: String -> IO (Graph String)
readGraph path = do
    putStrLn $ "attempting to read DOT script from path: " ++ path
    context <- readFile path
    let g = decode context
    putStrLn $ "graph:\n" ++ show g
    return g

writeGraph :: String -> Graph String -> IO ()
writeGraph path g = do
    putStrLn $ "writing to destination file '" ++ path ++ "'"
    createDirectoryIfMissing True $ dropFileName path
    writeFile path $ encode g
