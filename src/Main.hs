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
            let g' = modifyGraph (head args) g
            writeGraph dest g'
        _ -> putStrLn "please supply a source and destination path"

readGraph :: String -> IO DotGraph
readGraph path = do
    putStrLn $ "attempting to read DOT script from path: " ++ path
    context <- readFile path
    let g = decode context
    putStrLn $ "graph:\n" ++ show g
    return g

writeGraph :: String -> DotGraph -> IO ()
writeGraph path g = do
    putStrLn $ "writing to destination file '" ++ path ++ "'"
    createDirectoryIfMissing True $ dropFileName path
    writeFile path $ encode g

modifyGraph :: String -> DotGraph -> DotGraph
modifyGraph command g = case split ':' command of
    "symmetric" : _ -> symmetric g
    _ -> g

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split at xs = prefix : split at (if null xs' then [] else drop 1 xs')
    where
    (prefix, xs') = break (== at) xs
