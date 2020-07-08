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
            let g' = foldl modifyGraph g args
            writeGraph dest $! g'
        _ -> putStrLn "please supply a source and destination path"

readGraph :: String -> IO DotGraph
readGraph path = do
    putStrLn $ "attempting to read DOT script from path - " ++ show path
    context <- readFile path
    return $ decode context

writeGraph :: String -> DotGraph -> IO ()
writeGraph path g = do
    putStrLn $ "writing to destination file - " ++ show path
    createDirectoryIfMissing True $ dropFileName path
    writeFile path $ encode g

modifyGraph :: DotGraph -> String -> DotGraph
modifyGraph g command = case split ':' command of
    "symmetric" : [] -> symmetric g
    "reflexive" : [] -> reflexive g
    _ -> error $ "invalid graph modifier - " ++ show command

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split at xs = prefix : split at (if null xs' then [] else drop 1 xs')
    where
    (prefix, xs') = break (== at) xs
