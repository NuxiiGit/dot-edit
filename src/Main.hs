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
        source : args -> do
            sourceIsFile <- doesFileExist source
            destExists <- (not . null $ args) && doesFileExist (head args)
            let modifiers = if destExists
                then tail args
                else args
            g <- readGraph source
            let g' = foldl modifyGraph g modifiers
            putStrLn $ encode g'
            putStrLn $ show destExists
            --writeGraph dest $! g'
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
    "antisymmetric" : [] -> antisymmetric g
    "reflexive" : [] -> reflexive g
    "transitive" : [] -> transitive g
    "transpose" : [] -> transpose g
    "add-edge" : a : b : [] -> add g (a, b)
    "del-edge" : a : b : [] -> remove g (a, b)
    "del-node" : v : [] -> deleteNode g v
    x -> error $ "invalid graph modifier - " ++ show command ++ " (" ++ show x ++ ")"

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split at xs = prefix : split at (if null xs' then [] else drop 1 xs')
    where
    (prefix, xs') = break (== at) xs
