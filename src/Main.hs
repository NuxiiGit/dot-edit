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
            context <- if sourceIsFile
                then readFile source
                else return source
            let g = foldl modifyGraph (decode context) args
            putStrLn $ encode g
            --writeGraph dest $! g
        _ -> putStrLn "please supply a source and destination path"

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
