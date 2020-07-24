import Graph
import Dot
import Pathing

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
            let g = unwrap $ decode context
            putStrLn $ encode $ foldl modifyGraph g args
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
    "depthf" : v : [] -> depthFirst g v
    "breadthf" : v : [] -> breadthFirst g v
    "bestf" : v : [] -> bestFirst g v
    "dunion" : s : [] -> let h = unwrap $ decode s in dunion g h
    "union" : s : [] -> let h = unwrap $ decode s in union g h
    "intersection" : s : [] -> let h = unwrap $ decode s in intersection g h
    "difference" : s : [] -> let h = unwrap $ decode s in difference g h
    "compose" : s : [] -> let h = unwrap $ decode s in compose g h
    x -> error $ "invalid graph modifier - " ++ show command ++ " (" ++ show x ++ ")"

unwrap :: Maybe DotGraph -> DotGraph
unwrap (Just g) = g
unwrap Nothing = error "unable to parse dot graph"

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split at xs = prefix : split at (if null xs' then [] else drop 1 xs')
    where
    (prefix, xs') = break (== at) xs
