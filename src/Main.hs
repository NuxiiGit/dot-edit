import Graph
import Dot
import Pathing

import System.IO
import System.Directory
import System.FilePath
import System.Environment

import Control.Monad

import Data.Maybe (isJust)

main :: IO ()
main = do
    args <- getArgs
    case args of
        source : args -> do
            sourceIsFile <- doesFileExist source
            context <- if sourceIsFile
                then readFile source
                else return source
            let mg = decode context
            let args' = if isJust mg
                then args
                else source : args
            putStrLn $ encode $ foldl modifyGraph (unwrapOr [] mg) args'
        _ -> do
            putStrLn "usage:"
            putStrLn "  dot-edit [filepath or graph literal] [options]"

modifyGraph :: DotGraph -> String -> DotGraph
modifyGraph g command = case split ':' command of
    "symmetric" : [] -> symmetric g
    "antisymmetric" : [] -> antisymmetric g
    "reflexive" : [] -> reflexive g
    "transitive" : [] -> transitive g
    "transpose" : [] -> transposeGraph g
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
    "equivalence" : [] -> transitive $ reflexive $ symmetric g
    "order" : [] -> transitive $ antisymmetric g
    x -> error $ "invalid graph modifier - " ++ show command ++ " (" ++ show x ++ ")"

unwrap :: Maybe DotGraph -> DotGraph
unwrap = unwrapOr (error "unable to parse dot graph")

unwrapOr :: DotGraph -> Maybe DotGraph -> DotGraph
unwrapOr _ (Just g) = g
unwrapOr fail Nothing = fail

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split at xs = prefix : split at (if null xs' then [] else drop 1 xs')
    where
    (prefix, xs') = break (== at) xs
