-- |Suppies functions for importing and exporting DOT scripts.
module Dot (module Dot)
    where
    import Graph
    import Pathing
    import Parser

    import Control.Applicative

    import Data.Char

    -- |Type alias for dot graphs.
    type DotGraph = Graph String

    -- |Writes a graph to the DOT format.
    encode :: DotGraph -> String
    encode g = if isDirected g
        then let
            body = concat ["  " ++ display' a ++ " -> " ++ display' b ++ ";\n" | (a, b) <- g]
            in "digraph {\n" ++ body ++ "}"
        else let
            g' = antisymmetric g
            body = concat ["  " ++ display' a ++ " -- " ++ display' b ++ ";\n" | (a, b) <- g']
            in "graph {\n" ++ body ++ "}"
        where
        display' xs = case parse (graphic <|> numeral) xs of
            Just (xs', []) | xs' == xs -> xs
            _ -> "\"" ++ xs ++ "\""

    -- |Writes a graph to the DOT format.
    decode :: String -> Maybe DotGraph
    decode xs = case parse (graph <|> digraph) xs of
        Just (g, _) -> Just g
        _ -> Nothing

    -- |Parses a graph.
    graph :: Parser DotGraph
    graph = do
        symbol "graph"
        symbol "{"
        gs <- many $ subgraph "--"
        symbol "}"
        return $ symmetric $ concat gs

    -- |Parses a directed graph.
    digraph :: Parser DotGraph
    digraph = do
        symbol "digraph"
        symbol "{"
        gs <- many $ subgraph "->"
        symbol "}"
        return $ concat gs

    -- |Parses a subgraph.
    subgraph :: String -> Parser DotGraph
    subgraph separator = cluster separator <|> path separator

    -- |Parses a path of idenifiers into a simple subgraph.
    path :: String -> Parser DotGraph
    path separator = do
        x <- token identifier
        xs <- many $ symbol separator >> token identifier
        return $ graphify $ x : xs

    -- |Parses a cluster of identifiers into a simple tree-style subgraph.
    cluster :: String -> Parser DotGraph
    cluster separator = do
        x <- token identifier
        symbol separator
        symbol "{"
        xs <- many $ token identifier
        symbol "}"
        return [(x, x') | x' <- xs]
