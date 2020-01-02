-- |Suppies functions for importing and exporting DOT scripts.
module Dot (module Dot)
    where
    import Graph
    import Pathing
    import Parser

    import Control.Monad

    {- TODO:
     - Add function for loading DOT scripts.
     - Add function for saving DOT scripts.
     -}
    
    -- |Type alias for dot graphs.
    type DotGraph = Graph String

    -- |Writes a graph to the DOT format.
    encode :: DotGraph -> String
    encode g = "graph { " ++ body ++ "}"
        where
        body = concat [a ++ " -- " ++ b ++ "; " | (a, b) <- g]

    -- |Writes a graph to the DOT format.
    decode :: String -> Parser DotGraph -> DotGraph
    decode xs = case parse graph xs of
        Just (g, []) -> g
        Just (_, s) -> error $ "expected EOF, got " ++ s
        _ -> error "failed to parse graph"

    -- |Parses a graph.
    graph :: Parser DotGraph
    graph = undefined