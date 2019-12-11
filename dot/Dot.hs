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

    graph :: Parser DotGraph
    graph = undefined

    digraph :: Parser DotGraph
    digraph = undefined