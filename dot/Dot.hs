-- |Suppies functions for importing and exporting DOT scripts.
module Dot (module Dot)
    where
    import Graph
    import Pathing
    import Parser

    import Control.Applicative

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

    -- |Parses a token.
    token :: Parser a -> Parser a
    token p = do
        whitestuff
        v <- p
        return v

    -- |Parses whitestuff.
    whitestuff :: Parser ()
    whitestuff = whitespace <|> comment

    -- |Parses comments.
    comment :: Parser ()
    comment = do
        string "//"
        many (sat (/= '\n'))
        return ()