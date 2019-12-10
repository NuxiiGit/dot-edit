-- |Monadic parser.
module Parser (Parser)
    where
    import Control.Applicative
    import Data.Char

    -- |A data structure which defines a monadic parser.
    newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

    instance Functor Parser
        where
        fmap f p = Parser (\input -> do
            (v, output) <- parse p input
            Just (f v, output))

    instance Applicative Parser
        where
        pure v = Parser (\input -> Just (v, input))
        pf <*> pv = Parser (\input -> do
            (f, input') <- parse pf input
            (v, output) <- parse pv input'
            Just (f v, output))

    instance Monad Parser
        where
        pv >>= f = Parser (\input -> do
            (v, output) <- parse pv input
            parse (f v) output)

    instance Alternative Parser
        where
        empty = Parser (\_ -> Nothing)
        p <|> q = Parser (\input -> case parse p input of
            Nothing -> parse q input
            x -> x)