-- |Monadic parser.
module Parser (Parser)
    where
    import Control.Applicative
    import Data.Char

    -- |A data structure which defines a monadic parser.
    newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

    instance Functor Parser
        where
        fmap f p = Parser $ \input -> do
            (v, output) <- parse p input
            Just (f v, output)

    instance Applicative Parser
        where
        pure v = Parser $ \input -> Just (v, input)
        pf <*> pv = Parser $ \input -> do
            (f, input') <- parse pf input
            (v, output) <- parse pv input'
            Just (f v, output)

    instance Monad Parser
        where
        pv >>= f = Parser $ \input -> do
            (v, output) <- parse pv input
            parse (f v) output

    instance Alternative Parser
        where
        empty = Parser $ \_ -> Nothing
        p <|> q = Parser $ \input -> case parse p input of
            Nothing -> parse q input
            x -> x

    -- |Gets the first character of the input string.
    next :: Parser Char
    next = Parser $ \input -> case input of
        [] -> Nothing
        (x : xs) -> Just (x, xs)
    
    -- |Same as `next` except the character must satisfy some predicate `p`.
    sat :: (Char -> Bool) -> Parser Char
    sat p = do
        x <- next
        if p x
        then return x
        else empty
    
    -- |Parses a digit.
    digit :: Parser Char
    digit = sat isDigit

    -- |Parses an alphabetic letter.
    alphabetic :: Parser Char
    alphabetic = sat isAlpha

    -- |Parses an alphabetic letter.
    alphanumeric :: Parser Char
    alphanumeric = sat isAlphaNum

    -- |Parses a specific character.
    char :: Char -> Parser Char
    char x = sat (== x)

    -- |Parses a string.
    string :: String -> Parser String
    string [] = return []
    string (x : xs) = do
        char x
        string xs
    
    -- |Parses whitespace.
    space :: Parser ()
    space = do
        many (sat isSpace)
        return ()