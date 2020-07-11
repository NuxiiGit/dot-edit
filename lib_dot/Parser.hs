-- |Monadic parser.
module Parser (module Parser)
    where
    import Data.Char
    import Control.Applicative

    -- |A data structure which defines a monadic parser.
    newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

    instance Functor Parser
        where
        fmap f p = Parser $ \input -> do
            (v, output) <- parse p input
            return (f v, output)

    instance Applicative Parser
        where
        pure v = Parser $ \input -> return (v, input)
        pf <*> pv = Parser $ \input -> do
            (f, input') <- parse pf input
            (v, output) <- parse pv input'
            return (f v, output)

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

    -- |Parses a statement.
    statement :: Parser a -> Parser a
    statement p = do
        value <- token p
        many filler
        char ';' <|> char ',' <|> char '\n'
        return value
        where
        filler = do
            x <- sat isSpace
            if x == '\n'
            then empty
            else return x

    -- |Just like `string`, except it ignores whitestuff.
    symbol :: String -> Parser String
    symbol s = token $ string s

    -- |Parses a token.
    token :: Parser a -> Parser a
    token p = (whitestuff >> token p) <|> p

    -- |Parses whitestuff.
    whitestuff :: Parser ()
    whitestuff = whitespace <|> comment <|> directive <|> blockComment <|> statementSeparator

    -- |Parses space.
    whitespace :: Parser ()
    whitespace = do
        some $ sat isSpace
        return ()

    -- |Parses a statement end.
    statementSeparator :: Parser ()
    statementSeparator = do
        char ';' <|> char ','
        return ()

    -- |Parses comments.
    comment :: Parser ()
    comment = do
        string "//"
        many $ sat (/= '\n')
        return ()

    -- |Parses block comments.
    blockComment :: Parser ()
    blockComment = do
        beginComment
        endComment
        where
        beginComment = string "/*"
        endComment = do
            many $ sat (/= '*')
            next
            v <- next
            if v == '/'
            then return ()
            else endComment

    -- |Parses pre-processor directives.
    directive :: Parser ()
    directive = do
        char '#'
        many $ sat (/= '\n')
        return ()

    -- |Parses an identifier.
    identifier :: Parser String
    identifier = do
        x <- alphabetic
        xs <- many alphanumeric
        return (x : xs)

    -- |Parses a string.
    string :: String -> Parser String
    string [] = return []
    string (x : xs) = do
        char x
        string xs
        return (x : xs)

    -- |Parses a specific character.
    char :: Char -> Parser Char
    char x = sat (== x)

    -- |Parses a digit.
    digit :: Parser Char
    digit = sat isDigit

    -- |Parses an alphabetic letter.
    alphabetic :: Parser Char
    alphabetic = sat isAlpha

    -- |Parses an alphanumeric letter.
    alphanumeric :: Parser Char
    alphanumeric = sat isAlphaNum

    -- |Same as `next` except the character must satisfy some predicate `p`.
    sat :: (Char -> Bool) -> Parser Char
    sat p = do
        x <- next
        if p x
        then return x
        else empty
    
    -- |Gets the first character of the input string.
    next :: Parser Char
    next = Parser $ \input -> case input of
        [] -> Nothing
        (x : xs) -> Just (x, xs)
