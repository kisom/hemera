> module Parser (
>     readExpr
>   , parseLisp
> ) where

> import Control.Applicative ((<$>))
> import Control.Monad
> import qualified Control.Monad.Error as E
> import Text.ParserCombinators.Parsec hiding (spaces)
> import qualified Scheme

`symbol` declares the valid characters in a Scheme symbol.

> symbol :: Parser Char
> symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

Reading an expression returns a `String` for now: either the error
that occurred or whether a value was successfully parsed.

> readExpr :: String -> Scheme.Imperfect Scheme.LispVal
> readExpr input = case parse parseExpr "lisp" input of
>     Left err  -> E.throwError $ Scheme.Syntax err
>     Right val -> return val

> parseLisp :: String -> Scheme.Imperfect Scheme.LispVal
> parseLisp input = case parse parseExpr "lisp" input of
>     Left  err -> E.throwError $ Scheme.Syntax err
>     Right val -> Right val

If we encounter a space, we should use the Parsec skipMany1 to eat
them all.

> spaces :: Parser ()
> spaces = skipMany1 space

Comments are also useful to skip over.

> comment :: Parser ()
> comment = skipMany1 p
>     where p = char ';' >> many (noneOf "\n") >> newline 

We can combine these into a skipping skipper.

> skip :: Parser ()
> skip = spaces

Sometimes, it's useful to convert a single character to a string.

> parserCharToString :: Parser Char -> Parser String
> parserCharToString c = c >>= return . (:"")

An escaped character is any single character preceded by a backquote.

> parseEscapedChar :: Parser String
> parseEscapedChar = do
>     char '\\'
>     x <- oneOf "\"\\nrt"
>     return $ '\\' : [x]

A Scheme string will be enclosed in double quotes and contains a
sequence of escaped and unescaped characters.

> parseString :: Parser Scheme.LispVal
> parseString = do
>     char '"'
>     x <- many (unquoted <|> parseEscapedChar)
>     char '"'
>     return . Scheme.String $ concat x
>   where unquoted    = parserCharToString $ noneOf "\""

An atom is the basic unit. The case of true and false (the literals #t
and #f) need to be checked for, otherwise, the atom should be returned directly 

> parseAtom :: Parser Scheme.LispVal
> parseAtom = do 
>               first <- letter <|> symbol
>               last <- many (letter <|> digit <|> symbol)
>               let atom = (first:last)
>               return $ case atom of 
>                          "#t" -> Scheme.Bool True
>                          "#f" -> Scheme.Bool False
>                          _    -> Scheme.Atom atom

A number is a sequence (e.g., `many1`) of digits.

> parseNumber :: Parser Scheme.LispVal
> parseNumber = many1 digit
>     >>= return . Scheme.Number . read

A proper list is a series of expressions separated by spaces. The outer
parens will be captured by the caller as the same context could represent
a dotted list.

> parseProperList :: Parser Scheme.LispVal
> parseProperList =  Scheme.List <$> sepBy parseExpr skip

A dotted list has the final (tail) element separated from the other
elements in the list by a '.'

> parseDottedList :: Parser Scheme.LispVal
> parseDottedList = do
>     head <- endBy parseExpr skip
>     tail <- char '.' >> skip >> parseExpr
>     return $ Scheme.DottedList head tail

A list is either a proper or improper (dotted) list.

> parseList = between (char '(') (char ')') $ tryList
>   where tryList = try parseDottedList <|> parseProperList

The quote syntactic sugar is a Scheme expression prefaced by a quote
character.

> parseQuoted :: Parser Scheme.LispVal
> parseQuoted = do
>     char '\''
>     x <- parseExpr
>     return $ Scheme.List [Scheme.Atom "quote", x]

`parseExpr` tries these parsers to attempt to extract a `LispVal` from
a given string.

> parseExpr :: Parser Scheme.LispVal
> parseExpr = parseAtom
>          <|> parseString
>          <|> parseNumber
>          <|> parseQuoted
>          <|> parseList

