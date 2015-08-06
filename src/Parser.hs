module Parser (
  readExpr
) where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Scheme

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser Scheme.LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ Scheme.String x

parseAtom :: Parser Scheme.LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Scheme.Bool True
                         "#f" -> Scheme.Bool False
                         _    -> Scheme.Atom atom

parseNumber :: Parser Scheme.LispVal
parseNumber = many1 digit
    >>= return . Scheme.Number . read

parseExpr :: Parser Scheme.LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
