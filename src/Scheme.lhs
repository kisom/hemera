> module Scheme (
>     LispVal(..)
>   , LispError(..)
>   , Imperfect
>   , showVal
>   , eval
>   , trapError
>   , extractValue
> ) where

`Control.Monad.Error` provides the error and exception tools Hemera will
use for signalling errors.

> import qualified Control.Monad.Error as E
> import qualified Text.ParserCombinators.Parsec as P (ParseError)

The core Lisp values are:

+ the atom
+ the list, which denotes a proper list
+ the dotted list, which denotes an improper list
+ numbers, which is currently integral only
+ strings
+ booleans, the values #t for true and #f for false

> data LispVal = Atom String
>              | List [LispVal]
>              | DottedList [LispVal] LispVal
>              | Number Integer
>              | String String
>              | Bool Bool
>     deriving (Read)

The nil value is important in Scheme:

> nil :: LispVal
> nil = Atom "nil"

It's useful to be able to print out the value of a `LispVal`:

> showVal :: LispVal -> String
> showVal (String s)       = "\"" ++ s ++ "\""
> showVal (Atom a)         = a
> showVal (Number n)       = show n
> showVal (Bool True)      = "#t"
> showVal (Bool _)         = "#f"
> showVal (List lst)       = "(" ++ unwordsList lst ++ ")"
> showVal (DottedList h t) = "(" ++ unwordsList h ++ "." ++ showVal t ++ ")"

Also useful to show the type of a thing:

> showType :: LispVal -> String
> showType (String _)       = "string"
> showType (Atom _)         = "symbol"
> showType (Number _)       = "integer"
> showType (Bool _)         = "boolean"
> showType (List _)         = "cons"
> showType (DottedList _ _) = "cons"

In order to display the contents of a list (both proper and improper),
we'll need to combine the strings showing their values:

> unwordsList :: [LispVal] -> String
> unwordsList = unwords . map showVal

With `showVal`, `LispVal` can be made to satisfy the `Show` interface:

> instance Show LispVal where show = showVal

Evaluation for the primitive types is simple: return the primitive itself.

> eval :: LispVal -> Imperfect LispVal
> eval v@(String _)             = return v
> eval v@(Atom _)               = return v
> eval v@(Number _)             = return v
> eval v@(Bool _)               = return v
> eval (List [Atom "quote", v]) = return v
> eval (List (Atom f:args))     = mapM eval args >>= apply f
> eval badForm                  = E.throwError
>                               $ BadSpecialForm "Unrecognised special form"
>                                 badForm

Evaluation requires the apply function: we need a way to apply a function to some
arguments. An Atom contains a string with the name of the function, so lookup
needs to return a function corresponding to the string.

> apply :: String -> [LispVal] -> Imperfect LispVal
> apply f args = maybe (E.throwError $ NotFunction "Illegal function call" f)
>                      ($ args)
>                      (lookup f primitives)

primitives is a mapping of string names to all the builtin functions:

> primitives :: [(String, [LispVal] -> Imperfect LispVal)]
> primitives = [("+",              numericBinOp (+))
>              ,("-",              numericBinOp (-))
>              ,("*",              numericBinOp (*))
>              ,("/",              numericBinOp div)
>              ,("mod",            numericBinOp mod)
>              ,("quotient",       numericBinOp quot)
>              ,("rem",            numericBinOp rem)
>              ,("number?",        isNumber)
>              ,("string?",        isString)
>              ,("symbol?",        isSymbol)
>              ,("list?",          isList)
>              ,("nil?",           isNull)
>              ,("symbol->string", symbolToString)
>              ,("string->symbol", stringToSymbol)
>              ,("type-of",        typeString)
>              ]

numericBinOp is a function to apply a numeric function to list of arguments.

> numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> Imperfect LispVal
> numericBinOp f []    = E.throwError $ NumArgs 2 []
> numericBinOp f v@[_] = E.throwError $ NumArgs 2 v
> numericBinOp f ns = mapM unpackNumber ns >>= return . Number . foldl1 f

unpackNumber is how we get numbers out of our LispVals.

> unpackNumber :: LispVal -> Imperfect Integer
> unpackNumber (Number n) = return n
> unpackNumber v          = E.throwError $ TypeMismatch "integer" v

We also want to permit checking the types of values:

> isNumber :: [LispVal] -> Imperfect LispVal
> isNumber ((Number _):[]) = return $ Bool True
> isNumber _               = return $ Bool False
>
> isString :: [LispVal] -> Imperfect LispVal
> isString ((String _):[]) = return $ Bool True
> isString _               = return $ Bool False
>
> isSymbol :: [LispVal] -> Imperfect LispVal
> isSymbol ((Atom _):[]) = return $ Bool True
> isSymbol _             = return $ Bool False
>
> isList :: [LispVal] -> Imperfect LispVal
> isList ((List _):[])         = return $ Bool True
> isList ((DottedList _ _):[]) = return $ Bool True
> isList _                     = return $ Bool False
>
> isNull :: [LispVal] -> Imperfect LispVal
> isNull (Atom "nil":[]) = return $ Bool True
> isNull (List []:[])    = return $ Bool True
> isNull _               = return $ Bool False

The following symbol-handling functions are defined in R5RS:

> symbolToString :: [LispVal] -> Imperfect LispVal
> symbolToString (Atom sym:[]) = return $ String sym
> symbolToString (v:_)         = E.throwError $ TypeMismatch "symbol" v

> stringToSymbol :: [LispVal] -> Imperfect LispVal
> stringToSymbol (String s:[]) = return $ Atom s
> stringToSymbol (v:_)         = E.throwError $ TypeMismatch "string" v

Getting the type of a value isn't just a matter of applying a LispVal
to `showType`: we need to make sure there's only one value, and apply
that to `showType`.

> typeString :: [LispVal] -> Imperfect LispVal
> typeString (v:_) = return . String $ showType v
> typeString v     = E.throwError $ NumArgs 1 v

To implement proper errors, we'll need a LispError.

> data LispError = NumArgs Integer [LispVal]
>                | TypeMismatch String LispVal
>                | Syntax P.ParseError
>                | BadSpecialForm String LispVal
>                | NotFunction String String
>                | UnboundVar String String
>                | Default String

Errors should be displayable:

> showError :: LispError -> String
> showError (UnboundVar m v)      = m ++ ": " ++ v
> showError (BadSpecialForm m f)  = m ++ ": " ++ show f
> showError (NotFunction m f)     = m ++ ": " ++ show f 
> showError (NumArgs e args)      = "Expected " ++ show e ++ " args, given " 
>                                               ++ unwordsList args 
> showError (TypeMismatch e have) = "Type mismatch: the value "
>                                ++ show have ++ " (type: " ++ showType have
>                                ++ ") is not of type " ++ e
> showError (Syntax err)          = "Syntax error: " ++ show err
> instance Show LispError where show = showError

In order to work with the Haskell Except module, we'll need to make this an
instance of the `Error` typeclass:

> instance E.Error LispError where
>     noMsg  = Default "unknown error"
>     strMsg = Default

TODO: rewrite this to use `Control.Monad.Except`. Getting deprecation errors:

```
src/Scheme.lhs:11:3: Warning:
    Module ‘Control.Monad.Error’ is deprecated:
      Use Control.Monad.Except instead
```

We need a way to denote functions that can throw an error:

> type Imperfect = Either LispError
> trapError action = E.catchError action (return . show)

Hemera still needs a way to extract values from an Imperfect function:

> extractValue :: Imperfect a -> a
> extractValue (Right v) = v

The `Left` match is left undone: this is a programmer error that should be
caught.

