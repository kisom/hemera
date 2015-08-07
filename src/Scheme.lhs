> module Scheme (
>     LispVal(..)
>   , showVal
>   , eval
> ) where

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

In order to display the contents of a list (both proper and improper),
we'll need to combine the strings showing their values:

> unwordsList :: [LispVal] -> String
> unwordsList = unwords . map showVal

With `showVal`, `LispVal` can be made to satisfy the `Show` interface:

> instance Show LispVal where show = showVal

Evaluation for the primitive types is simple: return the primitive itself.

> eval :: LispVal -> LispVal
> eval v@(String _)             = v
> eval v@(Atom _)               = v
> eval v@(Number _)             = v
> eval v@(Bool _)               = v
> eval (List [Atom "quote", v]) = v
> eval (List (Atom f:args))     = apply f $ map eval args

Evaluation requires the apply function: we need a way to apply a function to some
arguments. An Atom contains a string with the name of the function, so lookup
needs to return a function corresponding to the string.

> apply :: String -> [LispVal] -> LispVal
> apply f args = maybe (Bool False) ($ args) $ lookup f primitives

primitives is a mapping of string names to all the builtin functions:

> primitives :: [(String, [LispVal] -> LispVal)]
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
>              ]

numericBinOp is a function to apply a numeric function to list of arguments.

> numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
> numericBinOp f ns = Number $ foldl1 f $ map unpackNumber ns

unpackNumber is how we get numbers out of our LispVals.

> unpackNumber :: LispVal -> Integer
> unpackNumber (Number n) = n
> unpackNumber _          = 0

This returns 0 (for now) if the value can't be parsed as a number. Note: the
book uses weak typing to also parse string values, but I've elected to not
permit this behaviour.

We also want to permit checking the types of values:

> isNumber :: [LispVal] -> LispVal
> isNumber ((Number _):[]) = Bool True
> isNumber _               = Bool False
>
> isString :: [LispVal] -> LispVal
> isString ((String _):[]) = Bool True
> isString _               = Bool False
>
> isSymbol :: [LispVal] -> LispVal
> isSymbol ((Atom _):[]) = Bool True
> isSymbol _             = Bool False
>
> isList :: [LispVal] -> LispVal
> isList ((List _):[])         = Bool True
> isList ((DottedList _ _):[]) = Bool True
> isList _                     = Bool False
>
> isNull :: [LispVal] -> LispVal
> isNull (nil:[]) = Bool True
> isNull _        = Bool False

The following symbol-handling functions are defined in R5RS:

> symbolToString :: [LispVal] -> LispVal
> symbolToString (Atom sym:[]) = String sym
> symbolToString _             = nil

> stringToSymbol :: [LispVal] -> LispVal
> stringToSymbol (String s:[]) = Atom s
> stringToSymbol _             = nil
