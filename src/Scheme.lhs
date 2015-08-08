> {-# LANGUAGE ExistentialQuantification #-}
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

> import qualified Control.Monad as M
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

It's useful to be able to print out the value of a `LispVal`:

> showVal :: LispVal -> String
> showVal (String s)       = "\"" ++ s ++ "\""
> showVal (Atom a)         = a
> showVal (Number n)       = show n
> showVal (Bool True)      = "#t"
> showVal (Bool _)         = "#f"
> showVal (List lst)       = "(" ++ unwordsList lst ++ ")"
> showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"

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
> eval v@(String _)                = return v
> eval v@(Atom _)                  = return v
> eval v@(Number _)                = return v
> eval v@(Bool _)                  = return v
> eval (List [Atom "if", p, t, f]) =
>     do
>         result <- eval p
>         case result of
>             (Bool False) -> eval f
>             (Bool True)  -> eval t
>             v            -> E.throwError $ TypeMismatch "boolean" v
> eval (List [Atom "quote", v])    = return v
> eval (List (Atom f:args))        = mapM eval args >>= apply f
> eval badForm                     = E.throwError
>                                  $ BadSpecialForm "Unrecognised special form"
>                                    badForm

Evaluation requires the apply function: we need a way to apply a function to some
arguments. An Atom contains a string with the name of the function, so lookup
needs to return a function corresponding to the string.

> apply :: String -> [LispVal] -> Imperfect LispVal
> apply f args = maybe (E.throwError $ NotFunction "Illegal function call" f)
>                      ($ args)
>                      (lookup f primitives)

primitives is a mapping of string names to all the builtin functions:

> primitives :: [(String, [LispVal] -> Imperfect LispVal)]
> primitives = [
>               ("not",            boolNot)
>              ,("cons",           cons)
>              ,("car",            car)
>              ,("cdr",            cdr)
>              ,("list",           list)
>              ,("+",              numericOp0 (+) 0)
>              ,("-",              numericBinOp (-))
>              ,("*",              numericOp0 (*) 1)
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
>              ,("=",              numBoolBinOp (==))
>              ,(">",              numBoolBinOp (>))
>              ,("<",              numBoolBinOp (<))
>              ,("/=",             numBoolBinOp (/=))
>              ,(">=",             numBoolBinOp (>=))
>              ,("<=",             numBoolBinOp (<=))
>              ,("or",             boolBoolBinOp (||))
>              ,("and",            boolBoolBinOp (&&))
>              ,("string=",        stringBoolBinOp (==))
>              ,("string<",        stringBoolBinOp (<))
>              ,("string>",        stringBoolBinOp (>))
>              ,("string<=",       stringBoolBinOp (>=))
>              ,("string>=",       stringBoolBinOp (<=))
>              ,("eq?",            eqv)
>              ,("eqv?",           eqv)
>              ,("equal?",         equal)
>              ]

boolNot is a logical inversion:

> boolNot :: [LispVal] -> Imperfect LispVal
> boolNot (Bool True:[])  = return $ Bool False
> boolNot (Bool False:[]) = return $ Bool True
> boolNot (v:[])          = E.throwError $ TypeMismatch (showType (Bool True)) v
> boolNot v               = E.throwError $ NumArgs v

numericOp0 is a function to apply a numeric function to list of zero
or more arguments.

> numericOp0 :: (Integer -> Integer -> Integer) -> Integer -> [LispVal] -> Imperfect LispVal
> numericOp0 f def []     = return $ Number def
> numericOp0 f def (n:[]) = unpackNumber n >>= (return . Number . f def)
> numericOp0 f _ ns       = mapM unpackNumber ns >>= return . Number . foldl1 f

numericBinOp is a function to apply a numeric function to list of arguments.

> numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> Imperfect LispVal
> numericBinOp f []       = E.throwError $ NumArgs []
> numericBinOp f v@(_:[]) = E.throwError $ NumArgs v
> numericBinOp f ns       = mapM unpackNumber ns >>= return . Number . foldl1 f

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
> typeString v     = E.throwError $ NumArgs v 

Implementing the numeric boolean operations requires some helpers; a general
boolean function that performs some function `f` on a pair of arguments, using
an unpacker `u` to extract the relevant value from the function.

> boolBinOp :: (LispVal -> Imperfect a)
>           -> (a -> a -> Bool)
>           -> [LispVal]
>           -> Imperfect LispVal
> boolBinOp u f args = if length args /= 2
>                         then E.throwError $ NumArgs args
>                         else do
>                              left  <- u $ args !! 0
>                              right <- u $ args !! 1
>                              return $ Bool $ f left right

There's already a numeric unpacker (`unpackNumber`); we need to write one
for strings and booleans as well, now.

> unpackString :: LispVal -> Imperfect String
> unpackString (String s) = return s
> unpackString v          = E.throwError $ TypeMismatch "string" v

> unpackBool :: LispVal -> Imperfect Bool
> unpackBool (Bool v) = return v
> unpackBool v        = E.throwError $ TypeMismatch "boolean" v

Now the binary boolean operation functions can be specified.

> numBoolBinOp    = boolBinOp unpackNumber
> stringBoolBinOp = boolBinOp unpackString
> boolBoolBinOp   = boolBinOp unpackBool

It wouldn't be a Lisp without car, cons, and cdr:

> car :: [LispVal] -> Imperfect LispVal
> car [List (x : xs)]       = return x
> car [DottedList (x:xs) _] = return x
> car [v]                   = E.throwError $ TypeMismatch "cons" v
> car v                     = E.throwError $ NumArgs v

> cdr :: [LispVal] -> Imperfect LispVal
> cdr [List (x : xs)]         = return $ List xs
> cdr [DottedList [_] x]      = return x
> cdr [DottedList (_ : xs) x] = return $ DottedList xs x
> cdr [v]                     = E.throwError $ TypeMismatch "cons" v
> cdr v                       = E.throwError $ NumArgs v

> cons :: [LispVal] -> Imperfect LispVal
> cons [v, List []]        = return $ List [v]
> cons [v, List vs]        = return $ List (v : vs)
> cons [v, DottedList h t] = return $ DottedList (v:h) t
> cons [v1, v2]            = return $ DottedList [v1] v2
> cons v                   = E.throwError $ NumArgs v

The `list` function will be useful too:

> list :: [LispVal] -> Imperfect LispVal
> list v = return $ List v

The equality functions `eq?` and `eqv?` are the slower equality functions
that return #t if their arguments print the same.

> eqv :: [LispVal] -> Imperfect LispVal
> eqv [(Bool v1), (Bool v2)]     = return $ Bool $ v1 == v2
> eqv [(Number v1), (Number v2)] = return $ Bool $ v1 == v2
> eqv [(String v1), (String v2)] = return $ Bool $ v1 == v2
> eqv [(Atom v1), (Atom v2)]     = return $ Bool $ v1 == v2
> eqv [(DottedList xs x),
>      (DottedList ys y)]        = eqv [List $ xs ++ [x],
>                                       List $ ys ++ [y]]
> eqv [(List l1), (List l2)]     = return $ Bool $ (length l1 == length l2) &&
>                                                  (all eqvPair $ zip l1 l2)
>   where eqvPair (a, b) = case eqv [a, b] of
>                            Left err         -> False
>                            Right (Bool val) -> val
> eqv [_, _]                     = return $ Bool False
> eqv v                          = E.throwError $ NumArgs v

Implementing "equal?", which does equality like JavaScript does equality,
requires the existential types extension to use heterogenous lists.

> data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> Imperfect a)

This adds the constraint that a must be an instance of the `Eq`
typeclass. The unpackEquals helper will exploit this behaviour.

> unpackEquals :: LispVal -> LispVal -> Unpacker -> Imperfect Bool
> unpackEquals v1 v2 (AnyUnpacker u) =
>              do u1 <- u v1
>                 u2 <- u v2
>                 return $ u1 == u2
>        `E.catchError` (const $ return False)

equal can be written in terms of these functions:

> equal :: [LispVal] -> Imperfect LispVal
> equal [v1, v2] = do
>     primEq <- M.liftM or $ M.mapM (unpackEquals v1 v2)
>               [AnyUnpacker unpackNumber
>               ,AnyUnpacker unpackString
>               ,AnyUnpacker unpackBool]
>     eqvEquals <- eqv [v1, v2]
>     return $ Bool $ (primEq || let (Bool x) = eqvEquals in x)
> equal v        = E.throwError $ NumArgs v

-------------------------------------------------------------------------------

To implement proper errors, we'll need a LispError.

> data LispError = NumArgs [LispVal]
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
> showError (NumArgs args)        = "Invalid number of arguments: "
>                                ++ show (length args)
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

