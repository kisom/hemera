> module Scheme (
>     LispVal(..)
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
>     deriving (Show, Read)

