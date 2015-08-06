> import qualified System.Environment as Env
> import qualified Parser

For now, main just needs to read arguments from the input and parse
them.

> main :: IO ()
> main = do 
>         args <- Env.getArgs
>         case args of
>             (expr:_) -> putStrLn (Parser.readExpr expr)
>             _        -> return ()


