> import Control.Applicative
> import qualified Control.Monad as M
> import qualified System.Environment as Env
> import qualified System.IO as IO
> import qualified Parser
> import qualified Scheme

For now, main just needs to read arguments from the input and parse
them.

> main :: IO ()
> main = do
>     args <- Env.getArgs
>     case length args of
>         0 -> repl
>         1 -> let arg = args !! 0 in
>              case arg of
>                   "-h" -> putStrLn "Start a REPL with `./hemera`" >>
>                           putStrLn "Run a source file with `./hemera [path]`"
>                   path -> readFile path >>= evalAndPrint

Strings are generally buffered to standard output; `prnFlush` will
print a string and immediately flush the buffer, ensuring the string is
printed immediately.

> prnFlush :: String -> IO ()
> prnFlush s = putStr s >> IO.hFlush IO.stdout

This is useful in getting reading a line of input from the user:

> readPrompt :: String -> IO String
> readPrompt prompt = prnFlush prompt >> IO.getLine

That's the 'R' in REPL. Now for the 'E':

> evalString :: String -> IO String
> evalString expr = return $ Scheme.extractValue
>                          $ Scheme.trapError (M.liftM show $
>                            Parser.readExpr expr >>= Scheme.eval)

Followed by the 'P':

> evalAndPrint :: String -> IO ()
> evalAndPrint ""   = return ()
> evalAndPrint expr = evalString expr >>= putStrLn

Finally, time to build the 'L':

> until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
> until_ pred prompt action = do
>     result <- prompt
>     if pred result
>        then return ()
>        else action result >> until_ pred prompt action

Style note: adding an underscore to the name is a naming convention for
monadic conventions that repeat but don't return a value.

> repl :: IO ()
> repl = until_ (== ":q") (readPrompt "Hemera> ") evalAndPrint

Time to parse a file:

