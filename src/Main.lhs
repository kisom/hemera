> import Control.Applicative
> import qualified Control.Monad.Error as E
> import qualified Control.Monad as M
> import qualified System.Environment as Env
> import qualified System.IO as IO
> import qualified Parser
> import qualified Scheme

The main loop should run a REPL if no arguments are provided, or
if given a file name as an argument, it should evaluate that file.

> main :: IO ()
> main = do
>     args <- Env.getArgs
>     case length args of
>         0 -> repl
>         1 -> let arg = args !! 0 in
>              case arg of
>                   "-h" -> putStrLn "Start a REPL with `./hemera`" >>
>                           putStrLn "Run a source file with `./hemera [path]`"
>                   path -> readFile path >>= runWith

Strings are generally buffered to standard output; `prnFlush` will
print a string and immediately flush the buffer, ensuring the string is
printed immediately.

> prnFlush :: String -> IO ()
> prnFlush s = putStr s >> IO.hFlush IO.stdout

This is useful in getting reading a line of input from the user:

> readPrompt :: String -> IO String
> readPrompt prompt = prnFlush prompt >> IO.getLine

That's the 'R' in REPL. Now for the 'E':

> evalString :: Scheme.Env -> String -> IO String
> evalString env expr = runIOThrows $ M.liftM show
>                                   $ (Scheme.liftThrows $
>                                      Parser.readExpr expr)
>                                 >>= Scheme.eval env

Followed by the 'P':

> evalAndPrint :: Scheme.Env -> String -> IO ()
> evalAndPrint _   ""   = return ()
> evalAndPrint env expr = evalString env expr >>= putStrLn

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
> repl = Scheme.nullEnv >>= until_ (== ":q")
>                                  (readPrompt "Hemera> ") . 
>                                  evalAndPrint

evalLines splits a string into lines, such as those that come from a
file, and evaluates them.

> evalLines :: Scheme.Env -> String -> IO ()
> evalLines env s = M.mapM_ (evalAndPrint env) $ lines s

In order to run the top-level IOThrowsError, a function that runs the
error computation and catches error is needed:

> runIOThrows :: Scheme.IOThrowsError String -> IO String
> runIOThrows action = E.runErrorT (Scheme.trapError action)
>                  >>= return . Scheme.extractValue

> runWith :: String -> IO ()
> runWith s = Scheme.nullEnv >>= flip evalLines s
