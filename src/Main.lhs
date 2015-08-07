> import qualified Control.Monad as M
> import qualified System.Environment as Env
> import qualified Parser
> import qualified Scheme

For now, main just needs to read arguments from the input and parse
them.

> main :: IO ()
> main = evalStrings <$> Env.getArgs >>= M.mapM_ stringOut

To aid in testing, here's a function that will apply the parser to all
of its args.

> parseStrings :: [String] -> [Scheme.Imperfect Scheme.LispVal]
> parseStrings = map Parser.parseLisp

Following on, evaluating a list of strings:

> evalStrings :: [String] -> [Scheme.Imperfect Scheme.LispVal]
> evalStrings = map (\s -> Parser.parseLisp s >>= Scheme.eval)

> stringOut :: Scheme.Imperfect Scheme.LispVal -> IO ()
> stringOut (Left e)  = putStrLn $ show e
> stringOut (Right v) = putStrLn $ show v
