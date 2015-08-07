> import qualified Control.Monad as M
> import qualified System.Environment as Env
> import qualified Parser
> import qualified Scheme

For now, main just needs to read arguments from the input and parse
them.

> main :: IO ()
> main = do
>     args <- Env.getArgs
>     eval <- return $ M.liftM show $ Parser.readExpr (args !! 0) >>= Scheme.eval
>     putStrLn $ Scheme.extractValue $ Scheme.trapError eval

To aid in testing, here's a function that will apply the parser to all
of its args.

> parseStrings :: [String] -> [Either String Scheme.LispVal]
> parseStrings = map Parser.parseLisp
