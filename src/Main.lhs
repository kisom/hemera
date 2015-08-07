> import qualified System.Environment as Env
> import qualified Parser
> import qualified Scheme

For now, main just needs to read arguments from the input and parse
them.

> main :: IO ()
> main = Env.getArgs >>= mapM_ rep
>     where rep = print . Scheme.eval . Parser.readExpr

To aid in testing, here's a function that will apply the parser to all
of its args.

> parseStrings :: [String] -> [Either String Scheme.LispVal]
> parseStrings = map Parser.parseLisp
