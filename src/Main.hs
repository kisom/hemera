import qualified System.Environment as Env
import qualified Parser

main :: IO ()
main = do 
        args <- Env.getArgs
        case args of
            (expr:_) -> putStrLn (Parser.readExpr expr)
            _        -> return ()


