module Main (main) where

import qualified Hmmm.Infer ()
import Hmmm.Parser
import Hmmm.Syntax (Expr)

print' :: Either String Expr -> IO ()
print' (Left err) = putStrLn err
print' (Right expr) = print expr

main :: IO ()
main = getLine >>= print' . runParse >> main
