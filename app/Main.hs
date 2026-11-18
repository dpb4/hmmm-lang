module Main (main) where

import Hmmm.Eval
import Hmmm.Parser

main :: IO ()
main = do
    line <- getLine
    case runParse line of
        Left err -> putStrLn $ "parse error:\n" ++ err
        Right expr -> case runEval expr of
            Left err -> putStrLn $ "runtime error:\n" ++ err
            Right v -> putStrLn ("-> " ++ show v)
    main
