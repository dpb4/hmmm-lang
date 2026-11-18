module Main (main) where

import Control.Monad (unless)
import Hmmm.Eval
import Hmmm.Infer (typeOf)
import Hmmm.Parser
import System.IO (isEOF)

main :: IO ()
main = do
    eof <- isEOF
    unless eof $ do
        getLine >>= run
        main

run :: String -> IO ()
run line =
    case runParse line of
        Left err -> putStrLn $ "parse error:\n" ++ err
        Right expr -> case typeOf expr of
            Left err -> putStrLn $ "type error:\n" ++ err
            Right t -> case runEval expr of
                Left err -> putStrLn $ "runtime error:\n" ++ err
                Right v -> putStrLn ("-> " ++ show v ++ " : " ++ show t)
