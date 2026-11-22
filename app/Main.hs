{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad (unless)
import Hmmm.Builtins (builtinEnv, builtinTypes)
import Hmmm.Eval
import Hmmm.Infer (typeOf, typeOfProgram)
import Hmmm.Parser
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, isEOF, stderr)

-- | with no arguments, start a REPL; with a file path, run that file
main :: IO ()
main =
    getArgs >>= \args -> case args of
        [] -> repl
        [path] ->
            try (readFile path) >>= \case
                Left e -> failWith $ show (e :: IOException)
                Right source -> runFile source
        _ -> do
            name <- getProgName
            hPutStrLn stderr $ "usage: " ++ name ++ " [FILE]"
            exitFailure

repl :: IO ()
repl = do
    eof <- isEOF
    unless eof $ do
        getLine >>= run
        repl

run :: String -> IO ()
run line =
    case runParse line of
        Left err -> putStrLn $ "parse error:\n" ++ err
        Right expr -> case typeOf builtinTypes expr of
            Left err -> putStrLn $ "type error:\n" ++ err
            Right t -> case runEval builtinEnv expr of
                Left err -> putStrLn $ "runtime error:\n" ++ err
                Right v -> putStrLn ("-> " ++ display t v ++ " : " ++ show t)

runFile :: String -> IO ()
runFile source =
    case runParseTopExprs source of
        Left err -> failWith $ "parse error:\n" ++ err
        Right prog -> case typeOfProgram builtinTypes prog of
            Left err -> failWith $ "type error:\n" ++ err
            Right ts -> case runEvalProgram builtinEnv prog of
                Left err -> failWith $ "runtime error:\n" ++ err
                Right vs -> mapM_ putStrLn (zipWith display ts vs)

failWith :: String -> IO ()
failWith msg = hPutStrLn stderr msg >> exitFailure
