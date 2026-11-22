{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad (unless)
import Hmmm.Eval
import Hmmm.Infer (typeOf, typeOfProgram)
import Hmmm.Parser
import Hmmm.Prelude
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, isEOF, stderr)

-- | with no arguments, start a REPL; with a file path, run that file
main :: IO ()
main =
    getArgs >>= \args -> case args of
        [] -> putStrLn "\nnow in hmmm REPL" >> withScope repl
        [path] ->
            try (readFile path) >>= \case
                Left e -> failWith $ show (e :: IOException)
                Right source -> withScope (`runFile` source)
        _ -> do
            name <- getProgName
            hPutStrLn stderr $ "usage: " ++ name ++ " [FILE]"
            exitFailure

-- | load the prelude, then run the action in the scope it defines
withScope :: (Scope -> IO ()) -> IO ()
withScope action =
    loadPrelude >>= \case
        Left err -> failWith err
        Right scope -> action scope

repl :: Scope -> IO ()
repl scope = do
    eof <- isEOF
    unless eof $ do
        getLine >>= run scope
        repl scope

run :: Scope -> String -> IO ()
run (Scope ctx env) line =
    case runParse line of
        Left err -> putStrLn $ "parse error:\n" ++ err
        Right expr -> case typeOf ctx expr of
            Left err -> putStrLn $ "type error:\n" ++ err
            Right t -> case runEval env expr of
                Left err -> putStrLn $ "runtime error:\n" ++ err
                Right v -> putStrLn ("=> " ++ display t v ++ " : " ++ show t)

runFile :: Scope -> String -> IO ()
runFile (Scope ctx env) source =
    case runParseTopExprs source of
        Left err -> failWith $ "parse error:\n" ++ err
        Right prog -> case typeOfProgram ctx prog of
            Left err -> failWith $ "type error:\n" ++ err
            Right ts -> case runEvalProgram env prog of
                Left err -> failWith $ "runtime error:\n" ++ err
                Right vs -> mapM_ putStrLn (zipWith display ts vs)

failWith :: String -> IO ()
failWith msg = hPutStrLn stderr msg >> exitFailure
