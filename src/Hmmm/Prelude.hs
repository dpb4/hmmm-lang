{-# LANGUAGE LambdaCase #-}

{- | the prelude: library functions written in hmmm itself,
checked and evaluated before every program, on top of the builtins
-}
module Hmmm.Prelude (
    Scope (..),
    builtinScope,
    withPrelude,
    loadPrelude,
) where

import Control.Exception (IOException, try)
import Data.Bifunctor (first)
import Hmmm.Builtins
import Hmmm.Eval
import Hmmm.Infer
import Hmmm.Parser
import Hmmm.Syntax
import Paths_hmmm_lang (getDataFileName)

-- | everything in scope when a program starts: types for checking, values for evaluation
data Scope = Scope
    { scopeTypes :: Context
    , scopeEnv :: Env
    }

-- | only the builtins, without the prelude
builtinScope :: Scope
builtinScope = Scope builtinTypes builtinEnv

-- | add the definitions in a prelude's source to a scope
withPrelude :: Scope -> String -> Either String Scope
withPrelude (Scope ctx env) source = do
    prog <- first ("prelude parse error:\n" ++) $ runParseTopExprs source
    case [e | TEExpr e <- prog] of
        [] -> return ()
        e : _ -> Left $ "the prelude may only contain definitions, found the expression " ++ show e
    ctx' <- first ("prelude type error:\n" ++) $ programContext ctx prog
    env' <- first ("prelude runtime error:\n" ++) $ programEnv env prog
    return $ Scope ctx' env'

{- | the builtins plus the installed prelude.hmmm;
the file is found through cabal's data-files, which can be overridden with
the hmmm_lang_datadir environment variable
-}
loadPrelude :: IO (Either String Scope)
loadPrelude = do
    path <- getDataFileName "lib/prelude.hmmm"
    try (readFile path) >>= \case
        Left e -> return $ Left ("could not read the prelude: " ++ show (e :: IOException))
        Right source -> return $ withPrelude builtinScope source
