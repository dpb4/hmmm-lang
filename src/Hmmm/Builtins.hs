{-# LANGUAGE LambdaCase #-}

-- | functions that are in scope in every program
module Hmmm.Builtins (
    builtins,
    builtinTypes,
    builtinEnv,
) where

import Control.Monad.Except
import qualified Data.Map as M
import Hmmm.Eval
import Hmmm.Infer (Context, generalize)
import Hmmm.Syntax
import Hmmm.Types

{- | builtin functions;
note that @a@ is an abritrary type variable to be generalized over to make these polymorphic
-}
builtins :: [(Name, Polytype, Value)]
builtins =
    [ builtin "head" (list a ~> a) $ \case
        VList (x : _) -> return x
        VList [] -> throwError "head of an empty list"
        v -> notAList "head" v
    , builtin "tail" (list a ~> list a) $ \case
        VList (_ : xs) -> return $ VList xs
        VList [] -> throwError "tail of an empty list"
        v -> notAList "tail" v
    , builtin "empty?" (list a ~> bool) $ \case
        VList xs -> return $ VBool (null xs)
        v -> notAList "empty?" v
    ]
  where
    a = TVar (TV "a")
    bool = TLit CBool
    list = TCon . TCList
    x ~> y = TCon (TCFun x y)
    infixr 5 ~>

-- | a builtin with its type generalized over all of its type variables
builtin :: Name -> TypeMaybe -> (Value -> Eval Value) -> (Name, Polytype, Value)
builtin name t f = (name, generalize M.empty t, VPrim name f)

notAList :: Name -> Value -> Eval Value
notAList name v = throwError ("(" ++ name ++ ") requires a list (got " ++ show v ++ ")")

-- | the starting context for type inference
builtinTypes :: Context
builtinTypes = M.fromList [(n, t) | (n, t, _) <- builtins]

-- | the starting environment for evaluation
builtinEnv :: Env
builtinEnv = M.fromList [(n, v) | (n, _, v) <- builtins]
