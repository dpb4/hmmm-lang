{-# LANGUAGE LambdaCase #-}

module Hmmm.Eval (
    eval,
    runEval,
    Value,
) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Map as M
import Hmmm.Syntax

type Env = M.Map Name Value

data Value
    = VInt Integer
    | VBool Bool
    | VStr String
    | VClosure Env Name Expr

instance Show Value where
    show (VInt i) = show i
    show (VBool i) = show i
    show (VStr i) = show i
    show (VClosure _ p b) = "λ" ++ p ++ " -> " ++ show b

type Eval a = ReaderT Env (Except String) a

runEval :: Expr -> Either String Value
runEval e = runExcept (runReaderT (eval e) M.empty)

eval :: Expr -> Eval Value
eval = \case
    EVar v -> evalVar v
    EIf c a b -> evalIf c a b
    EAbs p b -> evalAbs p b
    ELet i v b -> evalLet i v b
    EApp f a -> evalApp f a
    EOp o a b -> evalOp o a b
    ELit l -> evalLit l

evalVar :: Name -> Eval Value
evalVar n = asks (M.lookup n) >>= maybe err return
  where
    err = throwError $ "unbound variable " ++ n

evalIf :: Expr -> Expr -> Expr -> Eval Value
evalIf cond a b =
    eval cond >>= \case
        VBool True -> eval a
        VBool False -> eval b
        v -> throwError ("if condition must be a bool (got " ++ show v ++ ")")

evalAbs :: Name -> Expr -> Eval Value
evalAbs param body = asks (\env -> VClosure env param body)

evalLet :: Name -> Expr -> Expr -> Eval Value
evalLet vid expr body = do
    v <- eval expr
    local (M.insert vid v) (eval body)

evalApp :: Expr -> Expr -> Eval Value
evalApp fune arge = do
    fun <- eval fune
    arg <- eval arge
    case fun of
        VClosure cenv param body -> local (const (M.insert param arg cenv)) (eval body)
        _ -> throwError "applying a non-function to an argument"

evalOp :: Operator -> Expr -> Expr -> Eval Value
evalOp o ae be = do
    a <- eval ae
    b <- eval be
    case (a, b) of
        (VInt ai, VInt bi) -> return $ case o of
            OAdd -> VInt $ ai + bi
            OSub -> VInt $ ai - bi
        -- TODO split op then type for better error messages
        _ -> throwError "expected two integers"

evalLit :: Literal -> Eval Value
evalLit l = return $ case l of
    LInt i -> VInt i
    LBool b -> VBool b
    LStr s -> VStr s
