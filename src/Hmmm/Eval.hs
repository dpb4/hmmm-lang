{-# LANGUAGE LambdaCase #-}

module Hmmm.Eval (
    eval,
    runEval,
    runEvalProgram,
    Value (..),
) where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as M
import Hmmm.Syntax

type Env = M.Map Name Value

data Value
    = VInt Integer
    | VBool Bool
    | VStr String
    | VClosure Env Name Expr
    deriving (Eq)

instance Show Value where
    show (VInt i) = show i
    show (VBool i) = show i
    show (VStr i) = show i
    show (VClosure _ p b) = "λ" ++ p ++ " -> " ++ show b

type Eval a = ReaderT Env (Except String) a

runEval :: Expr -> Either String Value
runEval e = runExcept (runReaderT (eval e) M.empty)

runEvalProgram :: [TopExpr] -> Either String [Value]
runEvalProgram prog = runExcept (runReaderT (evalProgram prog) M.empty)

evalProgram :: [TopExpr] -> Eval [Value]
evalProgram [] = return []
evalProgram (top : rest) = case top of
    TELet (Bind name _) val -> define name val
    TEFun (Bind name _) params body -> define name (foldr EAbs body params)
    TEExpr e -> (:) <$> eval e <*> evalProgram rest
  where
    define name val = do
        v <- eval val
        local (M.insert name v) (evalProgram rest)

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

evalAbs :: Binding -> Expr -> Eval Value
evalAbs (Bind param _) body = asks (\env -> VClosure env param body)

evalLet :: Binding -> Expr -> Expr -> Eval Value
evalLet (Bind vid _) expr body = do
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
    ( case o of
            OAnd -> evalOpAnd ae be
            OOr -> evalOpOr ae be
            _ -> do
                a <- eval ae
                b <- eval be
                case o of
                    OAdd -> evalOpAdd a b
                    OSub -> evalOpSub a b
                    OEq -> evalOpEq a b
                    ONeq -> evalOpNeq a b
                    OGt -> evalOpGt a b
                    OGeq -> evalOpGeq a b
                    OLt -> evalOpLt a b
                    OLeq -> evalOpLeq a b
        )

evalOpAdd :: Value -> Value -> Eval Value
evalOpAdd (VInt a) (VInt b) = return $ VInt (a + b)
evalOpAdd x y = throwError ("(+) requires two integers (got " ++ show x ++ ", " ++ show y ++ ")")

evalOpSub :: Value -> Value -> Eval Value
evalOpSub (VInt a) (VInt b) = return $ VInt (a - b)
evalOpSub x y = throwError ("(-) requires two integers (got " ++ show x ++ ", " ++ show y ++ ")")

evalOpEq :: Value -> Value -> Eval Value
evalOpEq (VInt a) (VInt b) = return $ VBool (a == b)
evalOpEq (VBool a) (VBool b) = return $ VBool (a == b)
evalOpEq (VStr a) (VStr b) = return $ VBool (a == b)
evalOpEq _ _ = return $ VBool False

evalOpNeq :: Value -> Value -> Eval Value
evalOpNeq (VInt a) (VInt b) = return $ VBool (a /= b)
evalOpNeq (VBool a) (VBool b) = return $ VBool (a /= b)
evalOpNeq (VStr a) (VStr b) = return $ VBool (a /= b)
evalOpNeq _ _ = return $ VBool True

evalOpGt :: Value -> Value -> Eval Value
evalOpGt (VInt a) (VInt b) = return $ VBool (a > b)
evalOpGt x y = throwError ("(>) requires two integers (got " ++ show x ++ ", " ++ show y ++ ")")

evalOpGeq :: Value -> Value -> Eval Value
evalOpGeq (VInt a) (VInt b) = return $ VBool (a >= b)
evalOpGeq x y = throwError ("(>=) requires two integers (got " ++ show x ++ ", " ++ show y ++ ")")

evalOpLt :: Value -> Value -> Eval Value
evalOpLt (VInt a) (VInt b) = return $ VBool (a < b)
evalOpLt x y = throwError ("(<) requires two integers (got " ++ show x ++ ", " ++ show y ++ ")")

evalOpLeq :: Value -> Value -> Eval Value
evalOpLeq (VInt a) (VInt b) = return $ VBool (a <= b)
evalOpLeq x y = throwError ("(<=) requires two integers (got " ++ show x ++ ", " ++ show y ++ ")")

evalOpAnd :: Expr -> Expr -> Eval Value
evalOpAnd a b = do
    a' <- eval a
    case a' of
        VBool True -> do
            b' <- eval b
            case b' of
                VBool _ -> return b'
                _ -> throwError ("(and) requires bools (got " ++ show b' ++ ")")
        VBool False -> return a'
        _ -> throwError ("(and) requires bools (got " ++ show a' ++ ")")

evalOpOr :: Expr -> Expr -> Eval Value
evalOpOr a b = do
    a' <- eval a
    case a' of
        VBool False -> do
            b' <- eval b
            case b' of
                VBool _ -> return b'
                _ -> throwError ("(or) requires bools (got " ++ show b' ++ ")")
        VBool True -> return a'
        _ -> throwError ("(or) requires bools (got " ++ show a' ++ ")")

evalLit :: Literal -> Eval Value
evalLit l = return $ case l of
    LInt i -> VInt i
    LBool b -> VBool b
    LStr s -> VStr s
