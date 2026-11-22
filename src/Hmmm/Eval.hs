{-# LANGUAGE LambdaCase #-}

module Hmmm.Eval (
    Env,
    Eval,
    eval,
    apply,
    runEval,
    runEvalProgram,
    display,
    Value (..),
) where

import Control.Monad (zipWithM)
import Control.Monad.Except
import Control.Monad.Reader
import Data.List
import qualified Data.Map as M
import Hmmm.Syntax
import Hmmm.Types

type Env = M.Map Name Value

data Value
    = VInt Integer
    | VBool Bool
    | VChar Char
    | VClosure Env Name Expr
    | VList (List Value)
    | -- | a builtin function, implemented in Haskell
      VPrim Name (Value -> Eval Value)

{- | structural equality for tests;
builtins are compared by name
hmmm's @==@ uses evalOpEq, wherein functions are never equal
-}
instance Eq Value where
    VInt a == VInt b = a == b
    VBool a == VBool b = a == b
    VChar a == VChar b = a == b
    VClosure env p body == VClosure env' p' body' = (env, p, body) == (env', p', body')
    VList a == VList b = a == b
    VPrim a _ == VPrim b _ = a == b
    _ == _ = False

instance Show Value where
    show (VInt i) = show i
    show (VBool i) = show i
    show (VChar c) = show c
    show (VClosure _ p b) = "λ" ++ p ++ " -> " ++ show b
    show (VList l) = show l
    show (VPrim n _) = "<builtin " ++ n ++ ">"

{- | show a value using its type;
this is necessary because a value doesn't know its own type, so this can disambiguate empty lists from empty strings
-}
display :: TypeMaybe -> Value -> String
display t v = case (t, v) of
    (TCon (TCList (TLit CChar)), VList cs) -> show [c | VChar c <- cs]
    (TCon (TCList et), VList vs) -> "[" ++ intercalate ", " (map (display et) vs) ++ "]"
    _ -> show v

type Eval a = ReaderT Env (Except String) a

-- | evaluate an expression, starting from the given environment (eg. the builtins)
runEval :: Env -> Expr -> Either String Value
runEval env e = runExcept (runReaderT (eval e) env)

-- | evaluate a program, starting from the given environment (eg. the builtins)
runEvalProgram :: Env -> [TopExpr] -> Either String [Value]
runEvalProgram env prog = runExcept (runReaderT (evalProgram prog) env)

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
    EList es -> evalList es

evalList :: (List Expr) -> Eval Value
evalList es = traverse eval es >>= (return . VList)

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
    apply fun arg

-- | apply a function value to an argument
apply :: Value -> Value -> Eval Value
apply fun arg = case fun of
    VClosure cenv param body -> local (const (M.insert param arg cenv)) (eval body)
    VPrim _ f -> f arg
    _ -> throwError ("applying a non-function " ++ show fun ++ " to an argument")

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
                    OCons -> evalOpCons a b
                    OAppend -> evalOpAppend a b
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
evalOpEq (VChar a) (VChar b) = return $ VBool (a == b)
-- lists are equal if they have the same length and each pair of elements is equal
evalOpEq (VList a) (VList b)
    | length a /= length b = return $ VBool False
    | otherwise = VBool . and <$> zipWithM (\x y -> isTrue <$> evalOpEq x y) a b
  where
    isTrue = (== VBool True)
-- two functions are never equal, even if they are the same closure
evalOpEq (VClosure{}) (VClosure{}) = return $ VBool False
evalOpEq (VPrim{}) (VPrim{}) = return $ VBool False
evalOpEq _ _ = return $ VBool False

evalOpNeq :: Value -> Value -> Eval Value
evalOpNeq a b =
    evalOpEq a b >>= \case
        VBool eq -> return $ VBool (not eq)
        v -> throwError ("(==) returned a non-bool (SHOULD NOT HAPPEN)" ++ show v)

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

evalOpCons :: Value -> Value -> Eval Value
evalOpCons e (VList l) = return $ VList (e : l)
evalOpCons x y = throwError ("(++) requires an element and a list (got " ++ show x ++ ", " ++ show y ++ ")")

evalOpAppend :: Value -> Value -> Eval Value
evalOpAppend (VList l) (VList r) = return $ VList (l ++ r)
evalOpAppend x y = throwError ("(+++) requires two lists (got " ++ show x ++ ", " ++ show y ++ ")")

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
    LChar c -> VChar c
