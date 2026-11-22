-- | the abstract syntax tree shared by every phase
module Hmmm.Syntax (
    Name,
    Expr (..),
    Operator (..),
    Literal (..),
    Binding (..),
    TopExpr (..),
) where

import Data.List
import Hmmm.Types

type Name = String

data Binding = Bind Name (Maybe Type) deriving (Eq)

data Expr
    = EVar Name
    | EIf Expr Expr Expr
    | EAbs Binding Expr
    | ELet Binding Expr Expr
    | EApp Expr Expr
    | EOp Operator Expr Expr
    | ELit Literal
    | EList (List Expr)
    deriving (Eq)

data Operator
    = OAdd
    | OSub
    | OEq
    | ONeq
    | OGt
    | OGeq
    | OLt
    | OLeq
    | OAnd
    | OOr
    | OCons
    | OAppend
    deriving (Eq)

instance Show Operator where
    show OAdd = "+"
    show OSub = "-"
    show OEq = "=="
    show ONeq = "/="
    show OGt = ">"
    show OGeq = ">="
    show OLt = "<"
    show OLeq = "<="
    show OAnd = "and"
    show OOr = "or"
    show OCons = "++"
    show OAppend = "+++"

instance Show Binding where
    show (Bind n (Just t)) = n ++ ": " ++ show t
    show (Bind n (Nothing)) = n

instance Show Expr where
    show e = case e of
        EVar n -> n
        EIf c a b -> "if " ++ show c ++ " then " ++ show a ++ " else " ++ show b
        EAbs p b -> "λ" ++ show p ++ " -> " ++ show b
        ELet i v b -> "let " ++ show i ++ " = " ++ show v ++ " in " ++ show b
        EApp f a -> show f ++ "(" ++ show a ++ ")"
        EOp o a b -> show a ++ " " ++ show o ++ " " ++ show b
        ELit l -> show l
        EList l -> show l

{- | a literal value as written in source code;
string literals are parsed into lists of chars, so they have no literal of their own
-}
data Literal
    = LInt Integer
    | LBool Bool
    | LChar Char
    deriving (Show, Eq)

data TopExpr
    = TEFun Binding (List Binding) Expr
    | TELet Binding Expr
    | TEExpr Expr
    deriving (Show, Eq)
