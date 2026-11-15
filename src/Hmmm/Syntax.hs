-- | the abstract syntax tree shared by every phase
module Hmmm.Syntax (
    Name,
    Expr (..),
    Operator (..),
    Literal (..),
) where

type Name = String

data Expr
    = EVar Name
    | EIf Expr Expr Expr
    | EAbs Name Expr
    | ELet Name Expr Expr
    | EApp Expr Expr
    | EOp Operator Expr Expr
    | ELit Literal

data Operator
    = OAdd
    | OSub

instance Show Operator where
    show OAdd = "+"
    show OSub = "-"

instance Show Expr where
    show e = case e of
        EVar n -> show n
        EIf c a b -> "if " ++ show c ++ " then " ++ show a ++ " else " ++ show b
        EAbs p b -> "λ" ++ show p ++ " -> " ++ show b
        ELet i v b -> "let " ++ show i ++ " = " ++ show v ++ " in " ++ show b
        EApp f a -> show f ++ "(" ++ show a ++ ")"
        EOp o a b -> show a ++ " " ++ show o ++ " " ++ show b
        ELit l -> show l

-- | a literal value as written in source code
data Literal
    = LInt Integer
    | LBool Bool
    | LStr String
    deriving (Show)
