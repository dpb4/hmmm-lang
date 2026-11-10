module Inference where

type Name = String

data Expr
    = EVar Name
    | EIf Expr Expr Expr
    | EAbs Name Expr
    | EApp Expr Expr
    | EOp Operator Expr Expr
    | EVal Value
    deriving (Show)

data Operator
    = OAdd
    | OSub
    deriving (Show)

data Value
    = VInt Integer
    | VBool Bool
    | VStr String
    deriving (Show)

data ConcreteType = CInt | CBool | CStr deriving (Show)

data Type
    = TLit ConcreteType
    | TVar Int
    | TFun Type Type
    deriving (Show)
