{-# LANGUAGE LambdaCase #-}

module Inference where

import Control.Monad.Except
import Control.Monad.RWS
import qualified Data.Map as M
import qualified Data.Set as S

type Name = String

data Expr
    = EVar Name
    | EIf Expr Expr Expr
    | EAbs Name Expr
    | ELet Name Expr Expr
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

data ConcreteType = CInt | CBool | CStr deriving (Eq, Show)
newtype TypeVar = TV Name deriving (Eq, Show, Ord)

data Type
    = TLit ConcreteType
    | TVar TypeVar
    | TFun Type Type
    deriving (Eq, Show)

data Polytype = ForAll (S.Set TypeVar) Type deriving (Show)
data Constraint = Constraint Type Type deriving (Show)

type Context = M.Map Name Polytype
type FreeVarCount = Int

{- | RWS Monad with:
Reader: typing context
Writer: list of equality constraints
State:  count of bound type variables
-}
type Infer t = RWST Context [Constraint] FreeVarCount (Except String) t

{- | a map of type vars to actual assigned types
this can be applied to any Substitutable
-}
type Subst = M.Map TypeVar Type

-- | an object to which substitutions can be applied, and which may have free type variables
class Substitutable a where
    applySubst :: Subst -> a -> a
    freeTypeVars :: a -> S.Set TypeVar

instance Substitutable Type where
    applySubst s t = case t of
        l@(TLit _) -> l
        tv@(TVar v) -> M.findWithDefault tv v s
        TFun i o -> TFun i' o'
          where
            i' = (applySubst s i)
            o' = (applySubst s o)

    freeTypeVars t = case t of
        TLit _ -> S.empty
        TVar v -> S.singleton v
        TFun i o -> S.union i' o'
          where
            i' = (freeTypeVars i)
            o' = (freeTypeVars o)

instance Substitutable Polytype where
    applySubst s (ForAll qvs t) = ForAll qvs $ applySubst unquantified t
      where
        unquantified = (foldr M.delete s qvs)
    freeTypeVars (ForAll qvs t) = S.difference qvs $ freeTypeVars t

instance Substitutable Constraint where
    applySubst s (Constraint t1 t2) = Constraint t1' t2'
      where
        t1' = applySubst s t1
        t2' = applySubst s t2
    freeTypeVars (Constraint t1 t2) = S.union tvs1 tvs2
      where
        tvs1 = freeTypeVars t1
        tvs2 = freeTypeVars t2

instance (Substitutable a) => Substitutable [a] where
    applySubst s = map (applySubst s)
    freeTypeVars l = foldr (S.union . freeTypeVars) S.empty l

compose :: Subst -> Subst -> Subst
compose a b = M.map (applySubst a) (M.union a b)

-- | create a new inference with constraint a=b
constrain :: Type -> Type -> Infer ()
constrain a b = tell [Constraint a b]

-- | get a new, free type variable
fresh :: Infer Type
fresh = do
    count <- get
    modify (+ 1)
    return . TVar . TV $ show count

-- | infer the type of an expression
infer :: Expr -> Infer Type
infer = \case
    EVal v -> return $ TLit $ case v of
        VInt _ -> CInt
        VBool _ -> CBool
        VStr _ -> CStr
    EVar n -> throwError "TODO"
    EIf cond tCase fCase -> do
        condT <- infer cond
        tCaseT <- infer tCase
        fCaseT <- infer fCase

        constrain condT $ TLit CBool
        constrain tCaseT fCaseT
        return tCaseT
    EAbs p b -> throwError "TODO"
    ELet id val body -> throwError "TODO"
    EApp f a -> throwError "TODO"
    EOp o a b -> throwError "TODO"

runInfer :: Expr -> Except String (Type, FreeVarCount, [Constraint])
runInfer i = runRWST (infer i) M.empty 0
