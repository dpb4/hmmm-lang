-- | type representations and substitutions, shared by inference and unification
module Hmmm.Types (
    ConcreteType (..),
    TypeVar (..),
    Type (..),
    Polytype (..),
    Constraint (..),
    Subst,
    Substitutable (..),
    compose,
) where

import qualified Data.Map as M
import qualified Data.Set as S
import Hmmm.Syntax (Name)

data ConcreteType = CInt | CBool | CStr deriving (Eq)

instance Show ConcreteType where
    show CInt = "Int"
    show CBool = "Bool"
    show CStr = "Str"

newtype TypeVar = TV Name deriving (Eq, Ord)

instance Show TypeVar where
    show (TV n) = "t" ++ n

{- | Type (aka Monotype) is one of:
TLit ConcreteType - a known primitive type
TVar TypeVar      - an unknown type
TFun Type Type    - a function with signature Type -> Type
-}
data Type
    = TLit ConcreteType
    | TVar TypeVar
    | TFun Type Type
    deriving (Eq)

instance Show Type where
    show (TLit ct) = show ct
    show (TVar tv) = show tv
    show (TFun a b) = show a ++ " -> " ++ show b

-- | a Polytype is a type with 0 or more universally quantified Type variables
data Polytype = ForAll (S.Set TypeVar) Type

instance Show Polytype where
    show (ForAll qvs t) = "∀ " ++ show qvs ++ ", " ++ show t

data Constraint = Constraint Type Type

instance Show Constraint where
    show (Constraint a b) = show a ++ " c= " ++ show b

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
    freeTypeVars (ForAll qvs t) = S.difference (freeTypeVars t) qvs

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

instance (Substitutable a) => Substitutable (M.Map k a) where
    applySubst s = M.map (applySubst s)
    freeTypeVars = freeTypeVars . M.elems

-- | compose s1 s2 applies s2 first, then s1
compose :: Subst -> Subst -> Subst
compose s1 s2 = M.map (applySubst s1) s2 `M.union` s1
