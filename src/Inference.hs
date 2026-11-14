{-# LANGUAGE LambdaCase #-}

module Inference where

import Control.Monad.Except
import Control.Monad.Identity
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
compose a b = M.map (applySubst b) (M.union a b)

-- | universal closure over Type t
generalize :: Context -> Type -> Polytype
generalize ctx t = ForAll vs t
  where
    ctxvs = freeTypeVars $ M.elems ctx
    tvs = freeTypeVars t
    vs = S.difference tvs ctxvs

{- | replace quantified variables with fresh type variables
eg. ForAll a, a -> a => t0 -> t0
which could then become Int -> Int
-}
instantiate :: Polytype -> Infer Type
instantiate (ForAll qvs t) = do
    let qvsl = S.toList qvs
    ftvs <- traverse (const fresh) qvsl
    let subst = M.fromList (zip qvsl ftvs)
    return $ applySubst subst t

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
    EVar name -> do
        ctx <- ask
        case M.lookup name ctx of
            Just t -> instantiate t
            Nothing -> throwError $ "Undefined variable " ++ name
    EIf cond tCase fCase -> do
        condT <- infer cond
        tCaseT <- infer tCase
        fCaseT <- infer fCase

        constrain condT $ TLit CBool
        constrain tCaseT fCaseT
        return tCaseT
    EAbs param body -> do
        paramt <- fresh
        let paramt' = ForAll S.empty paramt
        bodyt <- local (M.insert param paramt') (infer body)
        return $ TFun paramt bodyt
    ELet vid val body -> do
        valt <- infer val
        ctx <- ask
        let valtg = generalize ctx valt
        bodyt <- local (M.insert vid valtg) (infer body)
        return bodyt
    EApp fun arg -> do
        funt <- infer fun
        argt <- infer arg
        rett <- fresh
        constrain funt $ TFun argt rett
        return rett
    EOp op a b -> do
        let opt =
                ( let int = TLit CInt
                   in case op of
                        OAdd -> TFun int $ TFun int int
                        OSub -> TFun int $ TFun int int
                )
        at <- infer a
        bt <- infer b
        rett <- fresh
        constrain opt $ TFun at $ TFun bt rett
        return rett

type Solve a = Except String a

unify :: Type -> Type -> Solve Subst
unify a b | a == b = return M.empty -- Same
unify (TVar v) t = bind v t -- Var Left
unify t (TVar v) = bind v t -- Var Right
unify (TFun in1 out1) (TFun in2 out2) = do
    ins <- unify in1 in2
    outs <- unify out1 out2
    return $ compose outs ins
unify x y = throwError $ "Misc type mismatch " ++ show x ++ " and " ++ show y

-- unify a@(TFun in1 out1) b@(TFun in2 out2) -- Arrow (->) / other TCons
--     | a /= b = throwError $ "Type mismatch " ++ show a ++ " and " ++ show b
--     | otherwise = do
--         ins <- unify in1 in2
--         outs <- unify out1 out2
--         return $ compose outs ins
-- unify x y = throwError $ "Misc type mismatch " ++ show x ++ " and " ++ show y

bind :: TypeVar -> Type -> Solve Subst
bind v t
    | S.member v $ freeTypeVars t = throwError $ "Infinite type " ++ show v ++ " ~ " ++ show t -- Occurs check
    | otherwise = return $ M.singleton v t

runInfer :: Expr -> Either String (Type, FreeVarCount, [Constraint])
runInfer i = runIdentity $ runExceptT $ runRWST (infer i) M.empty 0

solve :: Subst -> [Constraint] -> Solve Subst
solve s [] = return s
solve s ((Constraint t1 t2) : cs) = do
    s1 <- unify t1 t2
    solve (s1 `compose` s) (applySubst s1 cs)

runSolve :: [Constraint] -> Either String Subst
runSolve = runExcept . solve M.empty

doAll :: Expr -> Either String Subst
doAll e = do
    (_, _, cs) <- runInfer e
    s <- runSolve cs
    return s

exp1 :: Expr
exp1 = ELet "foo" fun rest
  where
    fun = EAbs "x" (EOp OAdd (EVar "x") (EVal $ VInt 3))
    rest = EApp (EVar "foo") (EVal $ VInt 5)

exp2 :: Expr
exp2 = ELet "foo" fun rest
  where
    fun = EAbs "x" (EOp OAdd (EVar "x") (EVal $ VInt 3))
    rest = EApp (EVar "foo") (EVal $ VStr "c")

doFormat :: Expr -> IO ()
doFormat e =
    let er = runInfer e
     in case er of
            Left s -> print s
            Right (et, _, cs) -> do
                print et
                mapM_ print cs
