{-# LANGUAGE LambdaCase #-}

-- | constraint-generating type inference
module Hmmm.Infer (
    Context,
    FreeVarCount,
    Infer,
    infer,
    runInfer,
    generalize,
    typeOf,
    typeOfProgram,
) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.RWS
import Data.Foldable (traverse_)
import qualified Data.Map as M
import qualified Data.Set as S
import Hmmm.Syntax
import Hmmm.Types
import Hmmm.Unify (runSolve)

type Context = M.Map Name Polytype

type FreeVarCount = Int

{- | RWS Monad with:
Reader: typing context
Writer: list of equality constraints
State:  count of bound type variables
-}
type Infer t = RWST Context [Constraint] FreeVarCount (Except String) t

-- | universal closure over Type t
generalize :: Context -> TypeMaybe -> Polytype
generalize ctx t = ForAll vs t
  where
    ctxvs = freeTypeVars $ M.elems ctx
    tvs = freeTypeVars t
    vs = S.difference tvs ctxvs

{- | replace quantified variables with fresh type variables
eg. ForAll a, a -> a => t0 -> t0
which could then become Int -> Int
-}
instantiate :: Polytype -> Infer TypeMaybe
instantiate (ForAll qvs t) = do
    let qvsl = S.toList qvs
    ftvs <- traverse (const fresh) qvsl
    let subst = M.fromList (zip qvsl ftvs)
    return $ applySubst subst t

-- | create a new inference with constraint a=b
constrain :: TypeMaybe -> TypeMaybe -> Infer ()
constrain a b = tell [Constraint a b]

-- | get a new, free type variable
fresh :: Infer TypeMaybe
fresh = do
    count <- get
    modify (+ 1)
    return . TVar . TV $ show count

tfun :: TypeMaybe -> TypeMaybe -> TypeMaybe
tfun a b = TCon $ TCFun a b

inferBinding :: Binding -> Infer (Name, TypeMaybe)
inferBinding (Bind n mt) = case mt of
    Just t -> return $ (n, typeToMaybe t)
    Nothing -> do
        t <- fresh
        return (n, t)

{- | infer a let-bound value, solve its constraints, and generalize it;
returns the bound name, its polytype, and the substitution to apply to the context
-}
inferDefinition :: Binding -> Infer TypeMaybe -> Infer (Name, Polytype, Subst)
inferDefinition bind inferVal = do
    (vid, vidt) <- inferBinding bind
    -- the annotation must be solved with val's constraints, before generalizing
    (valt, cs) <- listen $ do
        t <- inferVal
        constrain vidt t
        return t
    s <- either throwError return $ runSolve cs
    ctx <- asks (applySubst s)
    return (vid, generalize ctx (applySubst s valt), s)

-- | infer a top-level function: params -> ... -> body, with an optional return type
inferFun :: [Binding] -> Maybe Type -> Expr -> Infer TypeMaybe
inferFun params rett body = do
    ps <- traverse inferBinding params
    let mono = M.fromList [(n, ForAll S.empty t) | (n, t) <- ps]
    bodyt <- local (M.union mono) (infer body)
    traverse_ (constrain bodyt . typeToMaybe) rett
    return $ foldr (tfun . snd) bodyt ps

inferOp :: Operator -> Infer TypeMaybe
inferOp = \case
    OAdd -> intIntToInt
    OSub -> intIntToInt
    OEq -> aaToBool
    ONeq -> aaToBool
    OGt -> intIntToBool
    OGeq -> intIntToBool
    OLt -> intIntToBool
    OLeq -> intIntToBool
    OAnd -> boolBoolToBool
    OOr -> boolBoolToBool
  where
    int = TLit CInt
    bool = TLit CBool
    intIntToInt = return $ tfun int $ tfun int int
    intIntToBool = return $ tfun int $ tfun int bool
    boolBoolToBool = return $ tfun bool $ tfun bool bool
    -- both sides of an equality must have the same type
    aaToBool = do
        a <- fresh
        return $ tfun a $ tfun a bool

-- | infer the type of an expression
infer :: Expr -> Infer TypeMaybe
infer = \case
    ELit l -> return $ TLit $ case l of
        LInt _ -> CInt
        LBool _ -> CBool
        LStr _ -> CStr
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
    EAbs bind body -> do
        (param, paramt) <- inferBinding bind
        let paramt' = ForAll S.empty paramt
        bodyt <- local (M.insert param paramt') (infer body)
        return $ tfun paramt bodyt
    ELet bind val body -> do
        (vid, valtg, s) <- inferDefinition bind (infer val)
        local (M.insert vid valtg . applySubst s) (infer body)
    EApp fun arg -> do
        funt <- infer fun
        argt <- infer arg
        rett <- fresh
        constrain funt $ tfun argt rett
        return rett
    EOp op a b -> do
        opt <- inferOp op
        at <- infer a
        bt <- infer b
        rett <- fresh
        constrain opt $ tfun at $ tfun bt rett
        return rett

runInfer :: Expr -> Either String (TypeMaybe, FreeVarCount, [Constraint])
runInfer i = runIdentity $ runExceptT $ runRWST (infer i) M.empty 0

-- | infer the constraints of an expression, solve them, and return its type
typeOf :: Expr -> Either String TypeMaybe
typeOf e = do
    (t, _, cs) <- runInfer e
    s <- runSolve cs
    return (applySubst s t)

inferProgram :: [TopExpr] -> Infer [TypeMaybe]
inferProgram [] = return []
inferProgram (top : rest) = case top of
    TELet bind val -> define bind (infer val)
    TEFun (Bind name rett) params body -> define (Bind name Nothing) (inferFun params rett body)
    TEExpr e -> do
        (t, cs) <- listen (infer e)
        s <- either throwError return $ runSolve cs
        (applySubst s t :) <$> inferProgram rest
  where
    define bind val = do
        (name, t, s) <- inferDefinition bind val
        local (M.insert name t . applySubst s) (inferProgram rest)

-- | the type of each top-level expression in a program, in order
typeOfProgram :: [TopExpr] -> Either String [TypeMaybe]
typeOfProgram prog = do
    (ts, _, _) <- runIdentity $ runExceptT $ runRWST (inferProgram prog) M.empty 0
    return ts
