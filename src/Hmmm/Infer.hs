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
) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.RWS
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
    EAbs param body -> do
        paramt <- fresh
        let paramt' = ForAll S.empty paramt
        bodyt <- local (M.insert param paramt') (infer body)
        return $ TFun paramt bodyt
    ELet vid val body -> do
        (valt, cs) <- listen (infer val)
        s <- either throwError return $ runSolve cs
        ctx <- asks (applySubst s)
        let valtg = generalize ctx (applySubst s valt)
        bodyt <- local (M.insert vid valtg . applySubst s) (infer body)
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

runInfer :: Expr -> Either String (Type, FreeVarCount, [Constraint])
runInfer i = runIdentity $ runExceptT $ runRWST (infer i) M.empty 0

-- | infer the constraints of an expression, solve them, and return its type
typeOf :: Expr -> Either String Type
typeOf e = do
    (t, _, cs) <- runInfer e
    s <- runSolve cs
    return (applySubst s t)
