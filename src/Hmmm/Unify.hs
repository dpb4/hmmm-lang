-- | solving equality constraints by unification
module Hmmm.Unify (
    Solve,
    unify,
    runSolve,
) where

import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Set as S
import Hmmm.Types

type Solve a = Except String a

unify :: Type -> Type -> Solve Subst
unify a b | a == b = return M.empty
unify (TVar v) t = bind v t
unify t (TVar v) = bind v t
unify (TFun in1 out1) (TFun in2 out2) = do
    ins <- unify in1 in2
    outs <- unify (applySubst ins out1) (applySubst ins out2)
    return $ compose outs ins
unify x y = throwError $ "Misc type mismatch " ++ show x ++ " and " ++ show y

bind :: TypeVar -> Type -> Solve Subst
bind v t
    | S.member v $ freeTypeVars t = throwError $ "Infinite type " ++ show v ++ " ~ " ++ show t -- Occurs check
    | otherwise = return $ M.singleton v t

solve :: Subst -> [Constraint] -> Solve Subst
solve s [] = return s
solve s ((Constraint t1 t2) : cs) = do
    s1 <- unify t1 t2
    solve (s1 `compose` s) (applySubst s1 cs)

runSolve :: [Constraint] -> Either String Subst
runSolve = runExcept . solve M.empty
