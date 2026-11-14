module Main (main) where

import Control.Monad.Except (runExcept)
import Data.Either (isLeft)
import qualified Data.Map as M
import qualified Data.Set as S
import Inference
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Inference"
        [ substitutionTests
        , unifyTests
        , inferTests
        , endToEndTests
        ]

-- helpers

tv :: Name -> TypeVar
tv = TV

var :: Name -> Type
var = TVar . tv

int, bool, str :: Type
int = TLit CInt
bool = TLit CBool
str = TLit CStr

(~>) :: Type -> Type -> Type
(~>) = TFun
infixr 5 ~>

runUnify :: Type -> Type -> Either String Subst
runUnify a b = runExcept (unify a b)

-- let foo = \x -> x + 3 in foo 5
exp1 :: Expr
exp1 = ELet "foo" fun rest
  where
    fun = EAbs "x" (EOp OAdd (EVar "x") (EVal $ VInt 3))
    rest = EApp (EVar "foo") (EVal $ VInt 5)

-- let foo = \x -> x + 3 in foo "c"
exp2 :: Expr
exp2 = ELet "foo" fun rest
  where
    fun = EAbs "x" (EOp OAdd (EVar "x") (EVal $ VInt 3))
    rest = EApp (EVar "foo") (EVal $ VStr "c")

assertLeft :: (Show b) => String -> Either a b -> Assertion
assertLeft msg e = assertBool (msg ++ ", got " ++ either (const "Left") show e) (isLeft e)

-- substitution

substitutionTests :: TestTree
substitutionTests =
    testGroup
        "Substitution"
        [ testCase "applySubst replaces variables inside functions" $
            applySubst (M.fromList [(tv "a", int)]) (var "a" ~> var "b")
                @?= int ~> var "b"
        , testCase "compose s1 s2 applies s2 first, then s1" $ do
            let s1 = M.fromList [(tv "b", int)]
                s2 = M.fromList [(tv "a", var "b")]
                s = compose s1 s2
            applySubst s (var "a") @?= int
            applySubst s (var "b") @?= int
        , testCase "free vars of a polytype exclude quantified vars" $
            freeTypeVars (ForAll (S.singleton (tv "a")) (var "a" ~> var "b"))
                @?= S.singleton (tv "b")
        , testCase "applySubst on a polytype leaves quantified vars alone" $ do
            let ForAll _ t =
                    applySubst
                        (M.fromList [(tv "a", int), (tv "b", bool)])
                        (ForAll (S.singleton (tv "a")) (var "a" ~> var "b"))
            t @?= var "a" ~> bool
        , testCase "generalize quantifies only vars not free in the context" $ do
            let ctx = M.fromList [("y", ForAll S.empty (var "b"))]
                ForAll qvs _ = generalize ctx (var "a" ~> var "b")
            qvs @?= S.singleton (tv "a")
        ]

-- unification

unifyTests :: TestTree
unifyTests =
    testGroup
        "Unification"
        [ testCase "identical types give empty substitution" $
            runUnify (int ~> bool) (int ~> bool) @?= Right M.empty
        , testCase "variable on the left" $
            runUnify (var "a") int @?= Right (M.fromList [(tv "a", int)])
        , testCase "variable on the right" $
            runUnify int (var "a") @?= Right (M.fromList [(tv "a", int)])
        , testCase "mismatched literals fail" $
            assertLeft "Int ~ Bool should fail" (runUnify int bool)
        , testCase "function vs literal fails" $
            assertLeft "Int -> Int ~ Int should fail" (runUnify (int ~> int) int)
        , testCase "occurs check" $
            assertLeft "a ~ a -> Int should fail" (runUnify (var "a") (var "a" ~> int))
        , testCase "input bindings propagate to outputs" $ do
            let r = runUnify (var "a" ~> var "a") (int ~> var "b")
            fmap (`applySubst` var "a") r @?= Right int
            fmap (`applySubst` var "b") r @?= Right int
        , testCase "a -> a ~ Int -> Bool fails" $
            assertLeft "should fail" (runUnify (var "a" ~> var "a") (int ~> bool))
        , testCase "solve threads substitutions through constraints" $ do
            let r = runSolve [Constraint (var "a") (var "b"), Constraint (var "b") int]
            fmap (`applySubst` var "a") r @?= Right int
        ]

-- constraint generation

inferTests :: TestTree
inferTests =
    testGroup
        "Constraint generation"
        [ testCase "literals produce no constraints" $
            fmap constraintCount (runInfer (EVal (VInt 1))) @?= Right 0
        , testCase "if produces condition and branch constraints" $
            fmap constraintCount (runInfer (EIf (EVal (VBool True)) (EVal (VInt 1)) (EVal (VInt 2))))
                @?= Right 2
        , testCase "undefined variable is an error" $
            assertLeft "should fail" (fmap constraintCount (runInfer (EVar "x")))
        ]
  where
    constraintCount (_, _, cs) = length cs

-- end to end: infer, then solve

endToEndTests :: TestTree
endToEndTests =
    testGroup
        "End to end"
        [ testCase "int literal" $ doAll (EVal (VInt 1)) @?= Right int
        , testCase "string literal" $ doAll (EVal (VStr "s")) @?= Right str
        , testCase "if" $
            doAll (EIf (EVal (VBool True)) (EVal (VInt 1)) (EVal (VInt 2))) @?= Right int
        , testCase "if with non-bool condition fails" $
            assertLeft "should fail" (doAll (EIf (EVal (VInt 0)) (EVal (VInt 1)) (EVal (VInt 2))))
        , testCase "if with mismatched branches fails" $
            assertLeft "should fail" (doAll (EIf (EVal (VBool True)) (EVal (VInt 1)) (EVal (VStr "s"))))
        , testCase "addition" $
            doAll (EOp OAdd (EVal (VInt 1)) (EVal (VInt 2))) @?= Right int
        , testCase "adding a string fails" $
            assertLeft "should fail" (doAll (EOp OSub (EVal (VInt 1)) (EVal (VStr "s"))))
        , testCase "identity function has type a -> a" $
            case doAll (EAbs "x" (EVar "x")) of
                Right (TFun (TVar a) (TVar b)) -> a @?= b
                other -> assertFailure $ "expected a -> a, got " ++ show other
        , testCase "lambda using addition is Int -> Int" $
            doAll (EAbs "x" (EOp OAdd (EVar "x") (EVal (VInt 1)))) @?= Right (int ~> int)
        , testCase "application" $
            doAll (EApp (EAbs "x" (EVar "x")) (EVal (VBool True))) @?= Right bool
        , testCase "exp1: let-bound function applied to Int" $ doAll exp1 @?= Right int
        , testCase "exp2: let-bound Int function applied to Str fails" $
            assertLeft "should fail" (doAll exp2)
        , testCase "let shadowing uses the inner binding" $
            doAll (ELet "x" (EVal (VInt 1)) (ELet "x" (EVal (VStr "s")) (EVar "x")))
                @?= Right str
        , testCase "let is polymorphic" $
            -- let id = \x -> x in if id True then id 1 else 2
            doAll
                ( ELet
                    "id"
                    (EAbs "x" (EVar "x"))
                    ( EIf
                        (EApp (EVar "id") (EVal (VBool True)))
                        (EApp (EVar "id") (EVal (VInt 1)))
                        (EVal (VInt 2))
                    )
                )
                @?= Right int
        , testCase "lambda-bound variables are not generalized" $
            -- \f -> let g = f in if g True then g 1 else 2  must fail
            assertLeft
                "f is monomorphic inside the lambda"
                ( doAll
                    ( EAbs
                        "f"
                        ( ELet
                            "g"
                            (EVar "f")
                            ( EIf
                                (EApp (EVar "g") (EVal (VBool True)))
                                (EApp (EVar "g") (EVal (VInt 1)))
                                (EVal (VInt 2))
                            )
                        )
                    )
                )
        , testCase "let generalizes over a polymorphic result" $
            -- let const = \a -> \b -> a in const 1 "s"
            doAll
                ( ELet
                    "const"
                    (EAbs "a" (EAbs "b" (EVar "a")))
                    (EApp (EApp (EVar "const") (EVal (VInt 1))) (EVal (VStr "s")))
                )
                @?= Right int
        , testCase "solved let constraints reach the outer context" $
            -- \x -> let y = x + 1 in x  :  Int -> Int
            doAll (EAbs "x" (ELet "y" (EOp OAdd (EVar "x") (EVal (VInt 1))) (EVar "x")))
                @?= Right (int ~> int)
        , testCase "type errors inside a let value are reported" $
            assertLeft
                "should fail"
                (doAll (ELet "y" (EOp OAdd (EVal (VInt 1)) (EVal (VBool True))) (EVal (VInt 0))))
        , testCase "self-application fails the occurs check" $
            assertLeft "should fail" (doAll (EAbs "f" (EApp (EVar "f") (EVar "f"))))
        , testCase "higher-order function" $
            -- \f -> f 1 + 2  :  (Int -> Int) -> Int
            doAll (EAbs "f" (EOp OAdd (EApp (EVar "f") (EVal (VInt 1))) (EVal (VInt 2))))
                @?= Right ((int ~> int) ~> int)
        ]
