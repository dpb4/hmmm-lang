module Hmmm.TypeInference.InferSpec (inferTests, endToEndTests) where

import qualified Data.Map as M
import qualified Data.Set as S
import Helpers
import Hmmm.Infer
import Hmmm.Syntax
import Hmmm.Types
import Test.Tasty
import Test.Tasty.HUnit

-- let foo = \x -> x + 3 in foo 5
exp1 :: Expr
exp1 = ELet "foo" fun rest
  where
    fun = EAbs "x" (EOp OAdd (EVar "x") (ELit $ LInt 3))
    rest = EApp (EVar "foo") (ELit $ LInt 5)

-- let foo = \x -> x + 3 in foo "c"
exp2 :: Expr
exp2 = ELet "foo" fun rest
  where
    fun = EAbs "x" (EOp OAdd (EVar "x") (ELit $ LInt 3))
    rest = EApp (EVar "foo") (ELit $ LStr "c")

-- constraint generation

inferTests :: TestTree
inferTests =
    testGroup
        "Constraint generation"
        [ testCase "generalize quantifies only vars not free in the context" $ do
            let ctx = M.fromList [("y", ForAll S.empty (var "b"))]
                ForAll qvs _ = generalize ctx (var "a" ~> var "b")
            qvs @?= S.singleton (tv "a")
        , testCase "literals produce no constraints" $
            fmap constraintCount (runInfer (ELit (LInt 1))) @?= Right 0
        , testCase "if produces condition and branch constraints" $
            fmap constraintCount (runInfer (EIf (ELit (LBool True)) (ELit (LInt 1)) (ELit (LInt 2))))
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
        [ testCase "int literal" $ typeOf (ELit (LInt 1)) @?= Right int
        , testCase "string literal" $ typeOf (ELit (LStr "s")) @?= Right str
        , testCase "if" $
            typeOf (EIf (ELit (LBool True)) (ELit (LInt 1)) (ELit (LInt 2))) @?= Right int
        , testCase "if with non-bool condition fails" $
            assertLeft "should fail" (typeOf (EIf (ELit (LInt 0)) (ELit (LInt 1)) (ELit (LInt 2))))
        , testCase "if with mismatched branches fails" $
            assertLeft "should fail" (typeOf (EIf (ELit (LBool True)) (ELit (LInt 1)) (ELit (LStr "s"))))
        , testCase "addition" $
            typeOf (EOp OAdd (ELit (LInt 1)) (ELit (LInt 2))) @?= Right int
        , testCase "adding a string fails" $
            assertLeft "should fail" (typeOf (EOp OSub (ELit (LInt 1)) (ELit (LStr "s"))))
        , testCase "identity function has type a -> a" $
            case typeOf (EAbs "x" (EVar "x")) of
                Right (TCon (TCFun (TVar a) (TVar b))) -> a @?= b
                other -> assertFailure $ "expected a -> a, got " ++ show other
        , testCase "lambda using addition is Int -> Int" $
            typeOf (EAbs "x" (EOp OAdd (EVar "x") (ELit (LInt 1)))) @?= Right (int ~> int)
        , testCase "application" $
            typeOf (EApp (EAbs "x" (EVar "x")) (ELit (LBool True))) @?= Right bool
        , testCase "exp1: let-bound function applied to Int" $ typeOf exp1 @?= Right int
        , testCase "exp2: let-bound Int function applied to Str fails" $
            assertLeft "should fail" (typeOf exp2)
        , testCase "let shadowing uses the inner binding" $
            typeOf (ELet "x" (ELit (LInt 1)) (ELet "x" (ELit (LStr "s")) (EVar "x")))
                @?= Right str
        , testCase "let is polymorphic" $
            -- let id = \x -> x in if id True then id 1 else 2
            typeOf
                ( ELet
                    "id"
                    (EAbs "x" (EVar "x"))
                    ( EIf
                        (EApp (EVar "id") (ELit (LBool True)))
                        (EApp (EVar "id") (ELit (LInt 1)))
                        (ELit (LInt 2))
                    )
                )
                @?= Right int
        , testCase "lambda-bound variables are not generalized" $
            -- \f -> let g = f in if g True then g 1 else 2  must fail
            assertLeft
                "f is monomorphic inside the lambda"
                ( typeOf
                    ( EAbs
                        "f"
                        ( ELet
                            "g"
                            (EVar "f")
                            ( EIf
                                (EApp (EVar "g") (ELit (LBool True)))
                                (EApp (EVar "g") (ELit (LInt 1)))
                                (ELit (LInt 2))
                            )
                        )
                    )
                )
        , testCase "let generalizes over a polymorphic result" $
            -- let const = \a -> \b -> a in const 1 "s"
            typeOf
                ( ELet
                    "const"
                    (EAbs "a" (EAbs "b" (EVar "a")))
                    (EApp (EApp (EVar "const") (ELit (LInt 1))) (ELit (LStr "s")))
                )
                @?= Right int
        , testCase "solved let constraints reach the outer context" $
            -- \x -> let y = x + 1 in x  :  Int -> Int
            typeOf (EAbs "x" (ELet "y" (EOp OAdd (EVar "x") (ELit (LInt 1))) (EVar "x")))
                @?= Right (int ~> int)
        , testCase "type errors inside a let value are reported" $
            assertLeft
                "should fail"
                (typeOf (ELet "y" (EOp OAdd (ELit (LInt 1)) (ELit (LBool True))) (ELit (LInt 0))))
        , testCase "self-application fails the occurs check" $
            assertLeft "should fail" (typeOf (EAbs "f" (EApp (EVar "f") (EVar "f"))))
        , testCase "higher-order function" $
            -- \f -> f 1 + 2  :  (Int -> Int) -> Int
            typeOf (EAbs "f" (EOp OAdd (EApp (EVar "f") (ELit (LInt 1))) (ELit (LInt 2))))
                @?= Right ((int ~> int) ~> int)
        ]
