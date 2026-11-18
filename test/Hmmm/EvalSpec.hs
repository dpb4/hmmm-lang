module Hmmm.EvalSpec (evalTests) where

import qualified Data.Map as M
import Helpers (assertLeft)
import Hmmm.Eval
import Hmmm.Syntax
import Hmmm.Types
import Test.Tasty
import Test.Tasty.HUnit

evalsTo :: Expr -> Value -> Assertion
evalsTo e expected = runEval e @?= Right expected

runs :: [TopExpr] -> [Value] -> Assertion
runs prog expected = runEvalProgram prog @?= Right expected

fails :: Expr -> Assertion
fails e = assertLeft "should be a runtime error" (runEval e)

plain :: Name -> Binding
plain n = Bind n Nothing

lit :: Integer -> Expr
lit = ELit . LInt

true, false :: Expr
true = ELit (LBool True)
false = ELit (LBool False)

str :: String -> Expr
str = ELit . LStr

v :: Name -> Expr
v = EVar

app :: Expr -> [Expr] -> Expr
app = foldl EApp

lam :: Name -> Expr -> Expr
lam = EAbs . plain

add, sub :: Expr -> Expr -> Expr
add = EOp OAdd
sub = EOp OSub

evalTests :: TestTree
evalTests =
    testGroup
        "EVALUATION"
        [ expressionTests
        , programTests
        ]

expressionTests :: TestTree
expressionTests =
    testGroup
        "Expressions"
        [ testCase "int literal" $ lit 1 `evalsTo` VInt 1
        , testCase "bool literal" $ true `evalsTo` VBool True
        , testCase "string literal" $ str "s" `evalsTo` VStr "s"
        , testCase "addition" $ add (lit 1) (lit 2) `evalsTo` VInt 3
        , testCase "subtraction is left associative" $
            add (sub (lit 1) (lit 2)) (lit 3) `evalsTo` VInt 2
        , testCase "if true" $ EIf true (lit 1) (lit 2) `evalsTo` VInt 1
        , testCase "if false" $ EIf false (lit 1) (lit 2) `evalsTo` VInt 2
        , testCase "only the chosen branch is evaluated" $
            -- if true then 1 else (1 2), which would fail if evaluated
            EIf true (lit 1) (EApp (lit 1) (lit 2)) `evalsTo` VInt 1
        , testCase "let" $ ELet (plain "x") (lit 1) (add (v "x") (lit 1)) `evalsTo` VInt 2
        , testCase "let shadowing" $
            ELet (plain "x") (lit 1) (ELet (plain "x") (lit 2) (v "x")) `evalsTo` VInt 2
        , testCase "annotations are ignored at runtime" $
            ELet (Bind "x" (Just (T CInt))) (lit 1) (v "x") `evalsTo` VInt 1
        , testCase "application" $ EApp (lam "x" (add (v "x") (lit 1))) (lit 1) `evalsTo` VInt 2
        , testCase "curried application" $
            app (lam "x" (lam "y" (sub (v "x") (v "y")))) [lit 5, lit 3] `evalsTo` VInt 2
        , testCase "a lambda evaluates to a closure" $
            -- the body is not run until the closure is applied
            lam "x" (EApp (lit 1) (lit 2)) `evalsTo` VClosure M.empty "x" (EApp (lit 1) (lit 2))
        , testCase "a closure captures the environment it was created in" $
            -- let y = 1 in \x -> y
            ELet (plain "y") (lit 1) (lam "x" (v "y"))
                `evalsTo` VClosure (M.singleton "y" (VInt 1)) "x" (v "y")
        , testCase "closures capture the environment where they are defined" $
            -- let x = 1 in let f = \y -> x in let x = 2 in f 0
            ELet
                (plain "x")
                (lit 1)
                (ELet (plain "f") (lam "y" (v "x")) (ELet (plain "x") (lit 2) (EApp (v "f") (lit 0))))
                `evalsTo` VInt 1
        , testCase "arguments do not leak into the caller's scope" $
            -- let x = 1 in (\x -> x) 2 + x
            ELet (plain "x") (lit 1) (add (EApp (lam "x" (v "x")) (lit 2)) (v "x")) `evalsTo` VInt 3
        , testGroup
            "Runtime errors (for programs that skip type checking)"
            [ testCase "unbound variable" $ fails (v "x")
            , testCase "applying a non-function" $ fails (EApp (lit 1) (lit 2))
            , testCase "non-bool if condition" $ fails (EIf (lit 1) (lit 2) (lit 3))
            , testCase "arithmetic on a non-integer" $ fails (add (lit 1) true)
            ]
        ]

programTests :: TestTree
programTests =
    testGroup
        "Programs"
        [ testCase "example program" $
            -- let foo (a: int): int = a + 1; let g = 9; foo g
            runs
                [ TEFun (Bind "foo" (Just (T CInt))) [Bind "a" (Just (T CInt))] (add (v "a") (lit 1))
                , TELet (plain "g") (lit 9)
                , TEExpr (EApp (v "foo") (v "g"))
                ]
                [VInt 10]
        , testCase "each expression produces a value, in order" $
            runs [TEExpr (lit 1), TEExpr true, TEExpr (str "s")] [VInt 1, VBool True, VStr "s"]
        , testCase "definitions produce no output" $
            runs [TELet (plain "g") (lit 1), TEFun (plain "f") [plain "x"] (v "x")] []
        , testCase "definitions are in scope for later items" $
            runs [TELet (plain "g") (lit 1), TELet (plain "h") (add (v "g") (lit 1)), TEExpr (v "h")] [VInt 2]
        , testCase "later definitions shadow earlier ones" $
            runs [TELet (plain "g") (lit 1), TELet (plain "g") (lit 2), TEExpr (v "g")] [VInt 2]
        , testCase "top-level functions capture earlier definitions, not later ones" $
            -- let x = 1; let f (y) = x; let x = 2; f 0
            runs
                [ TELet (plain "x") (lit 1)
                , TEFun (plain "f") [plain "y"] (v "x")
                , TELet (plain "x") (lit 2)
                , TEExpr (EApp (v "f") (lit 0))
                ]
                [VInt 1]
        , testCase "multi-parameter functions are curried" $
            -- let minus (a, b) = a - b; minus 5 3; let dec = \x -> minus x 1; dec 10
            runs
                [ TEFun (plain "minus") [plain "a", plain "b"] (sub (v "a") (v "b"))
                , TEExpr (app (v "minus") [lit 5, lit 3])
                , TELet (plain "dec") (lam "x" (app (v "minus") [v "x", lit 1]))
                , TEExpr (EApp (v "dec") (lit 10))
                ]
                [VInt 2, VInt 9]
        , testCase "partial application" $
            -- let add (a, b) = a + b; let inc = add 1; inc 41
            runs
                [ TEFun (plain "add") [plain "a", plain "b"] (add (v "a") (v "b"))
                , TELet (plain "inc") (EApp (v "add") (lit 1))
                , TEExpr (EApp (v "inc") (lit 41))
                ]
                [VInt 42]
        , testCase "higher-order function" $
            -- let twice (f, x) = f (f x); twice (\y -> y + 1) 0
            runs
                [ TEFun (plain "twice") [plain "f", plain "x"] (EApp (v "f") (EApp (v "f") (v "x")))
                , TEExpr (app (v "twice") [lam "y" (add (v "y") (lit 1)), lit 0])
                ]
                [VInt 2]
        , testCase "a function with no parameters is a value" $
            -- let f () = 1; f
            runs [TEFun (plain "f") [] (lit 1), TEExpr (v "f")] [VInt 1]
        , testCase "top-level let ... in expression" $
            runs [TEExpr (ELet (plain "x") (lit 1) (v "x"))] [VInt 1]
        , testCase "using a name before its definition is an error" $
            assertLeft "should fail" $
                runEvalProgram [TEExpr (v "g"), TELet (plain "g") (lit 1)]
        , testCase "an empty program produces nothing" $ runs [] []
        ]
