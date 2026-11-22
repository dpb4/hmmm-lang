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
        , testCase "empty list literal" $ EList [] `evalsTo` VList []
        , testCase "singleton list literal" $ EList [lit 1] `evalsTo` VList [VInt 1]
        , testCase "nested list literal" $ EList [EList [lit 1, lit 2], EList [lit 3, lit 4]] `evalsTo` VList [VList [VInt 1, VInt 2], VList [VInt 3, VInt 4]]
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
        , operatorTests
        , listTests
        , testGroup
            "Runtime errors (for programs that skip type checking)"
            [ testCase "unbound variable" $ fails (v "x")
            , testCase "applying a non-function" $ fails (EApp (lit 1) (lit 2))
            , testCase "non-bool if condition" $ fails (EIf (lit 1) (lit 2) (lit 3))
            , testCase "arithmetic on a non-integer" $ fails (add (lit 1) true)
            , testCase "comparison on non-integers" $
                mapM_ (\o -> fails (EOp o (str "a") (str "b"))) [OGt, OGeq, OLt, OLeq]
            , testCase "and/or on non-bools" $
                mapM_ (\o -> fails (EOp o (lit 1) (lit 0))) [OAnd, OOr]
            , testCase "cons onto a non-list" $ fails (cons (lit 1) (lit 2))
            , testCase "append with a non-list" $ do
                fails (append (lit 1) (EList [lit 2]))
                fails (append (EList [lit 1]) (lit 2))
            ]
        ]

-- | an expression that fails if it is ever evaluated: 1 2
bad :: Expr
bad = EApp (lit 1) (lit 2)

-- | the result of each operator on each pair of operands
truthTable :: Operator -> [(Expr, Expr, Bool)] -> Assertion
truthTable o = mapM_ (\(a, b, r) -> EOp o a b `evalsTo` VBool r)

operatorTests :: TestTree
operatorTests =
    testGroup
        "Operators"
        [ testCase "==" $
            truthTable
                OEq
                [ (lit 1, lit 1, True)
                , (lit 1, lit 2, False)
                , (true, true, True)
                , (true, false, False)
                , (str "a", str "a", True)
                , (str "a", str "b", False)
                ]
        , testCase "/=" $
            truthTable
                ONeq
                [ (lit 1, lit 1, False)
                , (lit 1, lit 2, True)
                , (false, false, False)
                , (true, false, True)
                , (str "a", str "a", False)
                , (str "a", str "b", True)
                ]
        , testCase ">" $ truthTable OGt [(lit 2, lit 1, True), (lit 1, lit 1, False), (lit 0, lit 1, False)]
        , testCase ">=" $ truthTable OGeq [(lit 2, lit 1, True), (lit 1, lit 1, True), (lit 0, lit 1, False)]
        , testCase "<" $ truthTable OLt [(lit 2, lit 1, False), (lit 1, lit 1, False), (lit 0, lit 1, True)]
        , testCase "<=" $ truthTable OLeq [(lit 2, lit 1, False), (lit 1, lit 1, True), (lit 0, lit 1, True)]
        , testCase "and" $
            truthTable OAnd [(true, true, True), (true, false, False), (false, true, False), (false, false, False)]
        , testCase "or" $
            truthTable OOr [(true, true, True), (true, false, True), (false, true, True), (false, false, False)]
        , testGroup
            "Short circuiting"
            [ testCase "false and _ skips the right side" $ EOp OAnd false bad `evalsTo` VBool False
            , testCase "true or _ skips the right side" $ EOp OOr true bad `evalsTo` VBool True
            , testCase "true and _ evaluates the right side" $ fails (EOp OAnd true bad)
            , testCase "false or _ evaluates the right side" $ fails (EOp OOr false bad)
            , testCase "the left side is always evaluated" $ do
                fails (EOp OAnd bad false)
                fails (EOp OOr bad true)
            , testCase "a skipped right side is not checked to be a bool" $ do
                EOp OAnd false (lit 1) `evalsTo` VBool False
                EOp OOr true (lit 1) `evalsTo` VBool True
            , testCase "an evaluated right side must be a bool" $ do
                fails (EOp OAnd true (lit 1))
                fails (EOp OOr false (lit 1))
            , testCase "short circuiting through nested and/or" $
                -- false and boom or true and true
                EOp OOr (EOp OAnd false bad) (EOp OAnd true true) `evalsTo` VBool True
            , testCase "short circuiting guards a later check" $
                -- let f = \x -> x /= 0 and 10 - x > 0 in f 0 or f 3
                ELet
                    (plain "f")
                    (lam "x" (EOp OAnd (EOp ONeq (v "x") (lit 0)) (EOp OGt (sub (lit 10) (v "x")) (lit 0))))
                    (EOp OOr (EApp (v "f") (lit 0)) (EApp (v "f") (lit 3)))
                    `evalsTo` VBool True
            ]
        , testCase "operands are evaluated before comparing" $
            -- 1 + 2 == 4 - 1
            EOp OEq (add (lit 1) (lit 2)) (sub (lit 4) (lit 1)) `evalsTo` VBool True
        , testCase "comparison as an if condition" $
            -- let x = 5 in if x > 0 and x < 10 then x else 0
            ELet
                (plain "x")
                (lit 5)
                (EIf (EOp OAnd (EOp OGt (v "x") (lit 0)) (EOp OLt (v "x") (lit 10))) (v "x") (lit 0))
                `evalsTo` VInt 5
        , testCase "two lambdas are never equal" $ do
            EOp OEq (lam "x" (v "x")) (lam "x" (v "x")) `evalsTo` VBool False
            EOp ONeq (lam "x" (v "x")) (lam "x" (v "x")) `evalsTo` VBool True
        , testCase "a function is not equal to itself" $
            -- let f = \x -> x in f == f
            ELet (plain "f") (lam "x" (v "x")) (EOp OEq (v "f") (v "f")) `evalsTo` VBool False
        , testCase "comparing a function's result" $
            -- (\x -> x + 1) 1 == 2
            EOp OEq (EApp (lam "x" (add (v "x") (lit 1))) (lit 1)) (lit 2) `evalsTo` VBool True
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

cons, append :: Expr -> Expr -> Expr
cons = EOp OCons
append = EOp OAppend

-- | a list of ints, as a value
ints :: [Integer] -> Value
ints = VList . map VInt

listTests :: TestTree
listTests =
    testGroup
        "Lists"
        [ testGroup
            "Cons (++)"
            [ testCase "1 ++ []" $ cons (lit 1) (EList []) `evalsTo` ints [1]
            , testCase "1 ++ [2, 3]" $ cons (lit 1) (EList [lit 2, lit 3]) `evalsTo` ints [1, 2, 3]
            , testCase "1 ++ 2 ++ []" $ cons (lit 1) (cons (lit 2) (EList [])) `evalsTo` ints [1, 2]
            , testCase "consing a list onto a list of lists" $
                cons (EList [lit 1]) (EList [EList [lit 2]]) `evalsTo` VList [ints [1], ints [2]]
            , testCase "operands are evaluated first" $
                -- 1 + 1 ++ [x] where x = 3
                ELet (plain "x") (lit 3) (cons (add (lit 1) (lit 1)) (EList [v "x"]))
                    `evalsTo` ints [2, 3]
            , testCase "cons does not change the original list" $
                -- let xs = [2] in [1 ++ xs, xs]
                ELet (plain "xs") (EList [lit 2]) (EList [cons (lit 1) (v "xs"), v "xs"])
                    `evalsTo` VList [ints [1, 2], ints [2]]
            ]
        , testGroup
            "Append (+++)"
            [ testCase "[1] +++ [2, 3]" $
                append (EList [lit 1]) (EList [lit 2, lit 3]) `evalsTo` ints [1, 2, 3]
            , testCase "[] +++ []" $ append (EList []) (EList []) `evalsTo` VList []
            , testCase "[] is a left identity" $ append (EList []) (EList [lit 1]) `evalsTo` ints [1]
            , testCase "[] is a right identity" $ append (EList [lit 1]) (EList []) `evalsTo` ints [1]
            , testCase "order is preserved" $
                append (EList [lit 3, lit 1]) (EList [lit 2]) `evalsTo` ints [3, 1, 2]
            , testCase "append is associative" $ do
                let (a, b, c) = (EList [lit 1], EList [lit 2], EList [lit 3])
                append (append a b) c `evalsTo` ints [1, 2, 3]
                append a (append b c) `evalsTo` ints [1, 2, 3]
            , testCase "cons and append combine" $
                -- 1 ++ [2] +++ [3]
                cons (lit 1) (append (EList [lit 2]) (EList [lit 3])) `evalsTo` ints [1, 2, 3]
            , testCase "appending strings in lists" $
                append (EList [str "a"]) (EList [str "b"]) `evalsTo` VList [VStr "a", VStr "b"]
            ]
        , testGroup
            "Equality"
            [ testCase "== on lists" $
                truthTable
                    OEq
                    [ (EList [lit 1, lit 2], EList [lit 1, lit 2], True)
                    , (EList [lit 1, lit 2], EList [lit 2, lit 1], False)
                    , (EList [lit 1], EList [lit 1, lit 2], False)
                    , (EList [], EList [], True)
                    , (cons (lit 1) (EList []), EList [lit 1], True)
                    ]
            , testCase "/= on lists" $
                truthTable
                    ONeq
                    [ (EList [lit 1, lit 2], EList [lit 1, lit 2], False)
                    , (EList [lit 1, lit 2], EList [lit 2, lit 1], True)
                    , (EList [lit 1], EList [lit 1, lit 2], True)
                    , (EList [], EList [], False)
                    , (append (EList [lit 1]) (EList [lit 2]), EList [lit 1, lit 2], False)
                    ]
            ]
        , testGroup
            "Element-wise equality"
            [ testCase "nested lists are compared element by element" $ do
                EOp OEq (EList [EList [lit 1], EList [lit 2]]) (EList [EList [lit 1], EList [lit 2]])
                    `evalsTo` VBool True
                EOp OEq (EList [EList [lit 1], EList [lit 2]]) (EList [EList [lit 1], EList [lit 3]])
                    `evalsTo` VBool False
            , testCase "a prefix is not equal" $ do
                EOp OEq (EList [lit 1]) (EList [lit 1, lit 2]) `evalsTo` VBool False
                EOp OEq (EList [lit 1, lit 2]) (EList [lit 1]) `evalsTo` VBool False
            , testCase "lists of functions are never equal" $
                -- let f = \x -> x in [f] == [f]
                ELet (plain "f") (lam "x" (v "x")) (EOp OEq (EList [v "f"]) (EList [v "f"]))
                    `evalsTo` VBool False
            , testCase "lists of functions are always unequal" $
                -- let f = \x -> x in [f] /= [f]
                ELet (plain "f") (lam "x" (v "x")) (EOp ONeq (EList [v "f"]) (EList [v "f"]))
                    `evalsTo` VBool True
            , testCase "empty lists of functions are equal" $
                -- [] == [] even if they would hold functions
                EOp OEq (EList []) (EList []) `evalsTo` VBool True
            ]
        , testGroup
            "Functions over lists"
            [ testCase "a lambda that conses" $
                -- (\xs -> 0 ++ xs) [1]
                EApp (lam "xs" (cons (lit 0) (v "xs"))) (EList [lit 1]) `evalsTo` ints [0, 1]
            , testCase "elements can be function applications" $
                -- let inc = \x -> x + 1 in [inc 1, inc 2]
                ELet
                    (plain "inc")
                    (lam "x" (add (v "x") (lit 1)))
                    (EList [EApp (v "inc") (lit 1), EApp (v "inc") (lit 2)])
                    `evalsTo` ints [2, 3]
            , testCase "top-level list functions" $
                -- let prepend (x, xs) = x ++ xs; let nil = []; prepend 1 nil; prepend "a" (prepend "b" nil)
                runs
                    [ TEFun (plain "prepend") [plain "x", plain "xs"] (cons (v "x") (v "xs"))
                    , TELet (plain "nil") (EList [])
                    , TEExpr (app (v "prepend") [lit 1, v "nil"])
                    , TEExpr (app (v "prepend") [str "a", app (v "prepend") [str "b", v "nil"]])
                    ]
                    [ints [1], VList [VStr "a", VStr "b"]]
            ]
        ]
