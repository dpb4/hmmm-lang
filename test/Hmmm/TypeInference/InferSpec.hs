module Hmmm.TypeInference.InferSpec (inferTests, endToEndTests, annotationTests, programTests) where

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
exp1 = ELet (Bind "foo" Nothing) fun rest
  where
    fun = EAbs (Bind "x" Nothing) (EOp OAdd (EVar "x") (ELit $ LInt 3))
    rest = EApp (EVar "foo") (ELit $ LInt 5)

-- let foo = \x -> x + 3 in foo "c"
exp2 :: Expr
exp2 = ELet (Bind "foo" Nothing) fun rest
  where
    fun = EAbs (Bind "x" Nothing) (EOp OAdd (EVar "x") (ELit $ LInt 3))
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
            case typeOf (EAbs (Bind "x" Nothing) (EVar "x")) of
                Right (TCon (TCFun (TVar a) (TVar b))) -> a @?= b
                other -> assertFailure $ "expected a -> a, got " ++ show other
        , testCase "lambda using addition is Int -> Int" $
            typeOf (EAbs (Bind "x" Nothing) (EOp OAdd (EVar "x") (ELit (LInt 1)))) @?= Right (int ~> int)
        , testCase "application" $
            typeOf (EApp (EAbs (Bind "x" Nothing) (EVar "x")) (ELit (LBool True))) @?= Right bool
        , testCase "exp1: let-bound function applied to Int" $ typeOf exp1 @?= Right int
        , testCase "exp2: let-bound Int function applied to Str fails" $
            assertLeft "should fail" (typeOf exp2)
        , testCase "let shadowing uses the inner binding" $
            typeOf (ELet (Bind "x" Nothing) (ELit (LInt 1)) (ELet (Bind "x" Nothing) (ELit (LStr "s")) (EVar "x")))
                @?= Right str
        , testCase "let is polymorphic" $
            -- let id = \x -> x in if id True then id 1 else 2
            typeOf
                ( ELet
                    (Bind "id" Nothing)
                    (EAbs (Bind "x" Nothing) (EVar "x"))
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
                        (Bind "f" Nothing)
                        ( ELet
                            (Bind "g" Nothing)
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
                    (Bind "const" Nothing)
                    (EAbs (Bind "a" Nothing) (EAbs (Bind "b" Nothing) (EVar "a")))
                    (EApp (EApp (EVar "const") (ELit (LInt 1))) (ELit (LStr "s")))
                )
                @?= Right int
        , testCase "solved let constraints reach the outer context" $
            -- \x -> let y = x + 1 in x  :  Int -> Int
            typeOf (EAbs (Bind "x" Nothing) (ELet (Bind "y" Nothing) (EOp OAdd (EVar "x") (ELit (LInt 1))) (EVar "x")))
                @?= Right (int ~> int)
        , testCase "type errors inside a let value are reported" $
            assertLeft
                "should fail"
                (typeOf (ELet (Bind "y" Nothing) (EOp OAdd (ELit (LInt 1)) (ELit (LBool True))) (ELit (LInt 0))))
        , testCase "self-application fails the occurs check" $
            assertLeft "should fail" (typeOf (EAbs (Bind "f" Nothing) (EApp (EVar "f") (EVar "f"))))
        , testCase "higher-order function" $
            -- \f -> f 1 + 2  :  (Int -> Int) -> Int
            typeOf (EAbs (Bind "f" Nothing) (EOp OAdd (EApp (EVar "f") (ELit (LInt 1))) (ELit (LInt 2))))
                @?= Right ((int ~> int) ~> int)
        ]

-- type annotations

-- | an annotated binding
(.:) :: Name -> Type -> Binding
n .: t = Bind n (Just t)
infix 4 .:

-- | an unannotated binding
plain :: Name -> Binding
plain n = Bind n Nothing

-- | annotation types, as the parser produces them
aInt, aBool, aStr :: Type
aInt = T CInt
aBool = T CBool
aStr = T CStr

(-->) :: Type -> Type -> Type
x --> y = TC (TCFun x y)
infixr 5 -->

lit :: Integer -> Expr
lit = ELit . LInt

true :: Expr
true = ELit (LBool True)

annotationTests :: TestTree
annotationTests =
    testGroup
        "Annotations"
        [ testGroup
            "Lambda parameters"
            [ testCase "annotated parameter is used directly, without a fresh variable" $ do
                let r = runInfer (EAbs ("x" .: aInt) (EVar "x"))
                fmap (\(t, _, _) -> t) r @?= Right (int ~> int)
                fmap (\(_, n, cs) -> (n, length cs)) r @?= Right (0, 0)
            , testCase "\\(x: int) -> x  :  Int -> Int" $
                typeOf (EAbs ("x" .: aInt) (EVar "x")) @?= Right (int ~> int)
            , testCase "\\(x: bool) -> x  :  Bool -> Bool" $
                typeOf (EAbs ("x" .: aBool) (EVar "x")) @?= Right (bool ~> bool)
            , testCase "applying to an argument of the wrong type fails" $
                -- (\(x: int) -> x) true
                assertLeft "should fail" (typeOf (EApp (EAbs ("x" .: aInt) (EVar "x")) true))
            , testCase "annotation that contradicts the body fails" $
                -- \(x: str) -> x + 1
                assertLeft "should fail" (typeOf (EAbs ("x" .: aStr) (EOp OAdd (EVar "x") (lit 1))))
            , testCase "function-typed parameter" $
                -- \(f: int -> bool) -> f 1  :  (Int -> Bool) -> Bool
                typeOf (EAbs ("f" .: aInt --> aBool) (EApp (EVar "f") (lit 1)))
                    @?= Right ((int ~> bool) ~> bool)
            , testCase "function-typed parameter applied to the wrong type fails" $
                -- \(f: int -> int) -> f true
                assertLeft "should fail" (typeOf (EAbs ("f" .: aInt --> aInt) (EApp (EVar "f") true)))
            , testCase "annotation flows into a nested let" $
                -- \(x: int) -> let y = x in y  :  Int -> Int
                typeOf (EAbs ("x" .: aInt) (ELet (plain "y") (EVar "x") (EVar "y")))
                    @?= Right (int ~> int)
            ]
        , testGroup
            "Let bindings"
            [ testCase "matching annotation" $
                -- let a: int = 1 in a
                typeOf (ELet ("a" .: aInt) (lit 1) (EVar "a")) @?= Right int
            , testCase "mismatched annotation fails" $
                -- let a: int = true in a
                assertLeft "should fail" (typeOf (ELet ("a" .: aInt) true (EVar "a")))
            , testCase "mismatched annotation fails even if the binding is unused" $
                -- let a: str = 1 in 2
                assertLeft "should fail" (typeOf (ELet ("a" .: aStr) (lit 1) (lit 2)))
            , testCase "matching function annotation" $
                -- let f: int -> int = \x -> x + 1 in f 2
                typeOf (ELet ("f" .: aInt --> aInt) (EAbs (plain "x") (EOp OAdd (EVar "x") (lit 1))) (EApp (EVar "f") (lit 2)))
                    @?= Right int
            , testCase "mismatched function annotation fails" $
                -- let f: int -> bool = \x -> x + 1 in f 2
                assertLeft
                    "should fail"
                    (typeOf (ELet ("f" .: aInt --> aBool) (EAbs (plain "x") (EOp OAdd (EVar "x") (lit 1))) (EApp (EVar "f") (lit 2))))
            , testCase "annotation can specialize a polymorphic value" $
                -- let id: int -> int = \x -> x in id 1
                typeOf (ELet ("id" .: aInt --> aInt) (EAbs (plain "x") (EVar "x")) (EApp (EVar "id") (lit 1)))
                    @?= Right int
            , testCase "the body sees the annotated type" $
                -- let id: bool -> bool = \x -> x in id  :  Bool -> Bool
                typeOf (ELet ("id" .: aBool --> aBool) (EAbs (plain "x") (EVar "x")) (EVar "id"))
                    @?= Right (bool ~> bool)
            , testCase "a specialized binding is not polymorphic in the body" $
                -- let id: int -> int = \x -> x in id true
                assertLeft
                    "id was annotated int -> int, so id true should fail"
                    (typeOf (ELet ("id" .: aInt --> aInt) (EAbs (plain "x") (EVar "x")) (EApp (EVar "id") true)))
            , testCase "unannotated let is still polymorphic" $
                -- let id = \x -> x in if id true then id 1 else 2
                typeOf
                    ( ELet
                        (plain "id")
                        (EAbs (plain "x") (EVar "x"))
                        (EIf (EApp (EVar "id") true) (EApp (EVar "id") (lit 1)) (lit 2))
                    )
                    @?= Right int
            ]
        ]

-- whole programs

str' :: String -> Expr
str' = ELit . LStr

app :: Expr -> [Expr] -> Expr
app = foldl EApp

-- | let foo (a: int): int = a + 1; let g = 9; foo g
exampleProgram :: [TopExpr]
exampleProgram =
    [ fooDef
    , TELet (plain "g") (lit 9)
    , TEExpr (EApp (EVar "foo") (EVar "g"))
    ]

-- | let foo (a: int): int = a + 1
fooDef :: TopExpr
fooDef = TEFun ("foo" .: aInt) ["a" .: aInt] (EOp OAdd (EVar "a") (lit 1))

-- | let add (a, b) = a + b
addDef :: TopExpr
addDef = TEFun (plain "add") [plain "a", plain "b"] (EOp OAdd (EVar "a") (EVar "b"))

-- | let id (x) = x
idDef :: TopExpr
idDef = TEFun (plain "id") [plain "x"] (EVar "x")

programTests :: TestTree
programTests =
    testGroup
        "Programs"
        [ testCase "example program" $ typeOfProgram exampleProgram @?= Right [int]
        , testCase "only expressions have a result type, in order" $
            typeOfProgram [TEExpr (lit 1), TELet (plain "g") true, TEExpr (EVar "g"), TEExpr (str' "s")]
                @?= Right [int, bool, str]
        , testCase "a program of only definitions has no result types" $
            typeOfProgram [TELet (plain "g") (lit 1), idDef] @?= Right []
        , testCase "top-level functions are curried" $
            typeOfProgram [addDef, TEExpr (EVar "add")] @?= Right [int ~> int ~> int]
        , testCase "partial application of a top-level function" $
            -- let inc = add 1; inc
            typeOfProgram [addDef, TELet (plain "inc") (EApp (EVar "add") (lit 1)), TEExpr (EVar "inc")]
                @?= Right [int ~> int]
        , testCase "top-level functions are polymorphic" $
            typeOfProgram [idDef, TEExpr (EApp (EVar "id") true), TEExpr (EApp (EVar "id") (str' "s"))]
                @?= Right [bool, str]
        , testCase "top-level values are polymorphic" $
            -- let id = \x -> x; id true; id 1
            typeOfProgram
                [ TELet (plain "id") (EAbs (plain "x") (EVar "x"))
                , TEExpr (EApp (EVar "id") true)
                , TEExpr (EApp (EVar "id") (lit 1))
                ]
                @?= Right [bool, int]
        , testCase "a polymorphic function's type has the shape a -> a" $
            case typeOfProgram [idDef, TEExpr (EVar "id")] of
                Right [TCon (TCFun (TVar a) (TVar a'))] -> a @?= a'
                other -> assertFailure $ "expected [a -> a], got " ++ show other
        , testCase "parameters are monomorphic within the body" $
            -- let f (g) = if g true then g 1 else 2
            assertLeft
                "should fail"
                ( typeOfProgram
                    [ TEFun
                        (plain "f")
                        [plain "g"]
                        (EIf (EApp (EVar "g") true) (EApp (EVar "g") (lit 1)) (lit 2))
                    ]
                )
        , testCase "higher-order function" $
            -- let twice (f, x) = f (f x); twice (\y -> y + 1) 0
            typeOfProgram
                [ TEFun (plain "twice") [plain "f", plain "x"] (EApp (EVar "f") (EApp (EVar "f") (EVar "x")))
                , TEExpr (app (EVar "twice") [EAbs (plain "y") (EOp OAdd (EVar "y") (lit 1)), lit 0])
                ]
                @?= Right [int]
        , testCase "a definition is in scope for later items only" $
            assertLeft "using g before its definition should fail" $
                typeOfProgram [TEExpr (EVar "g"), TELet (plain "g") (lit 1)]
        , testCase "later definitions shadow earlier ones" $
            typeOfProgram [TELet (plain "g") (lit 1), TELet (plain "g") true, TEExpr (EVar "g")]
                @?= Right [bool]
        , testCase "top-level functions are not recursive (yet)" $
            -- let f (x) = f x
            assertLeft "f is not in scope in its own body" $
                typeOfProgram [TEFun (plain "f") [plain "x"] (EApp (EVar "f") (EVar "x"))]
        , testCase "top-level let ... in expression" $
            typeOfProgram [TEExpr (ELet (plain "x") (lit 1) (EVar "x"))] @?= Right [int]
        , testCase "type errors in unused definitions are reported" $
            assertLeft "should fail" $
                typeOfProgram [TELet (plain "g") (EOp OAdd (lit 1) true), TEExpr (lit 0)]
        , testGroup
            "Annotations"
            [ testCase "parameter annotation is enforced" $
                -- let f (x: int) = x; f true
                assertLeft "should fail" $
                    typeOfProgram [TEFun (plain "f") ["x" .: aInt] (EVar "x"), TEExpr (EApp (EVar "f") true)]
            , testCase "matching return type" $
                typeOfProgram [fooDef, TEExpr (EVar "foo")] @?= Right [int ~> int]
            , testCase "return type annotation is enforced" $
                -- let f (a: int): bool = a + 1
                assertLeft "should fail" $
                    typeOfProgram [TEFun ("f" .: aBool) ["a" .: aInt] (EOp OAdd (EVar "a") (lit 1))]
            , testCase "return type annotation specializes a polymorphic function" $
                -- let f (x): int = x; f
                typeOfProgram [TEFun ("f" .: aInt) [plain "x"] (EVar "x"), TEExpr (EVar "f")]
                    @?= Right [int ~> int]
            , testCase "function return type" $
                -- let k (x: int): int -> int = \y -> x; k 5 7
                typeOfProgram
                    [ TEFun ("k" .: aInt --> aInt) ["x" .: aInt] (EAbs (plain "y") (EVar "x"))
                    , TEExpr (app (EVar "k") [lit 5, lit 7])
                    ]
                    @?= Right [int]
            , testCase "function return type is enforced on the result" $
                -- let k (x: int): int -> int = \y -> x; k 5 "s"
                assertLeft "should fail" $
                    typeOfProgram
                        [ TEFun ("k" .: aInt --> aInt) ["x" .: aInt] (EAbs (plain "y") (EVar "x"))
                        , TEExpr (app (EVar "k") [lit 5, str' "s"])
                        ]
            , testCase "value annotation is enforced" $
                assertLeft "should fail" $
                    typeOfProgram [TELet ("g" .: aStr) (lit 1)]
            ]
        ]
