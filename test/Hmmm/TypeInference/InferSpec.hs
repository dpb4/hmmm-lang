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
        , testCase "list produces constraint per element" $
            fmap constraintCount (runInfer (EList [(ELit (LInt 2)), (ELit (LInt 1)), (ELit (LInt 3))]))
                @?= Right 3
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
        , testCase "empty list has type List a" $
            case typeOf (EList []) of
                Right (TCon (TCList (TVar a))) -> a @?= a -- TODO hacky
                other -> assertFailure $ "expected a -> a, got " ++ show other
        , testCase "list with mismatched elements fails" $
            assertLeft "should fail" (typeOf (EList [ELit $ LInt 2, ELit $ LBool False]))
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
        , operatorTests
        , listTests
        ]

operatorTests :: TestTree
operatorTests =
    testGroup
        "Operators"
        [ testCase "comparisons on ints are Bool" $
            mapM_ (\o -> typeOf (EOp o (lit 1) (lit 2)) @?= Right bool) [OGt, OGeq, OLt, OLeq]
        , testCase "comparisons on non-ints fail" $
            mapM_
                (\(o, a, b) -> assertLeft (show (EOp o a b) ++ " should fail") (typeOf (EOp o a b)))
                [(o, a, b) | o <- [OGt, OGeq, OLt, OLeq], (a, b) <- [(str' "a", str' "b"), (true, true), (lit 1, true)]]
        , testCase "and/or on bools are Bool" $
            mapM_ (\o -> typeOf (EOp o true true) @?= Right bool) [OAnd, OOr]
        , testCase "and/or on non-bools fail" $
            mapM_
                (\(o, a, b) -> assertLeft (show (EOp o a b) ++ " should fail") (typeOf (EOp o a b)))
                [(o, a, b) | o <- [OAnd, OOr], (a, b) <- [(lit 1, lit 2), (true, lit 1), (str' "s", true)]]
        , testCase "equality works on ints, bools and strings" $
            mapM_
                (\(o, a, b) -> typeOf (EOp o a b) @?= Right bool)
                [(o, a, b) | o <- [OEq, ONeq], (a, b) <- [(lit 1, lit 2), (true, true), (str' "a", str' "b")]]
        , testCase "equality on mismatched types fails" $
            mapM_
                (\(o, a, b) -> assertLeft (show (EOp o a b) ++ " should fail") (typeOf (EOp o a b)))
                [(o, a, b) | o <- [OEq, ONeq], (a, b) <- [(lit 1, true), (str' "s", lit 1)]]
        , testCase "equality constrains both sides to the same type" $
            -- \x -> \y -> x == y  :  a -> a -> Bool
            case typeOf (EAbs (plain "x") (EAbs (plain "y") (EOp OEq (EVar "x") (EVar "y")))) of
                Right (TCon (TCFun (TVar a) (TCon (TCFun (TVar a') (TLit CBool))))) -> a @?= a'
                other -> assertFailure $ "expected a -> a -> Bool, got " ++ show other
        , testCase "equality with a literal determines the other side" $
            -- \x -> x == "s"  :  Str -> Bool
            typeOf (EAbs (plain "x") (EOp OEq (EVar "x") (str' "s"))) @?= Right (str ~> bool)
        , testCase "comparison infers Int for its operands" $
            -- \x -> \y -> x < y  :  Int -> Int -> Bool
            typeOf (EAbs (plain "x") (EAbs (plain "y") (EOp OLt (EVar "x") (EVar "y"))))
                @?= Right (int ~> int ~> bool)
        , testCase "and infers Bool for its operands" $
            -- \x -> x and true  :  Bool -> Bool
            typeOf (EAbs (plain "x") (EOp OAnd (EVar "x") true)) @?= Right (bool ~> bool)
        , testCase "comparison result can be an if condition" $
            -- \x -> if x > 0 and x < 10 then x else 0  :  Int -> Int
            typeOf
                ( EAbs
                    (plain "x")
                    (EIf (EOp OAnd (EOp OGt (EVar "x") (lit 0)) (EOp OLt (EVar "x") (lit 10))) (EVar "x") (lit 0))
                )
                @?= Right (int ~> int)
        , testCase "comparison result is not an Int" $
            -- (1 < 2) + 1
            assertLeft "should fail" (typeOf (EOp OAdd (EOp OLt (lit 1) (lit 2)) (lit 1)))
        , testCase "chained comparison fails" $
            -- (1 < 2) < 3
            assertLeft "should fail" (typeOf (EOp OLt (EOp OLt (lit 1) (lit 2)) (lit 3)))
        , testCase "chained equality compares the Bool result" $
            -- (1 == 2) == false  :  Bool
            typeOf (EOp OEq (EOp OEq (lit 1) (lit 2)) (ELit (LBool False))) @?= Right bool
        , testCase "each use of == gets its own type variable" $
            -- 1 == 2 and "a" == "b"
            typeOf (EOp OAnd (EOp OEq (lit 1) (lit 2)) (EOp OEq (str' "a") (str' "b"))) @?= Right bool
        , testCase "a let-bound equality function is polymorphic" $
            -- let eq = \x -> \y -> x == y in eq 1 2 and eq "a" "b"
            typeOf
                ( ELet
                    (plain "eq")
                    (EAbs (plain "x") (EAbs (plain "y") (EOp OEq (EVar "x") (EVar "y"))))
                    (EOp OAnd (app (EVar "eq") [lit 1, lit 2]) (app (EVar "eq") [str' "a", str' "b"]))
                )
                @?= Right bool
        , testCase "a let-bound equality function still rejects mismatched arguments" $
            -- let eq = \x -> \y -> x == y in eq 1 "a"
            assertLeft "should fail" $
                typeOf
                    ( ELet
                        (plain "eq")
                        (EAbs (plain "x") (EAbs (plain "y") (EOp OEq (EVar "x") (EVar "y"))))
                        (app (EVar "eq") [lit 1, str' "a"])
                    )
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
            , testCase "annotated empty list is not polymorphic" $
                -- let id: List int = [] in id
                typeOf (ELet ("id" .: (TC $ TCList aInt)) (EList []) (EVar "id"))
                    @?= Right (TCon $ TCList int)
            , testCase "annotated singleton list fails for wrong element" $
                -- let id: List int = [] in id
                assertLeft "id was annoted List int, so List bool should fail" (typeOf (ELet ("id" .: (TC $ TCList aInt)) (EList [true]) (EVar "id")))
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

-- lists

cons, append :: Expr -> Expr -> Expr
cons = EOp OCons
append = EOp OAppend

lam :: Name -> Expr -> Expr
lam = EAbs . plain

-- | a type of the form List a, for some type variable a
isPolyList :: Either String TypeMaybe -> Assertion
isPolyList t = case t of
    Right (TCon (TCList (TVar _))) -> return ()
    other -> assertFailure $ "expected List a, got " ++ show other

fails :: Expr -> Assertion
fails e = assertLeft (show e ++ " should fail") (typeOf e)

listTests :: TestTree
listTests =
    testGroup
        "Lists"
        [ testGroup
            "Literals"
            [ testCase "[1, 2]  :  List Int" $ typeOf (EList [lit 1, lit 2]) @?= Right (list int)
            , testCase "[\"a\"]  :  List Str" $ typeOf (EList [str' "a"]) @?= Right (list str)
            , testCase "nested lists" $
                typeOf (EList [EList [lit 1], EList [lit 2, lit 3]]) @?= Right (list (list int))
            , testCase "an empty inner list takes its siblings' type" $
                typeOf (EList [EList [lit 1], EList []]) @?= Right (list (list int))
            , testCase "a list of empty lists is polymorphic in the inner element" $
                case typeOf (EList [EList [], EList []]) of
                    Right (TCon (TCList (TCon (TCList (TVar _))))) -> return ()
                    other -> assertFailure $ "expected List (List a), got " ++ show other
            , testCase "mismatched nested lists fail" $ fails (EList [EList [lit 1], EList [true]])
            , testCase "a list and a non-list element fail" $ fails (EList [EList [lit 1], lit 2])
            , testCase "an element determines a variable's type" $
                -- \x -> [x, 1]  :  Int -> List Int
                typeOf (lam "x" (EList [EVar "x", lit 1])) @?= Right (int ~> list int)
            , testCase "singleton of a variable" $
                -- \x -> [x]  :  a -> List a
                case typeOf (lam "x" (EList [EVar "x"])) of
                    Right (TCon (TCFun (TVar a) (TCon (TCList (TVar a'))))) -> a @?= a'
                    other -> assertFailure $ "expected a -> List a, got " ++ show other
            , testCase "list of functions" $
                -- [\x -> x + 1, \y -> y]  :  List (Int -> Int)
                typeOf (EList [lam "x" (EOp OAdd (EVar "x") (lit 1)), lam "y" (EVar "y")])
                    @?= Right (list (int ~> int))
            , testCase "list elements are checked against a function's parameter" $
                -- (\x -> x + 1) applied in [f 1, true] fails
                fails (EList [EApp (lam "x" (EOp OAdd (EVar "x") (lit 1))) (lit 1), true])
            , testCase "lists can be compared with ==" $
                typeOf (EOp OEq (EList [lit 1]) (EList [])) @?= Right bool
            , testCase "lists of different types cannot be compared" $
                fails (EOp OEq (EList [lit 1]) (EList [true]))
            , testCase "a list is not an Int" $ fails (EOp OAdd (EList [lit 1]) (lit 1))
            ]
        , testGroup
            "Cons (++)"
            [ testCase "1 ++ []  :  List Int" $ typeOf (cons (lit 1) (EList [])) @?= Right (list int)
            , testCase "1 ++ [2]  :  List Int" $ typeOf (cons (lit 1) (EList [lit 2])) @?= Right (list int)
            , testCase "1 ++ 2 ++ []  :  List Int" $
                typeOf (cons (lit 1) (cons (lit 2) (EList []))) @?= Right (list int)
            , testCase "consing a list onto a list of lists" $
                typeOf (cons (EList [lit 1]) (EList [])) @?= Right (list (list int))
            , testCase "element type must match the list" $ fails (cons true (EList [lit 1]))
            , testCase "the right side must be a list" $ fails (cons (lit 1) (lit 2))
            , testCase "list ++ list is not append" $ fails (cons (EList [lit 1]) (EList [lit 2]))
            , testCase "cons infers both argument types" $
                -- \x -> \xs -> x ++ xs  :  a -> List a -> List a
                case typeOf (lam "x" (lam "xs" (cons (EVar "x") (EVar "xs")))) of
                    Right (TCon (TCFun (TVar a) (TCon (TCFun (TCon (TCList (TVar a'))) (TCon (TCList (TVar a''))))))) -> do
                        a @?= a'
                        a' @?= a''
                    other -> assertFailure $ "expected a -> List a -> List a, got " ++ show other
            , testCase "consing an Int fixes the list type" $
                -- \xs -> 1 ++ xs  :  List Int -> List Int
                typeOf (lam "xs" (cons (lit 1) (EVar "xs"))) @?= Right (list int ~> list int)
            , testCase "each use of ++ gets its own type variable" $
                -- [1 ++ []] == [1 ++ []] and true ++ [] == [true]
                typeOf
                    ( EOp
                        OAnd
                        (EOp OEq (EList [cons (lit 1) (EList [])]) (EList []))
                        (EOp OEq (cons true (EList [])) (EList [true]))
                    )
                    @?= Right bool
            ]
        , testGroup
            "Append (+++)"
            [ testCase "[1] +++ [2]  :  List Int" $
                typeOf (append (EList [lit 1]) (EList [lit 2])) @?= Right (list int)
            , testCase "[] +++ []  :  List a" $ isPolyList (typeOf (append (EList []) (EList [])))
            , testCase "[] +++ [1]  :  List Int" $ typeOf (append (EList []) (EList [lit 1])) @?= Right (list int)
            , testCase "element types must match" $ fails (append (EList [lit 1]) (EList [true]))
            , testCase "the left side must be a list" $ fails (append (lit 1) (EList [lit 2]))
            , testCase "the right side must be a list" $ fails (append (EList [lit 1]) (lit 2))
            , testCase "append infers a list parameter" $
                -- \xs -> xs +++ [1]  :  List Int -> List Int
                typeOf (lam "xs" (append (EVar "xs") (EList [lit 1]))) @?= Right (list int ~> list int)
            , testCase "cons and append combine" $
                -- 1 ++ [2] +++ [3]
                typeOf (cons (lit 1) (append (EList [lit 2]) (EList [lit 3]))) @?= Right (list int)
            ]
        , testGroup
            "Polymorphism"
            [ testCase "a let-bound empty list is polymorphic" $
                -- let nil = [] in if true ++ nil == [true] then 1 ++ nil else [2]
                typeOf
                    ( ELet
                        (plain "nil")
                        (EList [])
                        ( EIf
                            (EOp OEq (cons true (EVar "nil")) (EList [true]))
                            (cons (lit 1) (EVar "nil"))
                            (EList [lit 2])
                        )
                    )
                    @?= Right (list int)
            , testCase "a lambda-bound list is monomorphic" $
                -- \xs -> if 1 ++ xs == [] then true ++ xs else []
                fails
                    ( lam
                        "xs"
                        ( EIf
                            (EOp OEq (cons (lit 1) (EVar "xs")) (EList []))
                            (cons true (EVar "xs"))
                            (EList [])
                        )
                    )
            , testCase "a let-bound list constructor is polymorphic" $
                -- let single = \x -> [x] in if single true == [true] then single 1 else []
                typeOf
                    ( ELet
                        (plain "single")
                        (lam "x" (EList [EVar "x"]))
                        ( EIf
                            (EOp OEq (EApp (EVar "single") true) (EList [true]))
                            (EApp (EVar "single") (lit 1))
                            (EList [])
                        )
                    )
                    @?= Right (list int)
            ]
        , testGroup
            "Annotations"
            [ testCase "annotated list parameter" $
                -- \(xs: [int]) -> xs  :  List Int -> List Int
                typeOf (EAbs ("xs" .: TC (TCList aInt)) (EVar "xs")) @?= Right (list int ~> list int)
            , testCase "annotated list parameter is enforced" $
                -- (\(xs: [int]) -> xs) [true]
                fails (EApp (EAbs ("xs" .: TC (TCList aInt)) (EVar "xs")) (EList [true]))
            , testCase "annotation specializes cons" $
                -- let c: int -> [int] -> [int] = \x -> \xs -> x ++ xs in c
                typeOf
                    ( ELet
                        ("c" .: aInt --> TC (TCList aInt) --> TC (TCList aInt))
                        (lam "x" (lam "xs" (cons (EVar "x") (EVar "xs"))))
                        (EVar "c")
                    )
                    @?= Right (int ~> list int ~> list int)
            , testCase "a list annotation is not an element annotation" $
                -- let x: [int] = 1 in x
                fails (ELet ("x" .: TC (TCList aInt)) (lit 1) (EVar "x"))
            , testCase "nested list annotation" $
                -- let x: [[int]] = [[]] in x
                typeOf (ELet ("x" .: TC (TCList (TC (TCList aInt)))) (EList [EList []]) (EVar "x"))
                    @?= Right (list (list int))
            ]
        , testGroup
            "Programs"
            [ testCase "a polymorphic top-level list function" $
                -- let prepend (x, xs) = x ++ xs; prepend 1 [2]; prepend "a" []
                typeOfProgram
                    [ TEFun (plain "prepend") [plain "x", plain "xs"] (cons (EVar "x") (EVar "xs"))
                    , TEExpr (app (EVar "prepend") [lit 1, EList [lit 2]])
                    , TEExpr (app (EVar "prepend") [str' "a", EList []])
                    ]
                    @?= Right [list int, list str]
            , testCase "a list-typed top-level function rejects other lists" $
                -- let f (xs: [int]): [int] = xs; f [true]
                assertLeft "should fail" $
                    typeOfProgram
                        [ TEFun ("f" .: TC (TCList aInt)) ["xs" .: TC (TCList aInt)] (EVar "xs")
                        , TEExpr (EApp (EVar "f") (EList [true]))
                        ]
            , testCase "a top-level empty list is polymorphic" $
                -- let nil = []; 1 ++ nil; "a" ++ nil
                typeOfProgram
                    [ TELet (plain "nil") (EList [])
                    , TEExpr (cons (lit 1) (EVar "nil"))
                    , TEExpr (cons (str' "a") (EVar "nil"))
                    ]
                    @?= Right [list int, list str]
            ]
        ]
