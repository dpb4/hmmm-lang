module Hmmm.BuiltinsSpec (builtinTests) where

import qualified Data.Map as M
import Helpers
import Hmmm.Builtins
import Hmmm.Eval
import Hmmm.Infer
import Hmmm.Syntax
import Hmmm.Types
import Test.Tasty
import Test.Tasty.HUnit

-- | type check and evaluate an expression with the builtins in scope
runs :: Expr -> TypeMaybe -> Value -> Assertion
runs e t v = do
    typeOf builtinTypes e @?= Right t
    runEval builtinEnv e @?= Right v

-- | type checks with the builtins in scope, but fails at runtime
failsAtRuntime :: Expr -> Assertion
failsAtRuntime e = do
    assertBool ("should type check: " ++ show e) (isRight' (typeOf builtinTypes e))
    assertLeft "should fail at runtime" (runEval builtinEnv e)
  where
    isRight' = either (const False) (const True)

typeError :: Expr -> Assertion
typeError e = assertLeft (show e ++ " should not type check") (typeOf builtinTypes e)

lit :: Integer -> Expr
lit = ELit . LInt

ints :: [Integer] -> Expr
ints = EList . map lit

vints :: [Integer] -> Value
vints = VList . map VInt

call :: Name -> [Expr] -> Expr
call f = foldl EApp (EVar f)

lam :: Name -> Expr -> Expr
lam = EAbs . (`Bind` Nothing)

builtinTests :: TestTree
builtinTests =
    testGroup
        "BUILTINS"
        [ testGroup
            "Table"
            [ testCase "names are unique" $
                let names = [n | (n, _, _) <- builtins]
                 in M.size (M.fromList (zip names names)) @?= length names
            , testCase "every builtin has a type and an implementation" $
                M.keys builtinTypes @?= M.keys builtinEnv
            , testCase "builtin types are fully generalized" $
                mapM_
                    (\(n, t) -> assertBool (n ++ " has free type variables") (null (freeTypeVars t)))
                    (M.toList builtinTypes)
            , testCase "builtins are values that show by name" $
                fmap show (runEval builtinEnv (EVar "head")) @?= Right "<builtin head>"
            ]
        , testGroup
            "head"
            [ testCase "head [1, 2]" $ runs (call "head" [ints [1, 2]]) int (VInt 1)
            , testCase "head of a string is a Char" $
                runs (call "head" [strLit "ab"]) char (VChar 'a')
            , testCase "head of an empty list fails at runtime" $ failsAtRuntime (call "head" [ints []])
            , testCase "head of a non-list fails to type check" $ typeError (call "head" [lit 1])
            ]
        , testGroup
            "tail"
            [ testCase "tail [1, 2, 3]" $ runs (call "tail" [ints [1, 2, 3]]) (list int) (vints [2, 3])
            , testCase "tail of a singleton is empty" $ runs (call "tail" [ints [1]]) (list int) (vints [])
            , testCase "tail of a string is a string" $
                runs (call "tail" [strLit "ab"]) str (VList [VChar 'b'])
            , testCase "tail of an empty list fails at runtime" $ failsAtRuntime (call "tail" [ints []])
            , testCase "tail of a non-list fails to type check" $ typeError (call "tail" [lit 1])
            ]
        , testGroup
            "empty?"
            [ testCase "empty? []" $
                -- the element type is unconstrained, so check the evaluation only
                runEval builtinEnv (call "empty?" [ints []]) @?= Right (VBool True)
            , testCase "empty? [1]" $ runs (call "empty?" [ints [1]]) bool (VBool False)
            , testCase "empty? \"\"" $ runEval builtinEnv (call "empty?" [strLit ""]) @?= Right (VBool True)
            , testCase "empty? of a non-list fails to type check" $ typeError (call "empty?" [lit 1])
            ]
        , testGroup
            "As values"
            [ testCase "builtins are polymorphic" $
                -- if empty? "a" then 0 else head [1]
                runs (EIf (call "empty?" [strLit "a"]) (lit 0) (call "head" [ints [1]])) int (VInt 1)
            , testCase "builtins compose" $
                -- head (tail [1, 2, 3])
                runs (call "head" [call "tail" [ints [1, 2, 3]]]) int (VInt 2)
            , testCase "a builtin can be passed as an argument" $
                -- (\f -> f [1, 2]) head
                runs (EApp (lam "f" (EApp (EVar "f") (ints [1, 2]))) (EVar "head")) int (VInt 1)
            , testCase "a builtin can be let-bound and used at two types" $
                -- let h = head in if h [true] then h [1] else 0
                runs
                    ( ELet
                        (Bind "h" Nothing)
                        (EVar "head")
                        (EIf (EApp (EVar "h") (EList [ELit (LBool True)])) (EApp (EVar "h") (ints [1])) (lit 0))
                    )
                    int
                    (VInt 1)
            , testCase "a builtin can be shadowed" $
                -- let head = \x -> 0 in head 1
                runs (ELet (Bind "head" Nothing) (lam "x" (lit 0)) (call "head" [lit 1])) int (VInt 0)
            , testCase "a builtin's type is that of a function" $
                case typeOf builtinTypes (EVar "tail") of
                    Right (TCon (TCFun (TCon (TCList (TVar a))) (TCon (TCList (TVar a'))))) -> a @?= a'
                    other -> assertFailure $ "expected List a -> List a, got " ++ show other
            , testCase "builtins are never equal, even to themselves" $
                runEval builtinEnv (EOp OEq (EVar "head") (EVar "head")) @?= Right (VBool False)
            , testCase "builtins are available in programs" $ do
                -- let second (xs) = head (tail xs); second "abc"
                let prog =
                        [ TEFun (Bind "second" Nothing) [Bind "xs" Nothing] (call "head" [call "tail" [EVar "xs"]])
                        , TEExpr (call "second" [strLit "abc"])
                        ]
                typeOfProgram builtinTypes prog @?= Right [char]
                runEvalProgram builtinEnv prog @?= Right [VChar 'b']
            , testCase "recursive functions over lists" $ do
                -- let length (xs) = if empty? xs then 0 else 1 + length (tail xs)
                -- let map (f, xs) = if empty? xs then [] else f (head xs) ++ map f (tail xs)
                -- length "abc"; map (\x -> x + 1) [1, 2, 3]; map length ["a", "", "bc"]
                let prog =
                        [ TEFun
                            (Bind "length" Nothing)
                            [Bind "xs" Nothing]
                            ( EIf
                                (call "empty?" [EVar "xs"])
                                (lit 0)
                                (EOp OAdd (lit 1) (call "length" [call "tail" [EVar "xs"]]))
                            )
                        , TEFun
                            (Bind "map" Nothing)
                            [Bind "f" Nothing, Bind "xs" Nothing]
                            ( EIf
                                (call "empty?" [EVar "xs"])
                                (EList [])
                                ( EOp
                                    OCons
                                    (EApp (EVar "f") (call "head" [EVar "xs"]))
                                    (call "map" [EVar "f", call "tail" [EVar "xs"]])
                                )
                            )
                        , TEExpr (call "length" [strLit "abc"])
                        , TEExpr (call "map" [lam "x" (EOp OAdd (EVar "x") (lit 1)), ints [1, 2, 3]])
                        , TEExpr (call "map" [EVar "length", EList [strLit "a", strLit "", strLit "bc"]])
                        ]
                typeOfProgram builtinTypes prog @?= Right [int, list int, list int]
                runEvalProgram builtinEnv prog @?= Right [VInt 3, vints [2, 3, 4], vints [1, 0, 2]]
            , testCase "without the builtins, they are unbound" $ do
                assertLeft "should fail" (typeOf M.empty (EVar "head"))
                assertLeft "should fail" (runEval M.empty (EVar "head"))
            ]
        ]
