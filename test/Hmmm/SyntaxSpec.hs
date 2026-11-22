module Hmmm.SyntaxSpec (syntaxTests) where

import Hmmm.Syntax
import Test.Tasty
import Test.Tasty.HUnit

syntaxTests :: TestTree
syntaxTests =
    testGroup
        "SYNTAX"
        [ testGroup
            "Showing literals as source code"
            [ testCase "int" $ show (LInt 12) @?= "12"
            , testCase "negative int" $ show (LInt (-3)) @?= "-3"
            , testCase "true" $ show (LBool True) @?= "true"
            , testCase "false" $ show (LBool False) @?= "false"
            , testCase "char" $ show (LChar 'a') @?= "'a'"
            , testCase "quote char" $ show (LChar '\'') @?= "'\\''"
            , testCase "a literal in an expression" $
                show (EOp OAdd (EVar "n") (ELit (LInt 1))) @?= "n + 1"
            , testCase "a lambda body" $
                show (EAbs (Bind "b" Nothing) (EIf (EVar "b") (ELit (LBool False)) (ELit (LBool True))))
                    @?= "λb -> if b then false else true"
            ]
        ]
