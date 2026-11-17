module Hmmm.ParserSpec (parserTests) where

import Helpers (assertLeft)
import Hmmm.Parser
import Hmmm.Syntax
import Test.Tasty
import Test.Tasty.HUnit

parsesTo :: String -> Expr -> Assertion
parsesTo input expected = runParse input @?= Right expected

rejects :: String -> Assertion
rejects input = assertLeft ("parsing " ++ show input ++ " should fail") (runParse input)

int :: Integer -> Expr
int = ELit . LInt

v :: Name -> Expr
v = EVar

parserTests :: TestTree
parserTests =
    testGroup
        "PARSING"
        [ testGroup
            "Literals and variables"
            [ testCase "integer" $ "16" `parsesTo` int 16
            , testCase "integer (hex)" $ "0x16" `parsesTo` int 22
            , testCase "integer (oct)" $ "0o16" `parsesTo` int 14
            , testCase "true" $ "true" `parsesTo` ELit (LBool True)
            , testCase "false" $ "false" `parsesTo` ELit (LBool False)
            , testCase "string" $ "\"hi\"" `parsesTo` ELit (LStr "hi")
            , testCase "variable" $ "x" `parsesTo` v "x"
            , testCase "variable starting with a keyword's first letter" $ do
                "foo" `parsesTo` v "foo"
                "tx" `parsesTo` v "tx"
                "left" `parsesTo` v "left"
            , testCase "variable with a keyword as a prefix" $ do
                "truey" `parsesTo` v "truey"
                "ift" `parsesTo` v "ift"
                "int" `parsesTo` v "int"
            , testCase "miscapitalized true/false parse as variables" $ do
                "True" `parsesTo` v "True"
                "False" `parsesTo` v "False"
            , testCase "alphanumeric variable" $ "x1" `parsesTo` v "x1"
            , -- FIXME: this parses just fine as App 1 x. i dont like that
              -- make numbers more robust then uncomment this
              -- testCase "variable cannot start with number" $ rejects "1x"
              testCase "keyword is not a variable" $ rejects "then"
            ]
        , testGroup
            "Whitespace"
            [ testCase "leading and trailing whitespace" $ "  x  " `parsesTo` v "x"
            , testCase "newlines" $ "let x = 1\nin x" `parsesTo` ELet "x" (int 1) (v "x")
            , testCase "no whitespace around operators" $ "1+2" `parsesTo` EOp OAdd (int 1) (int 2)
            , testCase "empty input fails" $ rejects ""
            , testCase "trailing garbage fails" $ rejects "1 )"
            ]
        , testGroup
            "Operators"
            [ testCase "addition" $ "1 + 2" `parsesTo` EOp OAdd (int 1) (int 2)
            , testCase "subtraction" $ "1 - 2" `parsesTo` EOp OSub (int 1) (int 2)
            , testCase "left associative" $
                "1 - 2 + 3" `parsesTo` EOp OAdd (EOp OSub (int 1) (int 2)) (int 3)
            , testCase "parentheses" $
                "1 - (2 + 3)" `parsesTo` EOp OSub (int 1) (EOp OAdd (int 2) (int 3))
            , testCase "dangling operator fails" $ rejects "1 +"
            ]
        , testGroup
            "Application"
            [ testCase "single argument" $ "f x" `parsesTo` EApp (v "f") (v "x")
            , testCase "left associative" $
                "f x y" `parsesTo` EApp (EApp (v "f") (v "x")) (v "y")
            , testCase "binds tighter than operators" $
                "f x + g y" `parsesTo` EOp OAdd (EApp (v "f") (v "x")) (EApp (v "g") (v "y"))
            , testCase "parenthesized argument" $
                "f (g x)" `parsesTo` EApp (v "f") (EApp (v "g") (v "x"))
            , testCase "literal arguments" $
                "f 1 true" `parsesTo` EApp (EApp (v "f") (int 1)) (ELit (LBool True))
            ]
        , testGroup
            "If"
            [ testCase "simple" $
                "if true then 1 else 2" `parsesTo` EIf (ELit (LBool True)) (int 1) (int 2)
            , testCase "variable condition" $
                "if x then 1 else 2" `parsesTo` EIf (v "x") (int 1) (int 2)
            , testCase "application in the condition stops at then" $
                "if f x then 1 else 2" `parsesTo` EIf (EApp (v "f") (v "x")) (int 1) (int 2)
            , testCase "else branch extends to the right" $
                "if c then 1 else 2 + 3" `parsesTo` EIf (v "c") (int 1) (EOp OAdd (int 2) (int 3))
            , testCase "nested" $
                "if a then if b then 1 else 2 else 3"
                    `parsesTo` EIf (v "a") (EIf (v "b") (int 1) (int 2)) (int 3)
            , testCase "missing else fails" $ rejects "if a then 1"
            ]
        , testGroup
            "Let"
            [ testCase "simple" $ "let x = 1 in x" `parsesTo` ELet "x" (int 1) (v "x")
            , testCase "operator in value and body" $
                "let x = 1 + 2 in x - 1"
                    `parsesTo` ELet "x" (EOp OAdd (int 1) (int 2)) (EOp OSub (v "x") (int 1))
            , testCase "nested" $
                "let x = 1 in let y = 2 in x"
                    `parsesTo` ELet "x" (int 1) (ELet "y" (int 2) (v "x"))
            , testCase "let-bound function" $
                "let id = \\x -> x in id 1"
                    `parsesTo` ELet "id" (EAbs "x" (v "x")) (EApp (v "id") (int 1))
            , testCase "keyword as binding name fails" $ rejects "let in = 1 in 2"
            ]
        , testGroup
            "Lambda"
            [ testCase "backslash" $ "\\x -> x" `parsesTo` EAbs "x" (v "x")
            , testCase "lambda symbol" $ "λx -> x" `parsesTo` EAbs "x" (v "x")
            , testCase "spaces after the backslash" $ "\\ x -> x" `parsesTo` EAbs "x" (v "x")
            , testCase "body extends to the right" $
                "\\x -> x + 1" `parsesTo` EAbs "x" (EOp OAdd (v "x") (int 1))
            , testCase "curried" $
                "\\x -> \\y -> x" `parsesTo` EAbs "x" (EAbs "y" (v "x"))
            , testCase "arrow is not minus" $
                "\\x -> x - 1" `parsesTo` EAbs "x" (EOp OSub (v "x") (int 1))
            , testCase "applied lambda" $
                "(\\x -> x) 1" `parsesTo` EApp (EAbs "x" (v "x")) (int 1)
            , testCase "variable named f" $
                "\\f -> f 1" `parsesTo` EAbs "f" (EApp (v "f") (int 1))
            ]
        ]
