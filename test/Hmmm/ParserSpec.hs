module Hmmm.ParserSpec (parserTests) where

import Helpers (assertLeft, strLit)
import Hmmm.Parser
import Hmmm.Syntax
import Hmmm.Types
import Test.Tasty
import Test.Tasty.HUnit

parsesTo :: String -> Expr -> Assertion
parsesTo input expected = runParse input @?= Right expected

rejects :: String -> Assertion
rejects input = assertLeft ("parsing " ++ show input ++ " should fail") (runParse input)

-- | parse a whole program of top-level items
programIs :: String -> [TopExpr] -> Assertion
programIs input expected = runParseTopExprs input @?= Right expected

rejectsProgram :: String -> Assertion
rejectsProgram input = assertLeft ("parsing program " ++ show input ++ " should fail") (runParseTopExprs input)

int :: Integer -> Expr
int = ELit . LInt

v :: Name -> Expr
v = EVar

-- | an unannotated binding
b :: Name -> Binding
b n = Bind n Nothing

-- | an annotated binding
(.:) :: Name -> Type -> Binding
n .: t = Bind n (Just t)

infix 4 .:

tInt, tBool, tStr :: Type
tInt = T CInt
tBool = T CBool
tStr = TC (TCList (T CChar))

(~>) :: Type -> Type -> Type
a ~> r = TC (TCFun a r)
infixr 5 ~>

tChar :: Type
tChar = T CChar

tList :: Type -> Type
tList = TC . TCList

-- | the type annotation of @let x: <ann> = x in x@
annotates :: String -> Type -> Assertion
annotates ann t = ("let x: " ++ ann ++ " = x in x") `parsesTo` ELet ("x" .: t) (v "x") (v "x")

infix 1 `annotates`

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
            , testCase "string" $ "\"hi\"" `parsesTo` strLit "hi"
            , testCase "char" $ "'a'" `parsesTo` ELit (LChar 'a')
            , testCase "a string is a list of chars" $
                "\"ab\"" `parsesTo` EList [ELit (LChar 'a'), ELit (LChar 'b')]
            , testCase "a string is the same as its list of chars" $
                runParse "\"ab\"" @?= runParse "['a', 'b']"
            , testCase "the empty string is the empty list" $ "\"\"" `parsesTo` EList []
            , testCase "strings with spaces" $ "\"a b\"" `parsesTo` strLit "a b"
            , testCase "strings as list elements" $
                "[\"a\", \"bc\"]" `parsesTo` EList [strLit "a", strLit "bc"]
            , testCase "strings as arguments" $
                "f \"a\" 'b'" `parsesTo` EApp (EApp (v "f") (strLit "a")) (ELit (LChar 'b'))
            , testCase "a char with several characters fails" $ rejects "'ab'"
            , testCase "an empty char fails" $ rejects "''"
            , testCase "an unclosed string fails" $ rejects "\"ab"
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
            , testCase "variable with a question mark" $ do
                "empty?" `parsesTo` v "empty?"
                "a?b" `parsesTo` v "a?b"
            , testCase "variable cannot start with a question mark" $ rejects "?x"
            , testCase "a predicate applied to an argument" $
                "empty? xs" `parsesTo` EApp (v "empty?") (v "xs")
            , -- FIXME: this parses just fine as App 1 x. i dont like that
              -- make numbers more robust then uncomment this
              -- testCase "variable cannot start with number" $ rejects "1x"
              testCase "keyword is not a variable" $ rejects "then"
            ]
        , testGroup
            "Whitespace"
            [ testCase "leading and trailing whitespace" $ "  x  " `parsesTo` v "x"
            , testCase "newlines" $ "let x = 1\nin x" `parsesTo` ELet (b "x") (int 1) (v "x")
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
            , testCase "equality" $ "1 == 2" `parsesTo` EOp OEq (int 1) (int 2)
            , testCase "inequality" $ "1 /= 2" `parsesTo` EOp ONeq (int 1) (int 2)
            , testCase "greater than" $ "1 > 2" `parsesTo` EOp OGt (int 1) (int 2)
            , testCase "greater than or equal" $ "1 >= 2" `parsesTo` EOp OGeq (int 1) (int 2)
            , testCase "less than" $ "1 < 2" `parsesTo` EOp OLt (int 1) (int 2)
            , testCase "less than or equal" $ "1 <= 2" `parsesTo` EOp OLeq (int 1) (int 2)
            , testCase "and" $ "a and b" `parsesTo` EOp OAnd (v "a") (v "b")
            , testCase "or" $ "a or b" `parsesTo` EOp OOr (v "a") (v "b")
            , testCase "multi-character operators without whitespace" $ do
                "1==2" `parsesTo` EOp OEq (int 1) (int 2)
                "1<=2" `parsesTo` EOp OLeq (int 1) (int 2)
                "1>=2" `parsesTo` EOp OGeq (int 1) (int 2)
            , testCase "= is not ==" $ rejects "1 = 2"
            , testCase "unknown operator fails" $ rejects "1 * 2"
            , testCase "arithmetic binds tighter than comparison" $
                "1 + 2 == 3 - 4"
                    `parsesTo` EOp OEq (EOp OAdd (int 1) (int 2)) (EOp OSub (int 3) (int 4))
            , testCase "comparison binds tighter than and/or" $
                "1 < 2 and 3 >= 4"
                    `parsesTo` EOp OAnd (EOp OLt (int 1) (int 2)) (EOp OGeq (int 3) (int 4))
            , testCase "application binds tighter than comparison" $
                "f x == g y" `parsesTo` EOp OEq (EApp (v "f") (v "x")) (EApp (v "g") (v "y"))
            , testCase "and binds tighter than or" $ do
                "a and b or c" `parsesTo` EOp OOr (EOp OAnd (v "a") (v "b")) (v "c")
                "a or b and c" `parsesTo` EOp OOr (v "a") (EOp OAnd (v "b") (v "c"))
                "a and b or c and d"
                    `parsesTo` EOp OOr (EOp OAnd (v "a") (v "b")) (EOp OAnd (v "c") (v "d"))
            , testCase "and/or are left associative" $ do
                "a and b and c" `parsesTo` EOp OAnd (EOp OAnd (v "a") (v "b")) (v "c")
                "a or b or c" `parsesTo` EOp OOr (EOp OOr (v "a") (v "b")) (v "c")
            , testCase "comparisons do not chain" $ do
                rejects "a == b == c"
                rejects "a < b < c"
                rejects "a < b == c"
            , testCase "parenthesized comparisons can be chained" $ do
                "(a == b) == c" `parsesTo` EOp OEq (EOp OEq (v "a") (v "b")) (v "c")
                "a == (b < c)" `parsesTo` EOp OEq (v "a") (EOp OLt (v "b") (v "c"))
            , testCase "comparisons can be joined with and/or" $
                "a < b and b < c or d"
                    `parsesTo` EOp
                        OOr
                        (EOp OAnd (EOp OLt (v "a") (v "b")) (EOp OLt (v "b") (v "c")))
                        (v "d")
            , testCase "and/or are keywords, not variables" $ do
                rejects "and"
                rejects "or"
            , testCase "variables may start with and/or" $
                "andy orc" `parsesTo` EApp (v "andy") (v "orc")
            , testCase "dangling and fails" $ rejects "a and"
            , testCase "cons" $ "1 ++ xs" `parsesTo` EOp OCons (int 1) (v "xs")
            , testCase "append" $ "xs +++ ys" `parsesTo` EOp OAppend (v "xs") (v "ys")
            , testCase "cons is right associative" $
                "1 ++ 2 ++ []" `parsesTo` EOp OCons (int 1) (EOp OCons (int 2) (EList []))
            , testCase "append is right associative" $
                "a +++ b +++ c" `parsesTo` EOp OAppend (v "a") (EOp OAppend (v "b") (v "c"))
            , testCase "cons and append mix" $
                "1 ++ a +++ b" `parsesTo` EOp OCons (int 1) (EOp OAppend (v "a") (v "b"))
            , testCase "+, ++ and +++ are distinct" $
                "1+2++a+++b"
                    `parsesTo` EOp OCons (EOp OAdd (int 1) (int 2)) (EOp OAppend (v "a") (v "b"))
            , testCase "arithmetic binds tighter than cons" $
                "x + 1 ++ xs" `parsesTo` EOp OCons (EOp OAdd (v "x") (int 1)) (v "xs")
            , testCase "cons binds tighter than comparison" $
                "1 ++ xs == ys" `parsesTo` EOp OEq (EOp OCons (int 1) (v "xs")) (v "ys")
            ]
        , testGroup
            "Lists"
            [ testCase "empty list" $ "[]" `parsesTo` EList []
            , testCase "empty list with whitespace" $ "[ ]" `parsesTo` EList []
            , testCase "singleton" $ "[1]" `parsesTo` EList [int 1]
            , testCase "several elements" $ "[1, 2, 3]" `parsesTo` EList [int 1, int 2, int 3]
            , testCase "no whitespace" $ "[1,2]" `parsesTo` EList [int 1, int 2]
            , testCase "elements are full expressions" $
                "[f 1, a, b]" `parsesTo` EList [EApp (v "f") (int 1), v "a", v "b"]
            , testCase "elements with operators, lambdas and ifs" $
                "[1 + 2, \\x -> x, if c then 1 else 2]"
                    `parsesTo` EList
                        [ EOp OAdd (int 1) (int 2)
                        , EAbs (b "x") (v "x")
                        , EIf (v "c") (int 1) (int 2)
                        ]
            , testCase "nested" $
                "[[1], [], [2, 3]]" `parsesTo` EList [EList [int 1], EList [], EList [int 2, int 3]]
            , testCase "a list is an application argument" $
                "f [1] x" `parsesTo` EApp (EApp (v "f") (EList [int 1])) (v "x")
            , testCase "a list in a let" $
                "let xs = [1] in xs" `parsesTo` ELet (b "xs") (EList [int 1]) (v "xs")
            , testCase "multi-line list" $ "[ 1\n  , 2\n  ]" `parsesTo` EList [int 1, int 2]
            , testCase "unclosed list fails" $ rejects "[1, 2"
            , testCase "trailing comma fails" $ rejects "[1, 2,]"
            , testCase "leading comma fails" $ rejects "[, 1]"
            , testCase "missing comma fails" $ rejects "[1 2 3,]"
            , testCase "lone comma fails" $ rejects "[,]"
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
            [ testCase "simple" $ "let x = 1 in x" `parsesTo` ELet (b "x") (int 1) (v "x")
            , testCase "operator in value and body" $
                "let x = 1 + 2 in x - 1"
                    `parsesTo` ELet (b "x") (EOp OAdd (int 1) (int 2)) (EOp OSub (v "x") (int 1))
            , testCase "nested" $
                "let x = 1 in let y = 2 in x"
                    `parsesTo` ELet (b "x") (int 1) (ELet (b "y") (int 2) (v "x"))
            , testCase "let-bound function" $
                "let id = \\x -> x in id 1"
                    `parsesTo` ELet (b "id") (EAbs (b "x") (v "x")) (EApp (v "id") (int 1))
            , testCase "keyword as binding name fails" $ rejects "let in = 1 in 2"
            ]
        , testGroup
            "Lambda"
            [ testCase "backslash" $ "\\x -> x" `parsesTo` EAbs (b "x") (v "x")
            , testCase "lambda symbol" $ "λx -> x" `parsesTo` EAbs (b "x") (v "x")
            , testCase "spaces after the backslash" $ "\\ x -> x" `parsesTo` EAbs (b "x") (v "x")
            , testCase "body extends to the right" $
                "\\x -> x + 1" `parsesTo` EAbs (b "x") (EOp OAdd (v "x") (int 1))
            , testCase "curried" $
                "\\x -> \\y -> x" `parsesTo` EAbs (b "x") (EAbs (b "y") (v "x"))
            , testCase "arrow is not minus" $
                "\\x -> x - 1" `parsesTo` EAbs (b "x") (EOp OSub (v "x") (int 1))
            , testCase "applied lambda" $
                "(\\x -> x) 1" `parsesTo` EApp (EAbs (b "x") (v "x")) (int 1)
            , testCase "variable named f" $
                "\\f -> f 1" `parsesTo` EAbs (b "f") (EApp (v "f") (int 1))
            ]
        , testGroup
            "Type annotations"
            [ testCase "let with a concrete type" $
                "let a: int = 1 in a" `parsesTo` ELet ("a" .: tInt) (int 1) (v "a")
            , testCase "let with a function type" $
                "let f: bool -> str = \\x -> \"s\" in f"
                    `parsesTo` ELet ("f" .: tBool ~> tStr) (EAbs (b "x") (strLit "s")) (v "f")
            , testCase "function types are right-associative" $
                "let f: int -> int -> int = g in f"
                    `parsesTo` ELet ("f" .: tInt ~> (tInt ~> tInt)) (v "g") (v "f")
            , testCase "parenthesized function argument type" $
                "let f: (int -> int) -> int = g in f"
                    `parsesTo` ELet ("f" .: (tInt ~> tInt) ~> tInt) (v "g") (v "f")
            , testCase "no spaces around the colon" $
                "let a:int = 1 in a" `parsesTo` ELet ("a" .: tInt) (int 1) (v "a")
            , testCase "annotated lambda parameter" $
                "\\(x: int) -> x" `parsesTo` EAbs ("x" .: tInt) (v "x")
            , testCase "unknown type name fails" $ rejects "let a: float = 1 in a"
            , testCase "missing type fails" $ rejects "let a: = 1 in a"
            , testCase "unparenthesized annotated lambda fails" $ rejects "\\x: int -> x"
            , testCase "type names are still valid variables" $
                "int bool" `parsesTo` EApp (v "int") (v "bool")
            , testCase "list is still a valid variable" $ "list" `parsesTo` v "list"
            ]
        , testGroup
            "List types"
            [ testCase "[T]" $ "[int]" `annotates` tList tInt
            , testCase "list T" $ "list int" `annotates` tList tInt
            , testCase "both spellings are the same type" $ do
                "[bool]" `annotates` tList tBool
                "list bool" `annotates` tList tBool
            , testCase "nested [[T]]" $ "[[str]]" `annotates` tList (tList tStr)
            , testCase "nested list list T" $ "list list str" `annotates` tList (tList tStr)
            , testCase "the spellings can be mixed" $ do
                "list [int]" `annotates` tList (tList tInt)
                "[list int]" `annotates` tList (tList tInt)
            , testCase "list binds tighter than ->" $
                "list int -> bool" `annotates` tList tInt ~> tBool
            , testCase "list of functions needs parentheses" $
                "list (int -> bool)" `annotates` tList (tInt ~> tBool)
            , testCase "brackets delimit a function type" $
                "[int -> bool]" `annotates` tList (tInt ~> tBool)
            , testCase "function returning a list" $
                "int -> [int]" `annotates` tInt ~> tList tInt
            , testCase "list types on lambda parameters" $
                "\\(xs: [int]) -> xs" `parsesTo` EAbs ("xs" .: tList tInt) (v "xs")
            , testCase "list types in a top-level function" $
                "let f (xs: list int): [int] = xs"
                    `programIs` [TEFun ("f" .: tList tInt) ["xs" .: tList tInt] (v "xs")]
            , testCase "char" $ "char" `annotates` tChar
            , testCase "str is [char]" $ do
                "str" `annotates` tList tChar
                "[char]" `annotates` tList tChar
                "list char" `annotates` tList tChar
            , testCase "str inside other types" $ do
                "[str]" `annotates` tList (tList tChar)
                "str -> str" `annotates` tList tChar ~> tList tChar
            , testCase "list without an element type fails" $ rejects "let x: list = x in x"
            , testCase "empty brackets are not a type" $ rejects "let x: [] = x in x"
            , testCase "unclosed list type fails" $ rejects "let x: [int = x in x"
            , testCase "list type of an unknown type fails" $ rejects "let x: [float] = x in x"
            ]
        , testGroup
            "Top level"
            [ testCase "example program" $
                "let foo (a: int): int = a + 1\nlet g = 9\nfoo g"
                    `programIs` [ TEFun ("foo" .: tInt) ["a" .: tInt] (EOp OAdd (v "a") (int 1))
                                , TELet (b "g") (int 9)
                                , TEExpr (EApp (v "foo") (v "g"))
                                ]
            , testCase "value definition" $ "let g = 9" `programIs` [TELet (b "g") (int 9)]
            , testCase "annotated value definition" $
                "let g: int = 9" `programIs` [TELet ("g" .: tInt) (int 9)]
            , testCase "function without a return type" $
                "let f (x) = x" `programIs` [TEFun (b "f") [b "x"] (v "x")]
            , testCase "function with several parameters" $
                "let f (x: int, y) = x"
                    `programIs` [TEFun (b "f") ["x" .: tInt, b "y"] (v "x")]
            , testCase "function with a function return type" $
                "let f (x: int): int -> int = \\y -> x"
                    `programIs` [TEFun ("f" .: tInt ~> tInt) ["x" .: tInt] (EAbs (b "y") (v "x"))]
            , testCase "a new line starts a new item" $
                "f x\ng y"
                    `programIs` [TEExpr (EApp (v "f") (v "x")), TEExpr (EApp (v "g") (v "y"))]
            , testCase "an indented line continues the item" $
                "f x\n  y" `programIs` [TEExpr (EApp (EApp (v "f") (v "x")) (v "y"))]
            , testCase "an indented operator continues the item" $
                "let a = 1\n  + 2" `programIs` [TELet (b "a") (EOp OAdd (int 1) (int 2))]
            , testCase "a line starting with an operator is not a continuation" $
                rejectsProgram "let a = 1\n+ 2"
            , testCase "top-level let ... in is an expression" $
                "let x = 1 in x" `programIs` [TEExpr (ELet (b "x") (int 1) (v "x"))]
            , testCase "let ... in followed by another item" $
                "let x = 1 in x\nlet y = 2"
                    `programIs` [TEExpr (ELet (b "x") (int 1) (v "x")), TELet (b "y") (int 2)]
            , testCase "blank lines and surrounding whitespace" $
                "\nlet g = 9\n\n\nfoo g\n"
                    `programIs` [TELet (b "g") (int 9), TEExpr (EApp (v "foo") (v "g"))]
            , testCase "a let expression nested in a definition" $
                "let f (x) = let y = x in y\nf 1"
                    `programIs` [ TEFun (b "f") [b "x"] (ELet (b "y") (v "x") (v "y"))
                                , TEExpr (EApp (v "f") (int 1))
                                ]
            , testCase "two items on one line fail" $ rejectsProgram "let g = 9 let h = 1"
            , testCase "empty program fails" $ rejectsProgram ""
            ]
        ]
