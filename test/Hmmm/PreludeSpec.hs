module Hmmm.PreludeSpec (preludeTests) where

import qualified Data.Map as M
import Helpers
import Hmmm.Eval
import Hmmm.Infer
import Hmmm.Parser
import Hmmm.Prelude
import Hmmm.Types
import Test.Tasty
import Test.Tasty.HUnit

-- | run an assertion in the scope of the installed prelude
withLoaded :: (Scope -> Assertion) -> Assertion
withLoaded k = loadPrelude >>= either assertFailure k

-- | parse, check and run an expression with the prelude loaded, and compare its displayed value
evaluates :: String -> String -> Assertion
evaluates src expected = withLoaded $ \(Scope ctx env) -> case runParse src of
    Left err -> assertFailure $ "parse error: " ++ err
    Right e -> case (typeOf ctx e, runEval env e) of
        (Right t, Right v) -> display t v @?= expected
        (Left err, _) -> assertFailure $ "type error: " ++ err
        (_, Left err) -> assertFailure $ "runtime error: " ++ err

-- | an expression that does not type check with the prelude loaded
rejected :: String -> Assertion
rejected src = withLoaded $ \(Scope ctx _) ->
    assertLeft (src ++ " should not type check") (runParse src >>= typeOf ctx)

-- | the type of a prelude name
typeOfName :: String -> (TypeMaybe -> Assertion) -> Assertion
typeOfName n k = withLoaded $ \(Scope ctx _) -> either assertFailure k (runParse n >>= typeOf ctx)

preludeTests :: TestTree
preludeTests =
    testGroup
        "PRELUDE"
        [ loadingTests
        , libraryTests
        ]

loadingTests :: TestTree
loadingTests =
    testGroup
        "Loading"
        [ testCase "the installed prelude loads" $ withLoaded (const (return ()))
        , testCase "the prelude adds to the builtins" $ withLoaded $ \(Scope ctx env) -> do
            assertBool "head should still be in scope" (M.member "head" ctx && M.member "head" env)
            assertBool "map should be in scope" (M.member "map" ctx && M.member "map" env)
        , testCase "the prelude's types and values define the same names" $
            withLoaded $ \(Scope ctx env) -> M.keys ctx @?= M.keys env
        , testCase "prelude definitions are fully generalized" $ withLoaded $ \(Scope ctx _) ->
            mapM_
                (\(n, t) -> assertBool (n ++ " has free type variables") (null (freeTypeVars t)))
                (M.toList ctx)
        , testCase "a prelude can use the builtins and its earlier definitions" $
            assertRight $ withPrelude builtinScope "let one (xs) = head xs\nlet two (xs) = one (tail xs)"
        , testCase "comments are allowed" $
            assertRight $ withPrelude builtinScope "-- a comment\nlet x = 1 -- another\n"
        , testCase "expressions are not allowed" $
            assertLeft "should fail" $ toEither (withPrelude builtinScope "let x = 1\nx")
        , testCase "parse errors are reported" $
            assertLeft "should fail" $ toEither (withPrelude builtinScope "let = 1")
        , testCase "type errors are reported" $
            assertLeft "should fail" $ toEither (withPrelude builtinScope "let x = 1 + true")
        , testCase "a program can shadow a prelude definition" $ withLoaded $ \(Scope ctx env) -> do
            let prog = runParseTopExprs "let map (x) = x + 1\nmap 1"
            (prog >>= typeOfProgram ctx) @?= Right [int]
            (prog >>= runEvalProgram env) @?= Right [VInt 2]
        ]
  where
    assertRight = either assertFailure (const (return ())) . toEither
    toEither = fmap (const ())

libraryTests :: TestTree
libraryTests =
    testGroup
        "Library"
        [ testCase "not" $ do
            "not true" `evaluates` "False"
            "not false" `evaluates` "True"
        , testCase "foldr" $ do
            "foldr (\\x -> \\acc -> x ++ acc) [] [1, 2, 3]" `evaluates` "[1, 2, 3]"
            "foldr (\\a -> \\b -> a - b) 0 [10, 3]" `evaluates` "7"
        , testCase "foldl" $ do
            "foldl (\\acc -> \\x -> x ++ acc) [] [1, 2, 3]" `evaluates` "[3, 2, 1]"
            "foldl (\\a -> \\b -> a - b) 0 [10, 3]" `evaluates` "-13"
        , testCase "map" $ do
            "map (\\x -> x + 1) [1, 2, 3]" `evaluates` "[2, 3, 4]"
            "map (\\c -> c == 'a') \"ab\"" `evaluates` "[True, False]"
            "map head [\"ab\", \"cd\"]" `evaluates` "\"ac\""
            "map not []" `evaluates` "[]"
        , testCase "filter" $ do
            "filter (\\x -> x > 1) [1, 2, 3]" `evaluates` "[2, 3]"
            "filter (\\c -> not (c == ' ')) \"a b c\"" `evaluates` "\"abc\""
            "filter (\\x -> false) [1]" `evaluates` "[]"
        , testCase "reverse" $ do
            "reverse [1, 2, 3]" `evaluates` "[3, 2, 1]"
            "reverse \"abc\"" `evaluates` "\"cba\""
        , testCase "concat" $ do
            "concat [[1], [], [2, 3]]" `evaluates` "[1, 2, 3]"
            "concat [\"ab\", \"cd\"]" `evaluates` "\"abcd\""
        , testCase "length" $ do
            "length [1, 2, 3]" `evaluates` "3"
            "length \"\"" `evaluates` "0"
        , testCase "sum" $ do
            "sum [1, 2, 3]" `evaluates` "6"
            "sum []" `evaluates` "0"
        , testCase "any?" $ do
            "any? (\\x -> x > 2) [1, 3]" `evaluates` "True"
            "any? (\\x -> x > 2) [1, 2]" `evaluates` "False"
            "any? (\\x -> true) []" `evaluates` "False"
        , testCase "all?" $ do
            "all? (\\x -> x > 0) [1, 3]" `evaluates` "True"
            "all? (\\x -> x > 1) [1, 3]" `evaluates` "False"
            "all? (\\x -> false) []" `evaluates` "True"
        , testCase "elem?" $ do
            "elem? 'b' \"abc\"" `evaluates` "True"
            "elem? 4 [1, 2, 3]" `evaluates` "False"
            "elem? \"b\" [\"a\", \"b\"]" `evaluates` "True"
        , testCase "take" $ do
            "take 2 [1, 2, 3]" `evaluates` "[1, 2]"
            "take 5 \"ab\"" `evaluates` "\"ab\""
            "take 0 [1]" `evaluates` "[]"
        , testCase "drop" $ do
            "drop 2 [1, 2, 3]" `evaluates` "[3]"
            "drop 5 \"ab\"" `evaluates` "\"\""
            "drop 0 [1]" `evaluates` "[1]"
        , testCase "range" $ do
            "range 1 4" `evaluates` "[1, 2, 3, 4]"
            "range 3 3" `evaluates` "[3]"
            "range 2 1" `evaluates` "[]"
        , testCase "functions combine" $
            "sum (map (\\x -> x + x) (filter (\\x -> x > 2) (range 1 5)))" `evaluates` "24"
        , testCase "library functions are polymorphic" $
            "length (map (\\c -> c == 'a') \"abc\") + length [[1]]" `evaluates` "4"
        , testCase "library functions are type checked" $ do
            rejected "map 1 [1]"
            rejected "sum \"abc\""
            rejected "filter (\\x -> x + 1) [1]"
            rejected "elem? 'a' [1]"
        , testCase "length : List a -> Int" $
            typeOfName "length" $ \t -> case t of
                TCon (TCFun (TCon (TCList (TVar _))) (TLit CInt)) -> return ()
                other -> assertFailure $ "expected List a -> Int, got " ++ show other
        ]
