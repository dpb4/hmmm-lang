module Main (main) where

import Hmmm.BuiltinsSpec (builtinTests)
import Hmmm.EvalSpec (evalTests)
import Hmmm.ParserSpec (parserTests)
import Hmmm.PreludeSpec (preludeTests)
import Hmmm.SyntaxSpec (syntaxTests)
import Hmmm.TypeInference (typeInferenceTests)
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "hmmm-lang"
        [ syntaxTests
        , parserTests
        , typeInferenceTests
        , evalTests
        , builtinTests
        , preludeTests
        ]
