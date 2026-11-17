module Main (main) where

import Hmmm.ParserSpec (parserTests)
import Hmmm.TypeInference (typeInferenceTests)
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "hmmm-lang"
        [ parserTests
        , typeInferenceTests
        ]
