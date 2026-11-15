module Main (main) where

import Hmmm.InferSpec (endToEndTests, inferTests)
import Hmmm.TypesSpec (substitutionTests)
import Hmmm.UnifySpec (unifyTests)
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "hmmm-lang"
        [ substitutionTests
        , unifyTests
        , inferTests
        , endToEndTests
        ]
