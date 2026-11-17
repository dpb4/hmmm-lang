-- | all type inference tests; one module under TypeInference/ per source module
module Hmmm.TypeInference (typeInferenceTests) where

import Hmmm.TypeInference.InferSpec (endToEndTests, inferTests)
import Hmmm.TypeInference.TypesSpec (substitutionTests)
import Hmmm.TypeInference.UnifySpec (unifyTests)
import Test.Tasty

typeInferenceTests :: TestTree
typeInferenceTests =
    testGroup
        "TYPE INFERENCE"
        [ substitutionTests
        , unifyTests
        , inferTests
        , endToEndTests
        ]
