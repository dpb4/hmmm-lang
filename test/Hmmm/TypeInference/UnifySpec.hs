module Hmmm.TypeInference.UnifySpec (unifyTests) where

import Control.Monad.Except (runExcept)
import qualified Data.Map as M
import Helpers
import Hmmm.Types
import Hmmm.Unify
import Test.Tasty
import Test.Tasty.HUnit

runUnify :: TypeMaybe -> TypeMaybe -> Either String Subst
runUnify a b = runExcept (unify a b)

unifyTests :: TestTree
unifyTests =
    testGroup
        "Unification"
        [ testCase "identical types give empty substitution" $
            runUnify (int ~> bool) (int ~> bool) @?= Right M.empty
        , testCase "variable on the left" $
            runUnify (var "a") int @?= Right (M.fromList [(tv "a", int)])
        , testCase "variable on the right" $
            runUnify int (var "a") @?= Right (M.fromList [(tv "a", int)])
        , testCase "mismatched literals fail" $
            assertLeft "Int ~ Bool should fail" (runUnify int bool)
        , testCase "function vs literal fails" $
            assertLeft "Int -> Int ~ Int should fail" (runUnify (int ~> int) int)
        , testCase "occurs check" $
            assertLeft "a ~ a -> Int should fail" (runUnify (var "a") (var "a" ~> int))
        , testCase "input bindings propagate to outputs" $ do
            let r = runUnify (var "a" ~> var "a") (int ~> var "b")
            fmap (`applySubst` var "a") r @?= Right int
            fmap (`applySubst` var "b") r @?= Right int
        , testCase "a -> a ~ Int -> Bool fails" $
            assertLeft "should fail" (runUnify (var "a" ~> var "a") (int ~> bool))
        , testCase "solve threads substitutions through constraints" $ do
            let r = runSolve [Constraint (var "a") (var "b"), Constraint (var "b") int]
            fmap (`applySubst` var "a") r @?= Right int
        ]
