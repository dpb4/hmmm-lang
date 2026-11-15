module Hmmm.TypesSpec (substitutionTests) where

import qualified Data.Map as M
import qualified Data.Set as S
import Helpers
import Hmmm.Types
import Test.Tasty
import Test.Tasty.HUnit

substitutionTests :: TestTree
substitutionTests =
    testGroup
        "Substitution"
        [ testCase "applySubst replaces variables inside functions" $
            applySubst (M.fromList [(tv "a", int)]) (var "a" ~> var "b")
                @?= int ~> var "b"
        , testCase "compose s1 s2 applies s2 first, then s1" $ do
            let s1 = M.fromList [(tv "b", int)]
                s2 = M.fromList [(tv "a", var "b")]
                s = compose s1 s2
            applySubst s (var "a") @?= int
            applySubst s (var "b") @?= int
        , testCase "free vars of a polytype exclude quantified vars" $
            freeTypeVars (ForAll (S.singleton (tv "a")) (var "a" ~> var "b"))
                @?= S.singleton (tv "b")
        , testCase "applySubst on a polytype leaves quantified vars alone" $ do
            let ForAll _ t =
                    applySubst
                        (M.fromList [(tv "a", int), (tv "b", bool)])
                        (ForAll (S.singleton (tv "a")) (var "a" ~> var "b"))
            t @?= var "a" ~> bool
        ]
