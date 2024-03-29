{-# LANGUAGE RankNTypes #-}

module Myml.Typing.Spec
  ( tests,
  )
where

import Data.Either
import Data.Equivalence.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import Myml.Syntax
import Myml.Test.Helper
import Myml.Typing
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Myml.Typing.Spec" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup "Unit tests" [describeTests, instantiateTests, inferTests]

emptyInferenceState :: InferenceState
emptyInferenceState = InferenceState (NewVar Map.empty) True

unifyAndTestDescribe :: [(String, String)] -> String -> String -> Assertion
unifyAndTestDescribe us t1 t2 =
  fst (runInference m Map.empty emptyInferenceState) @?= Right (pType t2)
  where
    m :: forall s. Inference s Type
    m = do
      mapM_ (\(a, b) -> unifyProper (pType a) (pType b)) us
      describeProper False Set.empty (pType t1)

testInstantiate :: String -> String -> [(String, String)] -> Assertion
testInstantiate s t pairs =
  fst (runInference m Map.empty emptyInferenceState)
    @?= Right (pType t)
  where
    m :: forall s. Inference s Type
    m = do
      instantiated <- instantiate (pScheme s)
      mapM_
        ( \(a, b) -> do
            a' <- classDesc (TySubProper (pType a))
            if a' == TySubProper (pType b)
              then return ()
              else error ("assert failed: classDesc " ++ a ++ " != " ++ b)
        )
        pairs
      return instantiated

testInference :: String -> Bool -> Assertion
testInference str success = isRight res @?= success
  where
    t = pTerm str
    (res, _) =
      runInference (infer t >>= generalize t) Map.empty emptyInferenceState

describeTests :: TestTree
describeTests =
  testGroup
    "Describe Tests"
    [ testCase "describe single variable" $
        unifyAndTestDescribe [("X", "Y")] "X" "Y",
      testCase "describe Unit" $ unifyAndTestDescribe [] "Unit" "Unit",
      testCase "describe Nat" $ unifyAndTestDescribe [] "Nat" "Nat",
      testCase "describe Bool" $ unifyAndTestDescribe [] "Bool" "Bool",
      testCase "describe Arrow" $
        unifyAndTestDescribe [("X", "Bool"), ("Y", "Nat")] "X -> Y" "Bool -> Nat",
      testCase "describe Rec 1" $
        unifyAndTestDescribe
          [("X", "X -> Bool"), ("Y", "Nat")]
          "X -> Y"
          "(\x3bc X . X -> Bool) -> Nat",
      testCase "describe Rec 2" $
        unifyAndTestDescribe
          [("X", "X -> Y"), ("Y", "Y -> Nat")]
          "X"
          "(\x3bc X . X -> (\x3bc Y . Y -> Nat))",
      testCase "describe Row 1" $
        unifyAndTestDescribe
          [("{ X }", "{ l : Present Z, Y }"), ("Z", "Unit")]
          "{ X }"
          "{ l : Present Unit, Y }", -- maybe changed
      testCase "describe Row 2" $
        unifyAndTestDescribe
          [("{ X }", "{ l : Present Z, · }"), ("Z", "Unit")]
          "{ X }"
          "{ l : Present Unit, · }",
      testCase "describe Row 3" $
        unifyAndTestDescribe
          [ ("{ X }", "{ l1 : L1, Y }"),
            ("{ Y }", "{ l2 : L2, Z }"),
            ("{ Z }", "{ l3 : L3, · }"),
            ("{ l : L1, · }", "{ l : Present Unit, · }"),
            ("{ l : L2, · }", "{ l : Present Nat, · }"),
            ("{ l : L3, · }", "{ l : Absent, · }")
          ]
          "{ X }"
          "{ l1 : Present Unit, l2 : Present Nat, l3 : Absent, · }",
      testCase "describe Row 4" $
        unifyAndTestDescribe
          [ ("{ X }", "{ l1 : L1, Y }"),
            ("{ Y }", "{ l2 : L2, Z }"),
            ("{ X }", "{ l1 : Absent, l2 : Present Nat, · }")
          ]
          "{ Y }"
          "{ l2 : Present Nat, · }"
    ]

instantiateTests :: TestTree
instantiateTests =
  testGroup
    "Describe Tests"
    [ testCase "class self reference" $
        testInstantiate
          "∀ α :: * . ∀ α1 :: * . ∀ α2 :: * . ∀ α3 :: * . ∀ ψ :: * => Presence . α -> (Unit -> α3) -> α1 -> { returnSelf : ψ α2 -> α3, · }"
          "_α -> (Unit -> _α3) -> _α1 -> { returnSelf : _ψ _α2 -> _α3, · }"
          []
    ]

inferTests :: TestTree
inferTests =
  testGroup
    "Inference tests"
    [ testCase "unbounded variable" $ testInference "λ x . y" False,
      testCase "wildcard" $ testInference "λ _ . _" False,
      testCase "wildcard let" $ testInference "let _ = λ x . x in _" False,
      testCase "normal lambda" $ testInference "λ x . x" True,
      testCase "normal let" $ testInference "let x = λ x . x in x" True,
      testCase "if record 1" $
        testInference
          "(if true then { l1 = λ x . x } else { l1 = λ x . x }).l1"
          True,
      testCase "if record 2" $
        testInference
          "(if true then { l2 = λ x . x } else { l1 = λ x . x }).l1"
          False,
      testCase "if record 3" $
        testInference
          "(if true then { l2 = λ x . x, l1 = λ x . x } else { l3 = λ x . x, l1 = λ x . x }).l1"
          True,
      testCase "record accesses" $ testInference "λ r . r.l1.l2.l3" True,
      testCase "diverge" $ testInference "(λ x . x x) (λ x . x x)" True,
      testCase "fix" $
        testInference "λ f . (λ x . f (λ v . x x v)) (λ x . f (λ v . x x v))" True
    ]
