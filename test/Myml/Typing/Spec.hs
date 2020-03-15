module Myml.Typing.Spec
  ( tests
  )
where

import           Myml.Syntax
import           Myml.Typing
import           Myml.Subst
import           Myml.Test.Helper
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.Equivalence.Monad
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

tests :: TestTree
tests = testGroup "Myml.Typing.Spec" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup "Unit tests" [describeTests, instantiateTests, regTreeEqTests]

emptyInferenceState :: InferenceState
emptyInferenceState = InferenceState (NewVar Map.empty) True

unifyAndTestDescribe :: [(String, String)] -> String -> String -> Assertion
unifyAndTestDescribe us t1 t2 =
  fst (runInference m Map.empty emptyInferenceState) @?= Right (pType t2)
 where
  m = do
    mapM_ (\(a, b) -> unifyProper (pType a) (pType b)) us
    describeProper Set.empty (pType t1)

testInstantiate :: String -> String -> [(String, String)] -> Assertion
testInstantiate s t pairs = fst (runInference m Map.empty emptyInferenceState)
  @?= Right (pType t)
 where
  m :: Inference Type
  m = do
    insted <- instantiate (pScheme s)
    mapM_
      (\(a, b) -> do
        a' <- classDesc (TySubProper (pType a))
        if a' == TySubProper (pType b)
          then return ()
          else error ("assert failed: classDesc " ++ a ++ " != " ++ b)
      )
      pairs
    return insted

describeTests :: TestTree
describeTests = testGroup
  "Describe Tests"
  [ testCase "describe single variable"
    $ unifyAndTestDescribe [("X", "Y")] "X" "Y"
  , testCase "describe Unit" $ unifyAndTestDescribe [] "Unit" "Unit"
  , testCase "describe Nat" $ unifyAndTestDescribe [] "Nat" "Nat"
  , testCase "describe Bool" $ unifyAndTestDescribe [] "Bool" "Bool"
  , testCase "describe Arrow"
    $ unifyAndTestDescribe [("X", "Bool"), ("Y", "Nat")] "X -> Y" "Bool -> Nat"
  , testCase "describe Rec 1" $ unifyAndTestDescribe
    [("X", "X -> Bool"), ("Y", "Nat")]
    "X -> Y"
    "(\x3bc X . X -> Bool) -> Nat"
  , testCase "describe Rec 2" $ unifyAndTestDescribe
    [("X", "X -> Y"), ("Y", "Y -> Nat")]
    "X"
    "(\x3bc X . X -> (\x3bc Y . Y -> Nat))"
  , testCase "describe Row 1" $ unifyAndTestDescribe
    [("{ | X }", "{ l : Present Z | Y }"), ("Z", "Unit")]
    "{ | X }"
    "{ l : Present Unit | \x3b2 }" -- maybe changed
  , testCase "describe Row 2" $ unifyAndTestDescribe
    [("{ | X }", "{ l : Present Z }"), ("Z", "Unit")]
    "{ | X }"
    "{ l : Present Unit }"
  , testCase "describe Row 3" $ unifyAndTestDescribe
    [ ("{ | X }"   , "{ l1 : L1 | Y }")
    , ("{ | Y }"   , "{ l2 : L2 | Z }")
    , ("{ | Z }"   , "{ l3 : L3 }")
    , ("{ l : L1 }", "{ l : Present Unit }")
    , ("{ l : L2 }", "{ l : Present Nat }")
    , ("{ l : L3 }", "{ l : Absent }")
    ]
    "{ | X }"
    "{ l1 : Present Unit, l2 : Present Nat, l3 : Absent }"
  , testCase "describe Row 4" $ unifyAndTestDescribe
    [ ("{ | X }", "{ l1 : L1 | Y }")
    , ("{ | Y }", "{ l2 : L2 | Z }")
    , ("{ | X }", "{ l1 : Absent, l2 : Present Nat }")
    ]
    "{ | Y }"
    "{ l2 : Present Nat }"
  ]

instantiateTests :: TestTree
instantiateTests = testGroup
  "Describe Tests"
  [ testCase "class self reference" $ testInstantiate
      "∀ α :: * . ∀ α1 :: * . ∀ α2 :: * . ∀ α3 :: * . ∀ ψ :: * => Presence . α -> (Unit -> α3) -> α1 -> { returnSelf : ψ α2 -> α3 }"
      "β -> (Unit -> β3) -> β1 -> { returnSelf : β4 β2 -> β3 }"
      []
  ]

regTreeEqTests :: TestTree
regTreeEqTests = testGroup
  "Regular Tree Equal"
  [ testCase "regTreeEq Identical recursive type"
  $            pType "\x3bc X . X -> Y"
  `regTreeEq'` pType "\x3bc X . X -> Y"
  @?=          True
  , testCase "regTreeEq Only bounded variable changed"
  $            pType "\x3bc X . X -> Y"
  `regTreeEq'` pType "\x3bc Z . Z -> Y"
  @?=          True
  , testCase "regTreeEq Not bound"
  $            pType "\x3bc X . Y -> Z"
  `regTreeEq'` pType "\x3bc W . Y -> Z"
  @?=          True
  , testCase "regTreeEq Unit, Bool, Nat, variable and arrow"
  $            pType "Unit -> Nat -> Bool -> X"
  `regTreeEq'` pType "Unit -> Nat -> Bool -> X"
  @?=          True
  , testCase "regTreeEq Variable not same"
  $            pType "X -> Z"
  `regTreeEq'` pType "Y -> Z"
  @?=          False
  , testCase "regTreeEq Empty row"
  $            pType "{ } -> [ | R ]"
  `regTreeEq'` pType "{ } -> [ | R ]"
  @?=          True
  , testCase "regTreeEq All absent row"
  $            pType "{ l1 : Absent } -> [ `l2 : Present Unit ]"
  `regTreeEq'` pType "{ l1 : Absent } -> [ `l2 : Present Unit ]"
  @?=          True
  , testCase "regTreeEq regTreeEq Label mismatch"
  $            pType "{ l2 : Absent }"
  `regTreeEq'` pType "{ l1 : Absent }"
  @?=          False
  , testCase "regTreeEq Row variable mismatch"
  $            pType "{ l1 : Absent | R1 }"
  `regTreeEq'` pType "{ l1 : Absent | R2}"
  @?=          False
  , testCase "regTreeEq Presence variable with type mismatch"
  $            pType "{ l1 : A Unit | R }"
  `regTreeEq'` pType "{ l1 : B Unit | R}"
  @?=          False
  , testCase "regTreeEq Presence variable mismatch"
  $            pType "{ l1 : A | R }"
  `regTreeEq'` pType "{ l1 : B | R}"
  @?=          False
  , testCase "regTreeEq Label number mismatch"
  $            pType "{ l1 : A | R }"
  `regTreeEq'` pType "{ l1 : A, l2 : B | R}"
  @?=          False
  , testCase "regTreeEq Label number mismatch"
  $            pType "{ l1 : A | R }"
  `regTreeEq'` pType "{ l1 : A, l2 : B | R}"
  @?=          False
  , testCase "regTreeEq Ref 1"
  $            pType "Ref Unit"
  `regTreeEq'` pType "Ref Unit"
  @?=          True
  , testCase "regTreeEq Ref 2"
  $            pType "Ref X"
  `regTreeEq'` pType "Ref Y"
  @?=          False
  , testCase "regTreeEq Recursive variant"
  $            pType
                 "\x3bc List . [ `cons : Present { head : Present X, tail : Present List }, `nil : Present Unit ]"
  `regTreeEq'` pType
                 "[ `cons : Present { head : Present X, tail : Present \x3bc List . [ `cons : Present { head : Present X, tail : Present List }, `nil : Present Unit ] }, `nil : Present Unit ]"
  @?=          True
  ]
