module Myml.Typing.Spec
  ( tests
  )
where

import           Myml.Typing
import           Myml.Test.Helper
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Myml.Typing.Spec" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [regTreeEqTests]

regTreeEqTests :: TestTree
regTreeEqTests = testGroup
  "Regular Tree Equal"
  [ testCase "Identical recursive type"
  $            pType "\x3bc X . X -> Y"
  `regTreeEq'` pType "\x3bc X . X -> Y"
  @?=          True
  , testCase "Only bounded variable changed"
  $            pType "\x3bc X . X -> Y"
  `regTreeEq'` pType "\x3bc Z . Z -> Y"
  @?=          True
  , testCase "Not bound"
  $            pType "\x3bc X . Y -> Z"
  `regTreeEq'` pType "\x3bc W . Y -> Z"
  @?=          True
  , testCase "Unit, Bool, Nat, variable and arrow"
  $            pType "Unit -> Nat -> Bool -> X"
  `regTreeEq'` pType "Unit -> Nat -> Bool -> X"
  @?=          True
  , testCase "Variable not same"
  $            pType "X -> Z"
  `regTreeEq'` pType "Y -> Z"
  @?=          False
  , testCase "Empty row"
  $            pType "{ } -> [ | R ]"
  `regTreeEq'` pType "{ } -> [ | R ]"
  @?=          True
  , testCase "All absent row"
  $            pType "{ l1 : Absent } -> [ `l2 : Present Unit ]"
  `regTreeEq'` pType "{ l1 : Absent } -> [ `l2 : Present Unit ]"
  @?=          True
  , testCase "Label mismatch"
  $            pType "{ l2 : Absent }"
  `regTreeEq'` pType "{ l1 : Absent }"
  @?=          False
  , testCase "Row variable mismatch"
  $            pType "{ l1 : Absent | R1 }"
  `regTreeEq'` pType "{ l1 : Absent | R2}"
  @?=          False
  , testCase "Presence variable with type mismatch"
  $            pType "{ l1 : A Unit | R }"
  `regTreeEq'` pType "{ l1 : B Unit | R}"
  @?=          False
  , testCase "Presence variable mismatch"
  $            pType "{ l1 : A | R }"
  `regTreeEq'` pType "{ l1 : B | R}"
  @?=          False
  , testCase "Label number mismatch"
  $            pType "{ l1 : A | R }"
  `regTreeEq'` pType "{ l1 : A, l2 : B | R}"
  @?=          False
  , testCase "Label number mismatch"
  $            pType "{ l1 : A | R }"
  `regTreeEq'` pType "{ l1 : A, l2 : B | R}"
  @?=          False
  , testCase "Ref 1" $ pType "Ref Unit" `regTreeEq'` pType "Ref Unit" @?= True
  , testCase "Ref 2" $ pType "Ref X" `regTreeEq'` pType "Ref Y" @?= False
  , testCase "Recursive variant"
  $            pType
                 "\x3bc List . [ `cons : Present { head : Present X, tail : Present List }, `nil : Present Unit ]"
  `regTreeEq'` pType
                 "[ `cons : Present { head : Present X, tail : Present \x3bc List . [ `cons : Present { head : Present X, tail : Present List }, `nil : Present Unit ] }, `nil : Present Unit ]"
  @?=          True
  ]
