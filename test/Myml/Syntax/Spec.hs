module Myml.Syntax.Spec
  ( tests
  )
where

import           Myml.Syntax
import           Myml.Test.Helper
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.SmallCheck         as SC
import qualified Data.Set                      as Set

tests :: TestTree
tests = testGroup "Myml.Syntax.Spec" [scProps, unitTests]

scProps :: TestTree
scProps = testGroup
  "SmallCheck Properties"
  [ localOption (SmallCheckDepth 1)
      $ SC.testProperty "isNatValue(x) -> isValue(x)" propNatValueIsValue
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [freeVariableTests, isValueTests]

propNatValueIsValue :: Term -> Bool
propNatValueIsValue t = not (isNatValue t) || isValue t

freeVariableTests :: TestTree
freeVariableTests =
  testGroup "freeVariable tests" [freeVariableTermTests, freeVariableTypeTests]

freeVariableTermTests :: TestTree
freeVariableTermTests = testGroup
  "freeVariable term tests"
  [ testCase "freeVariable simple variable"
  $   freeVariable (pTerm "x")
  @?= Set.fromList ["x"]
  , testCase "freeVariable bind 1"
  $   freeVariable (pTerm "\x3bb x . x x")
  @?= Set.empty
  , testCase "freeVariable bind 2"
  $   freeVariable (pTerm "\x3bb x . x y")
  @?= Set.fromList ["y"]
  , testCase "freeVariable bind 3"
  $   freeVariable (pTerm "\x3bb x . x y (\x3bb y . x z)")
  @?= Set.fromList ["y", "z"]
  , testCase "freeVariable let 1"
  $   freeVariable (pTerm "let x = y in z")
  @?= Set.fromList ["y", "z"]
  , testCase "freeVariable let 2"
  $   freeVariable (pTerm "let x = x in z")
  @?= Set.fromList ["x", "z"]
  , testCase "freeVariable record"
  $   freeVariable (pTerm "{ x = x, y = y }")
  @?= Set.fromList ["x", "y"]
  , testCase "freeVariable record extend"
  $   freeVariable (pTerm "{ x = x, y = y } with { z = z }")
  @?= Set.fromList ["x", "y", "z"]
  , testCase "freeVariable record access"
  $   freeVariable (pTerm "{ x = x, y = y }.x")
  @?= Set.fromList ["x", "y"]
  , testCase "freeVariable match"
  $   freeVariable (pTerm "[`l1 x -> x y, `l2 y -> z y]")
  @?= Set.fromList ["y", "z"]
  , testCase "freeVariable match extend"
  $   freeVariable (pTerm "[`l1 x -> x y] with [`l2 y -> z y]")
  @?= Set.fromList ["y", "z"]
  , testCase "freeVariable variant"
  $   freeVariable (pTerm "`x x")
  @?= Set.fromList ["x"]
  , testCase "freeVariable ref"
  $   freeVariable (pTerm "ref (x y)")
  @?= Set.fromList ["x", "y"]
  , testCase "freeVariable deref"
  $   freeVariable (pTerm "!(x y)")
  @?= Set.fromList ["x", "y"]
  , testCase "freeVariable assign"
  $   freeVariable (pTerm "x := y")
  @?= Set.fromList ["x", "y"]
  , testCase "freeVariable location" $ freeVariable (TmLoc 0) @?= Set.empty
  , testCase "freeVariable unit" $ freeVariable (pTerm "unit") @?= Set.empty
  , testCase "freeVariable true and false"
  $   freeVariable (pTerm "true false")
  @?= Set.empty
  , testCase "freeVariable if"
  $   freeVariable (pTerm "if x then y else z")
  @?= Set.fromList ["x", "y", "z"]
  , testCase "freeVariable zero" $ freeVariable (pTerm "zero") @?= Set.empty
  , testCase "freeVariable succ"
  $   freeVariable (pTerm "succ x")
  @?= Set.fromList ["x"]
  ]

freeVariableTypeTests :: TestTree
freeVariableTypeTests = testGroup
  "freeVariable type tests"
  [ testCase "freeVariable variable" $ freeVariable (pType "X") @?= Set.fromList
    ["X"]
  , testCase "freeVariable arrow"
  $   freeVariable (pType "X -> Y -> X")
  @?= Set.fromList ["X", "Y"]
  , testCase "freeVariable record"
  $   freeVariable (pType "{ l1 : P Unit, l2 : Absent, l3 : Present X | R }")
  @?= Set.fromList ["P", "X", "R"]
  , testCase "freeVariable record"
  $   freeVariable (pType "[ `l1 : P Unit, `l2 : Absent, `l3 : Present X | R ]")
  @?= Set.fromList ["P", "X", "R"]
  , testCase "freeVariable mu"
  $   freeVariable (pType "\x3bc X . (X -> T)")
  @?= Set.fromList ["T"]
  , testCase "freeVariable Unit" $ freeVariable (pType "Unit") @?= Set.empty
  , testCase "freeVariable Bool" $ freeVariable (pType "Bool") @?= Set.empty
  , testCase "freeVariable Nat" $ freeVariable (pType "Nat") @?= Set.empty
  ]

isValueTests :: TestTree
isValueTests = testGroup
  "isValue tests"
  [ testCase "isValue abstraction" $ isValue (pTerm "\x3bb x . x") @?= True
  , testCase "isValue application" $ isValue (pTerm "x x") @?= False
  , testCase "isValue variable" $ isValue (pTerm "x") @?= False
  , testCase "isValue let" $ isValue (pTerm "let x = unit in x") @?= False
  , testCase "isValue record 1" $ isValue (pTerm "{ l = x }") @?= False
  , testCase "isValue record 2"
  $   isValue (pTerm "{ l1 = x, l2 = unit }")
  @?= False
  , testCase "isValue record 3"
  $   isValue (pTerm "{ l1 = unit, l2 = unit }")
  @?= True
  , testCase "isValue record extend"
  $   isValue (pTerm "{ l1 = x } with { l2 = y }")
  @?= False
  , testCase "isValue record access" $ isValue (pTerm "x.x") @?= False
  , testCase "isValue match" $ isValue (pTerm "[`l x -> x]") @?= True
  , testCase "isValue match extend "
  $   isValue (pTerm "[`l1 x -> x] with [`l2 x -> x]")
  @?= False
  , testCase "isValue variant 1" $ isValue (pTerm "`l1 x") @?= False
  , testCase "isValue variant 2" $ isValue (pTerm "`l1 (\x3bb x . x)") @?= True
  , testCase "isValue ref" $ isValue (pTerm "ref unit") @?= False
  , testCase "isValue deref" $ isValue (pTerm "!unit") @?= False
  , testCase "isValue assign" $ isValue (pTerm "unit := unit") @?= False
  , testCase "isValue location" $ isValue (TmLoc 0) @?= True
  , testCase "isValue unit" $ isValue (pTerm "unit") @?= True
  , testCase "isValue true" $ isValue (pTerm "true") @?= True
  , testCase "isValue false" $ isValue (pTerm "false") @?= True
  , testCase "isValue if"
  $   isValue (pTerm "if unit then unit else unit")
  @?= False
  , testCase "isValue zero" $ isValue (pTerm "zero") @?= True
  , testCase "isValue succ 1" $ isValue (pTerm "succ unit") @?= False
  , testCase "isValue succ 2" $ isValue (pTerm "succ zero") @?= True
  , testCase "isValue succ 3" $ isValue (pTerm "succ (succ zero)") @?= True
  ]
