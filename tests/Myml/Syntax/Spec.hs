module Myml.Syntax.Spec
  ( tests
  )
where

import           Myml.Syntax
import           Myml.Test.Helper
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

tests :: TestTree
tests = testGroup "Myml.Syntax.Spec" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [fvTests, isValueTests]

fvTests :: TestTree
fvTests = testGroup "freeVariable tests" [fvTermTests, fvTypeTests]

fvTermTests :: TestTree
fvTermTests = testGroup
  "fvTerm tests"
  [ testCase "freeVariable simple variable"
  $   fvTerm (pTerm "x")
  @?= Set.fromList ["x"]
  , testCase "freeVariable bind 1"
  $   fvTerm (pTerm "\x3bb x . x x")
  @?= Set.empty
  , testCase "fvTerm bind 2" $ fvTerm (pTerm "\x3bb x . x y") @?= Set.fromList
    ["y"]
  , testCase "fvTerm bind 3"
  $   fvTerm (pTerm "\x3bb x . x y (\x3bb y . x z)")
  @?= Set.fromList ["y", "z"]
  , testCase "fvTerm let 1" $ fvTerm (pTerm "let x = y in z") @?= Set.fromList
    ["y", "z"]
  , testCase "fvTerm let 2" $ fvTerm (pTerm "let x = x in z") @?= Set.fromList
    ["x", "z"]
  , testCase "fvTerm record"
  $   fvTerm (pTerm "{ x = x, y = y }")
  @?= Set.fromList ["x", "y"]
  , testCase "fvTerm record extend"
  $   fvTerm (pTerm "{ x = x, y = y } with { z = z }")
  @?= Set.fromList ["x", "y", "z"]
  , testCase "fvTerm record access"
  $   fvTerm (pTerm "{ x = x, y = y }.x")
  @?= Set.fromList ["x", "y"]
  , testCase "fvTerm match"
  $   fvTerm (pTerm "[`l1 x = x y, `l2 y = z y]")
  @?= Set.fromList ["y", "z"]
  , testCase "fvTerm match extend"
  $   fvTerm (pTerm "[`l1 x = x y] with [`l2 y = z y]")
  @?= Set.fromList ["y", "z"]
  , testCase "fvTerm variant" $ fvTerm (pTerm "`x x") @?= Set.fromList ["x"]
  , testCase "fvTerm ref" $ fvTerm (pTerm "ref (x y)") @?= Set.fromList
    ["x", "y"]
  , testCase "fvTerm deref" $ fvTerm (pTerm "!(x y)") @?= Set.fromList
    ["x", "y"]
  , testCase "fvTerm assign" $ fvTerm (pTerm "x := y") @?= Set.fromList
    ["x", "y"]
  , testCase "fvTerm location" $ fvTerm (TmLoc 0) @?= Set.empty
  , testCase "fvTerm unit" $ fvTerm (pTerm "unit") @?= Set.empty
  , testCase "fvTerm seq" $ fvTerm (pTerm "x; y") @?= Set.fromList ["x", "y"]
  , testCase "fvTerm true and false" $ fvTerm (pTerm "true false") @?= Set.empty
  , testCase "fvTerm if" $ fvTerm (pTerm "if x then y else z") @?= Set.fromList
    ["x", "y", "z"]
  , testCase "fvTerm 0" $ fvTerm (pTerm "0") @?= Set.empty
  , testCase "fvTerm succ" $ fvTerm (pTerm "succ x") @?= Set.fromList ["x"]
  ]

fvTypeTests :: TestTree
fvTypeTests = testGroup
  "freeVariable type tests"
  [ testCase "fvType variable" $ fvType (pType "X") @?= Right
    (Map.fromList [("X", KProper)])
  , testCase "fvType arrow" $ fvType (pType "X -> Y -> X") @?= Right
    (Map.fromList [("X", KProper), ("Y", KProper)])
  , testCase "fvType record"
  $   fvType (pType "{ l1 : P Unit, l2 : Absent, l3 : Present X, R }")
  @?= Right
        (Map.fromList [("P", KPresenceWithType), ("X", KProper), ("R", KRow)])
  , testCase "fvType record"
  $   fvType (pType "[ `l1 : P Unit, `l2 : Absent, `l3 : Present X, R ]")
  @?= Right
        (Map.fromList [("P", KPresenceWithType), ("X", KProper), ("R", KRow)])
  , testCase "fvType mu" $ fvType (pType "\x3bc X . (X -> T)") @?= Right
    (Map.fromList [("T", KProper)])
  , testCase "fvType Unit" $ fvType (pType "Unit") @?= Right Map.empty
  , testCase "fvType Bool" $ fvType (pType "Bool") @?= Right Map.empty
  , testCase "fvType Nat" $ fvType (pType "Nat") @?= Right Map.empty
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
  , testCase "isValue match" $ isValue (pTerm "[`l x = x]") @?= True
  , testCase "isValue match extend "
  $   isValue (pTerm "[`l1 x = x] with [`l2 x = x]")
  @?= True
  , testCase "isValue variant 1" $ isValue (pTerm "`l1 x") @?= False
  , testCase "isValue variant 2" $ isValue (pTerm "`l1 (\x3bb x . x)") @?= True
  , testCase "isValue ref" $ isValue (pTerm "ref unit") @?= False
  , testCase "isValue deref" $ isValue (pTerm "!unit") @?= False
  , testCase "isValue assign" $ isValue (pTerm "unit := unit") @?= False
  , testCase "isValue location" $ isValue (TmLoc 0) @?= True
  , testCase "isValue unit" $ isValue (pTerm "unit") @?= True
  , testCase "isValue seq" $ isValue (pTerm "unit; unit") @?= False
  , testCase "isValue true" $ isValue (pTerm "true") @?= True
  , testCase "isValue false" $ isValue (pTerm "false") @?= True
  , testCase "isValue if"
  $   isValue (pTerm "if unit then unit else unit")
  @?= False
  , testCase "isValue zero" $ isValue (pTerm "zero") @?= True
  , testCase "isValue succ" $ isValue (pTerm "succ zero") @?= False
  ]
