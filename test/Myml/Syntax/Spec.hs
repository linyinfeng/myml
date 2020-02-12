module Myml.Syntax.Spec
  ( tests
  )
where

import           Myml.Syntax
import           Myml.Parser
import           Text.Trifecta
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.SmallCheck         as SC
import qualified Data.Map                      as Map
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

term :: String -> Term
term s = case parseString parseTerm mempty s of
  Success t -> t
  Failure e -> error (show e)

freeVariableTests :: TestTree
freeVariableTests = testGroup
  "freeVariable tests"
  [ testCase "freeVariable simple variable"
  $   freeVariable (term "x")
  @?= Set.fromList ["x"]
  , testCase "freeVariable bind 1"
  $   freeVariable (term "\x3bbx. x x")
  @?= Set.empty
  , testCase "freeVariable bind 2"
  $   freeVariable (term "\x3bbx. x y")
  @?= Set.fromList ["y"]
  , testCase "freeVariable bind 3"
  $   freeVariable (term "\x3bbx. x y (\x3bby. x z)")
  @?= Set.fromList ["y", "z"]
  , testCase "freeVariable let 1"
  $   freeVariable (term "let x = y in z")
  @?= Set.fromList ["y", "z"]
  , testCase "freeVariable let 2"
  $   freeVariable (term "let x = x in z")
  @?= Set.fromList ["x", "z"]
  , testCase "freeVariable record"
  $   freeVariable (term "{ x = x, y = y }")
  @?= Set.fromList ["x", "y"]
  , testCase "freeVariable record extend"
  $   freeVariable (term "{ x = x, y = y } with { z = z }")
  @?= Set.fromList ["x", "y", "z"]
  , testCase "freeVariable record access"
  $   freeVariable (term "{ x = x, y = y }.x")
  @?= Set.fromList ["x", "y"]
  , testCase "freeVariable match"
  $   freeVariable (term "[`l1 x -> x y, `l2 y -> z y]")
  @?= Set.fromList ["y", "z"]
  , testCase "freeVariable match extend"
  $   freeVariable (term "[`l1 x -> x y] with [`l2 y -> z y]")
  @?= Set.fromList ["y", "z"]
  , testCase "freeVariable variant"
  $   freeVariable (term "`x x")
  @?= Set.fromList ["x"]
  , testCase "freeVariable ref"
  $   freeVariable (term "ref (x y)")
  @?= Set.fromList ["x", "y"]
  , testCase "freeVariable deref"
  $   freeVariable (term "!(x y)")
  @?= Set.fromList ["x", "y"]
  , testCase "freeVariable assign"
  $   freeVariable (term "x := y")
  @?= Set.fromList ["x", "y"]
  , testCase "freeVariable location" $ freeVariable (TmLoc 0) @?= Set.empty
  , testCase "freeVariable unit" $ freeVariable (term "unit") @?= Set.empty
  , testCase "freeVariable true and false"
  $   freeVariable (term "true false")
  @?= Set.empty
  , testCase "freeVariable if"
  $   freeVariable (term "if x then y else z")
  @?= Set.fromList ["x", "y", "z"]
  , testCase "freeVariable zero" $ freeVariable (term "zero") @?= Set.empty
  , testCase "freeVariable succ" $ freeVariable (term "succ x") @?= Set.fromList
    ["x"]
  ]

isValueTests :: TestTree
isValueTests = testGroup
  "isValue tests"
  [ testCase "isValue abstraction" $ isValue (term "\x3bbx. x") @?= True
  , testCase "isValue application" $ isValue (term "x x") @?= False
  , testCase "isValue variable" $ isValue (term "x") @?= False
  , testCase "isValue let" $ isValue (term "let x = unit in x") @?= False
  , testCase "isValue record 1" $ isValue (term "{ l = x }") @?= False
  , testCase "isValue record 2"
  $   isValue (term "{ l1 = x, l2 = unit }")
  @?= False
  , testCase "isValue record 3"
  $   isValue (term "{ l1 = unit, l2 = unit }")
  @?= True
  , testCase "isValue record extend"
  $   isValue (term "{ l1 = x } with { l2 = y }")
  @?= False
  , testCase "isValue record access" $ isValue (term "x.x") @?= False
  , testCase "isValue match" $ isValue (term "[`l x -> x]") @?= True
  , testCase "isValue match extend "
  $   isValue (term "[`l1 x -> x] with [`l2 x -> x]")
  @?= False
  , testCase "isValue variant 1" $ isValue (term "`l1 x") @?= False
  , testCase "isValue variant 2" $ isValue (term "`l1 (\x3bbx. x)") @?= True
  , testCase "isValue ref" $ isValue (term "ref unit") @?= False
  , testCase "isValue deref" $ isValue (term "!unit") @?= False
  , testCase "isValue assign" $ isValue (term "unit := unit") @?= False
  , testCase "isValue location" $ isValue (TmLoc 0) @?= True
  , testCase "isValue unit" $ isValue (term "unit") @?= True
  , testCase "isValue true" $ isValue (term "true") @?= True
  , testCase "isValue false" $ isValue (term "false") @?= True
  , testCase "isValue if"
  $   isValue (term "if unit then unit else unit")
  @?= False
  , testCase "isValue zero" $ isValue (term "zero") @?= True
  , testCase "isValue succ 1" $ isValue (term "succ unit") @?= False
  , testCase "isValue succ 2" $ isValue (term "succ zero") @?= True
  , testCase "isValue succ 3" $ isValue (term "succ (succ zero)") @?= True
  ]
