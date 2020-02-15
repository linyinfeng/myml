module Myml.Subst.Spec
  ( tests
  )
where

import           Myml.Subst
import           Myml.Syntax
import           Myml.Test.Helper
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.SmallCheck         as SC
import qualified Data.Map                      as Map

tests :: TestTree
tests = testGroup "Myml.Subst.Spec" [scProps, unitTests]

scProps :: TestTree
scProps = testGroup
  "SmallCheck properties"
  [ localOption (SmallCheckDepth 1)
      $ SC.testProperty "[] term = term" propEmptySubstTerm
  ]

propEmptySubstTerm :: Term -> Bool
propEmptySubstTerm t = applySubst (Map.empty :: Subst Term) t == t

unitTests :: TestTree
unitTests = testGroup "Unit tests" [compsiteTests, substTests]

compsiteTests :: TestTree
compsiteTests = testGroup
  "Composite tests"
  [ testCase "Subst composite empty"
  $   (Map.empty `compositeSubst` Map.empty :: Subst Term)
  @?= Map.empty
  , testCase "Subst composite singleton left"
  $   (Map.singleton "x" (TmVar "y") `compositeSubst` Map.empty)
  @?= Map.singleton "x" (TmVar "y")
  , testCase "Subst composite singleton right"
  $   (Map.empty `compositeSubst` Map.singleton "x" (TmVar "y"))
  @?= Map.singleton "x" (TmVar "y")
  , testCase "Subst composite 1"
  $   (                Map.fromList [("y", TmVar "z")]
      `compositeSubst` Map.fromList [("x", TmVar "y")]
      )
  @?= Map.fromList [("x", TmVar "z"), ("y", TmVar "z")]
  , testCase "Subst composite 2"
  $   (                Map.fromList [("y", TmVar "z")]
      `compositeSubst` Map.fromList [("x", TmVar "y"), ("y", TmVar "w")]
      )
  @?= Map.fromList [("x", TmVar "z"), ("y", TmVar "w")]
  ]

substTests :: TestTree
substTests = testGroup "Substitution tests" [termSubstTests, typeSubstTests]

termSubstTests :: TestTree
termSubstTests = testGroup
  "Term substitution tests"
  [ testCase "Term substitution variable"
  $            Map.fromList [("x", pTerm "y")]
  `applySubst` pTerm "x"
  @?=          pTerm "y"
  , testCase "Term substitution abstraction 1"
  $            Map.fromList [("x", pTerm "y")]
  `applySubst` pTerm "\x3bb x . x"
  @?=          pTerm "\x3bb x . x"
  , testCase "Term substitution abstraction 2"
  $            Map.fromList [("x", pTerm "y")]
  `applySubst` pTerm "\x3bb z . x"
  @?=          pTerm "\x3bb z . y"
  , testCase "Term substitution abstraction 3"
  $            Map.fromList [("x", pTerm "y")]
  `applySubst` pTerm "\x3bb y . x"
  @?=          pTerm "\x3bb y0 . y"
  , testCase "Term substitution application"
  $            Map.fromList [("x", pTerm "y"), ("y", pTerm "z")]
  `applySubst` pTerm "x y"
  @?=          pTerm "y z"
  , testCase "Term substitution let"
  $            Map.fromList [("x", pTerm "y")]
  `applySubst` pTerm "let y = x in x"
  @?=          pTerm "let y0 = y in y"
  , testCase "Term substitution record"
  $            Map.fromList [("x", pTerm "y")]
  `applySubst` pTerm "{ l1 = x, l2 = x }"
  @?=          pTerm "{ l1 = y, l2 = y }"
  , testCase "Term substitution record extend"
  $            Map.fromList [("x", pTerm "y")]
  `applySubst` pTerm "{ l1 = x, l2 = x } with { l3 = x }"
  @?=          pTerm "{ l1 = y, l2 = y } with { l3 = y }"
  , testCase "Term substitution record access"
  $            Map.fromList [("x", pTerm "y")]
  `applySubst` pTerm "{ l1 = x, l2 = x }.l1"
  @?=          pTerm "{ l1 = y, l2 = y }.l1"
  , testCase "Term substitution match"
  $            Map.fromList [("x", pTerm "y")]
  `applySubst` pTerm "[ `l y -> x ]"
  @?=          pTerm "[ `l y0 -> y ]"
  , testCase "Term substitution match extend"
  $            Map.fromList [("x", pTerm "y")]
  `applySubst` pTerm "[ `l1 y -> x ] with [`l2 y -> x ]"
  @?=          pTerm "[ `l1 y0 -> y ] with [`l2 y0 -> y ]"
  , testCase "Term substitution variant"
  $            Map.fromList [("x", pTerm "y")]
  `applySubst` pTerm "`l x"
  @?=          pTerm "`l y"
  , testCase "Term substitution ref"
  $            Map.fromList [("x", pTerm "y")]
  `applySubst` pTerm "ref x"
  @?=          pTerm "ref y"
  , testCase "Term substitution deref"
  $            Map.fromList [("x", pTerm "y")]
  `applySubst` pTerm "! x"
  @?=          pTerm "! y"
  , testCase "Term substitution assign"
  $            Map.fromList [("x", pTerm "y")]
  `applySubst` pTerm "x := y"
  @?=          pTerm "y := y"
  , testCase "Term substitution location"
  $            Map.fromList [("x", pTerm "y")]
  `applySubst` TmLoc 0
  @?=          TmLoc 0
  , testCase "Term substitution unit"
  $            Map.fromList [("x", pTerm "y")]
  `applySubst` pTerm "unit"
  @?=          pTerm "unit"
  , testCase "Term substitution seq"
  $            Map.fromList [("x", pTerm "y")]
  `applySubst` pTerm "x; y"
  @?=          pTerm "y; y"
  , testCase "Term substitution true"
  $            Map.fromList [("x", pTerm "y")]
  `applySubst` pTerm "true"
  @?=          pTerm "true"
  , testCase "Term substitution false"
  $            Map.fromList [("x", pTerm "y")]
  `applySubst` pTerm "false"
  @?=          pTerm "false"
  , testCase "Term substitution if"
  $ Map.fromList [("x", pTerm "y"), ("y", pTerm "z"), ("z", pTerm "x")]
  `applySubst` pTerm "if x then y else z"
  @?= pTerm "if y then z else x"
  , testCase "Term substitution zero"
  $            Map.fromList [("x", pTerm "y")]
  `applySubst` pTerm "zero"
  @?=          pTerm "zero"
  , testCase "Term substitution succ"
  $            Map.fromList [("x", pTerm "y")]
  `applySubst` pTerm "succ x"
  @?=          pTerm "succ y"
  ]

typeSubstTests :: TestTree
typeSubstTests = testGroup
  "Type substitution tests"
  [ testCase "Type substitution proper variable"
  $            Map.fromList [("X", TySubProper (pType "Y"))]
  `applySubst` pType "X"
  @?=          pType "Y"
  , testCase "Type substitution proper arrow"
  $            Map.fromList
                 [("X", TySubProper (pType "Y")), ("Y", TySubProper (pType "Z"))]
  `applySubst` pType "X -> Y"
  @?=          pType "Y -> Z"
  , testCase "Type substitution proper record"
  $            Map.fromList
                 [("X", TySubProper (pType "Y")), ("Y", TySubProper (pType "Z"))]
  `applySubst` pType "{ l1 : Absent, l2 : Present X, l3 : P Y | R }"
  @?=          pType "{ l1 : Absent, l2 : Present Y, l3 : P Z | R }"
  , testCase "Type substitution proper variant"
  $            Map.fromList
                 [("X", TySubProper (pType "Y")), ("Y", TySubProper (pType "Z"))]
  `applySubst` pType "[ `l1 : Absent, `l2 : Present X, `l3 : P Y | R ]"
  @?=          pType "[ `l1 : Absent, `l2 : Present Y, `l3 : P Z | R ]"
  , testCase "Type substitution proper mu"
  $            Map.fromList [("Y", TySubProper (pType "X"))]
  `applySubst` pType "\x3bc X . X -> Y"
  @?=          pType "\x3bc X0 . X0 -> X"
  , testCase "Type substitution proper reference"
  $            Map.fromList [("X", TySubProper (pType "Y"))]
  `applySubst` pType "Ref X"
  @?=          pType "Ref Y"
  , testCase "Type substitution proper Unit"
  $            Map.fromList [("X", TySubProper (pType "Y"))]
  `applySubst` pType "Unit"
  @?=          pType "Unit"
  , testCase "Type substitution proper Bool"
  $            Map.fromList [("X", TySubProper (pType "Y"))]
  `applySubst` pType "Bool"
  @?=          pType "Bool"
  , testCase "Type substitution proper Nat"
  $            Map.fromList [("X", TySubProper (pType "Y"))]
  `applySubst` pType "Nat"
  @?=          pType "Nat"
  , testCase "Type substitution presence absent 1"
  $            Map.fromList [("P", TySubPresence PresenceInstAbsent)]
  `applySubst` pType "{ l : P Unit } -> [ `l : P Unit ]"
  @?=          pType "{ } -> [ ]"
  , testCase "Type substitution presence absent 2"
  $            Map.fromList [("P", TySubPresence PresenceInstAbsent)]
  `applySubst` pType "{ l : P Unit | R1 } -> [ `l : P Unit | R2 ]"
  @?=          pType "{ l : Absent | R1 } -> [ `l : Absent | R2 ]"
  , testCase "Type substitution presence present"
  $            Map.fromList [("P", TySubPresence PresenceInstPresent)]
  `applySubst` pType "{ l : P Unit | R1 } -> [ `l : P Unit | R2 ]"
  @?=          pType "{ l : Present Unit | R1 } -> [ `l : Present Unit | R2 ]"
  , testCase "Type substitution presence varaible"
  $            Map.fromList [("P", TySubPresence (PresenceInstVar "P'"))]
  `applySubst` pType "{ l : P Unit | R1 } -> [ `l : P Unit | R2 ]"
  @?=          pType "{ l : P' Unit | R1 } -> [ `l : P' Unit | R2 ]"
  , testCase "Type substitution row 1"
  $            Map.fromList [("R", TySubRow (pTypeRow "l2 : Absent"))]
  `applySubst` pType "{ l : P Unit | R } -> [ `l : P Unit | R ]"
  @?=          pType "{ l : P Unit } -> [ `l : P Unit ]"
  , testCase "Type substitution row 2"
  $            Map.fromList [("R", TySubRow (pTypeRow "l2 : Absent | R'"))]
  `applySubst` pType "{ l : P Unit | R } -> [ `l : P Unit | R ]"
  @?=          pType
                 "{ l : P Unit, l2 : Absent | R' } -> [ `l : P Unit, `l2 : Absent | R' ]"
  , testCase "Type substitution row 3"
  $            Map.fromList [("R", TySubRow (pTypeRow "| R'"))]
  `applySubst` pType "{ l : P Unit | R } -> [ `l : P Unit | R ]"
  @?=          pType "{ l : P Unit | R' } -> [ `l : P Unit | R' ]"
  , testCase "Type substitution row and presence"
  $            Map.fromList
                 [ ("P", TySubPresence PresenceInstPresent)
                 , ("R", TySubRow (pTypeRow "l2 : P Nat | R'"))
                 ]
  `applySubst` pType "{ l : P Unit | R } -> [ `l : P Unit | R ]"
  @?=          pType
                 "{ l : Present Unit, l2 : P Nat | R' } -> [ `l : Present Unit, `l2 : P Nat | R' ]"
  ]
