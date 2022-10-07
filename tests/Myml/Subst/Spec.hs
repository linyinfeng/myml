module Myml.Subst.Spec
  ( tests,
  )
where

import qualified Data.Map as Map
import Myml.Subst
import Myml.Syntax
import Myml.Test.Helper
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC

tests :: TestTree
tests = testGroup "Myml.Subst.Spec" [scProps, unitTests]

scProps :: TestTree
scProps =
  testGroup
    "SmallCheck properties"
    [ localOption (SmallCheckDepth 1) $
        SC.testProperty "[] term = term" propEmptySubstTerm
    ]

propEmptySubstTerm :: Term -> Bool
propEmptySubstTerm t = substTerm Map.empty t == t

unitTests :: TestTree
unitTests = testGroup "Unit tests" [compositeTests, substTests]

compositeTests :: TestTree
compositeTests =
  testGroup
    "Composite tests"
    [ testCase "Subst composite empty" $
        (Map.empty `compositeTermSubst` Map.empty)
          @?= Map.empty,
      testCase "Subst composite singleton left" $
        (Map.singleton "x" (TmVar "y") `compositeTermSubst` Map.empty)
          @?= Map.singleton "x" (TmVar "y"),
      testCase "Subst composite singleton right" $
        (Map.empty `compositeTermSubst` Map.singleton "x" (TmVar "y"))
          @?= Map.singleton "x" (TmVar "y"),
      testCase "Subst composite 1" $
        ( Map.fromList [("y", TmVar "z")]
            `compositeTermSubst` Map.fromList [("x", TmVar "y")]
        )
          @?= Map.fromList [("x", TmVar "z"), ("y", TmVar "z")],
      testCase "Subst composite 2" $
        ( Map.fromList [("y", TmVar "z")]
            `compositeTermSubst` Map.fromList [("x", TmVar "y"), ("y", TmVar "w")]
        )
          @?= Map.fromList [("x", TmVar "z"), ("y", TmVar "w")]
    ]

substTests :: TestTree
substTests = testGroup "Substitution tests" [termSubstTests, typeSubstTests]

termSubstTests :: TestTree
termSubstTests =
  testGroup
    "Term substitution tests"
    [ testCase "Term substitution variable" $
        Map.fromList [("x", pTerm "y")]
          `substTerm` pTerm "x"
          @?= pTerm "y",
      testCase "Term substitution abstraction 1" $
        Map.fromList [("x", pTerm "y")]
          `substTerm` pTerm "\x3bb x . x"
          @?= pTerm "\x3bb x . x",
      testCase "Term substitution abstraction 2" $
        Map.fromList [("x", pTerm "y")]
          `substTerm` pTerm "\x3bb z . x"
          @?= pTerm "\x3bb z . y",
      testCase "Term substitution abstraction 3" $
        Map.fromList [("x", pTerm "y")]
          `substTerm` pTerm "\x3bb y . x"
          @?= pTerm "\x3bb y1 . y",
      testCase "Term substitution application" $
        Map.fromList [("x", pTerm "y"), ("y", pTerm "z")]
          `substTerm` pTerm "x y"
          @?= pTerm "y z",
      testCase "Term substitution let" $
        Map.fromList [("x", pTerm "y")]
          `substTerm` pTerm "let y = x in x"
          @?= pTerm "let y1 = y in y",
      testCase "Term substitution record" $
        Map.fromList [("x", pTerm "y")]
          `substTerm` pTerm "{ l1 = x, l2 = x }"
          @?= pTerm "{ l1 = y, l2 = y }",
      testCase "Term substitution record extend" $
        Map.fromList [("x", pTerm "y")]
          `substTerm` pTerm "{ l1 = x, l2 = x } with { l3 = x }"
          @?= pTerm "{ l1 = y, l2 = y } with { l3 = y }",
      testCase "Term substitution record access" $
        Map.fromList [("x", pTerm "y")]
          `substTerm` pTerm "{ l1 = x, l2 = x }.l1"
          @?= pTerm "{ l1 = y, l2 = y }.l1",
      testCase "Term substitution match" $
        Map.fromList [("x", pTerm "y")]
          `substTerm` pTerm "[ `l y = x ]"
          @?= pTerm "[ `l y1 = y ]",
      testCase "Term substitution match extend" $
        Map.fromList [("x", pTerm "y")]
          `substTerm` pTerm "[ `l1 y = x ] with [`l2 y = x ]"
          @?= pTerm "[ `l1 y1 = y ] with [`l2 y1 = y ]",
      testCase "Term substitution variant" $
        Map.fromList [("x", pTerm "y")]
          `substTerm` pTerm "`l x"
          @?= pTerm "`l y",
      testCase "Term substitution ref" $
        Map.fromList [("x", pTerm "y")]
          `substTerm` pTerm "ref x"
          @?= pTerm "ref y",
      testCase "Term substitution deref" $
        Map.fromList [("x", pTerm "y")]
          `substTerm` pTerm "! x"
          @?= pTerm "! y",
      testCase "Term substitution assign" $
        Map.fromList [("x", pTerm "y")]
          `substTerm` pTerm "x := y"
          @?= pTerm "y := y",
      testCase "Term substitution location" $
        Map.fromList [("x", pTerm "y")]
          `substTerm` TmLoc 0
          @?= TmLoc 0,
      testCase "Term substitution unit" $
        Map.fromList [("x", pTerm "y")]
          `substTerm` pTerm "unit"
          @?= pTerm "unit",
      testCase "Term substitution seq" $
        Map.fromList [("x", pTerm "y")]
          `substTerm` pTerm "x; y"
          @?= pTerm "y; y",
      testCase "Term substitution true" $
        Map.fromList [("x", pTerm "y")]
          `substTerm` pTerm "true"
          @?= pTerm "true",
      testCase "Term substitution false" $
        Map.fromList [("x", pTerm "y")]
          `substTerm` pTerm "false"
          @?= pTerm "false",
      testCase "Term substitution if" $
        Map.fromList [("x", pTerm "y"), ("y", pTerm "z"), ("z", pTerm "x")]
          `substTerm` pTerm "if x then y else z"
          @?= pTerm "if y then z else x",
      testCase "Term substitution zero" $
        Map.fromList [("x", pTerm "y")]
          `substTerm` pTerm "zero"
          @?= pTerm "zero",
      testCase "Term substitution succ" $
        Map.fromList [("x", pTerm "y")]
          `substTerm` pTerm "succ x"
          @?= pTerm "succ y"
    ]

typeSubstTests :: TestTree
typeSubstTests =
  testGroup
    "Type substitution tests"
    [ testCase "Type substitution proper variable" $
        Map.fromList [("X", TySubProper (pType "Y"))]
          `substType` pType "X"
          @?= Right (pType "Y"),
      testCase "Type substitution proper arrow" $
        Map.fromList
          [("X", TySubProper (pType "Y")), ("Y", TySubProper (pType "Z"))]
          `substType` pType "X -> Y"
          @?= Right (pType "Y -> Z"),
      testCase "Type substitution proper record" $
        Map.fromList
          [("X", TySubProper (pType "Y")), ("Y", TySubProper (pType "Z"))]
          `substType` pType "{ l1 : Absent, l2 : Present X, l3 : P Y, R }"
          @?= Right (pType "{ l1 : Absent, l2 : Present Y, l3 : P Z, R }"),
      testCase "Type substitution proper variant" $
        Map.fromList
          [("X", TySubProper (pType "Y")), ("Y", TySubProper (pType "Z"))]
          `substType` pType "[ `l1 : Absent, `l2 : Present X, `l3 : P Y, R ]"
          @?= Right (pType "[ `l1 : Absent, `l2 : Present Y, `l3 : P Z, R ]"),
      testCase "Type substitution proper mu" $
        Map.fromList [("Y", TySubProper (pType "X"))]
          `substType` pType "\x3bc X . X -> Y"
          @?= Right (pType "\x3bc X1 . X1 -> X"),
      testCase "Type substitution proper reference" $
        Map.fromList [("X", TySubProper (pType "Y"))]
          `substType` pType "Ref X"
          @?= Right (pType "Ref Y"),
      testCase "Type substitution proper Unit" $
        Map.fromList [("X", TySubProper (pType "Y"))]
          `substType` pType "Unit"
          @?= Right (pType "Unit"),
      testCase "Type substitution proper Bool" $
        Map.fromList [("X", TySubProper (pType "Y"))]
          `substType` pType "Bool"
          @?= Right (pType "Bool"),
      testCase "Type substitution proper Nat" $
        Map.fromList [("X", TySubProper (pType "Y"))]
          `substType` pType "Nat"
          @?= Right (pType "Nat"),
      testCase "Type substitution presence with type absent 1"
      -- TODO maybe should be normalized
      $
        Map.fromList [("P", TySubPresenceWithType PresenceWithTypeAbsent)]
          `substType` pType "{ l : P Unit, · } -> [ `l : P Unit, · ]"
          @?= Right (pType "{ l : Absent, · } -> [ `l : Absent, · ]"),
      testCase "Type substitution presence with type absent 2" $
        Map.fromList [("P", TySubPresenceWithType PresenceWithTypeAbsent)]
          `substType` pType "{ l : P Unit, R1 } -> [ `l : P Unit, R2 ]"
          @?= Right (pType "{ l : Absent, R1 } -> [ `l : Absent, R2 ]"),
      testCase "Type substitution presence with type present" $
        Map.fromList [("P", TySubPresenceWithType PresenceWithTypePresent)]
          `substType` pType "{ l : P Unit, R1 } -> [ `l : P Unit, R2 ]"
          @?= Right (pType "{ l : Present Unit, R1 } -> [ `l : Present Unit, R2 ]"),
      testCase "Type substitution presence with type variable" $
        Map.fromList [("P", TySubPresenceWithType (PresenceWithTypeVar "P'"))]
          `substType` pType "{ l : P Unit, R1 } -> [ `l : P Unit, R2 ]"
          @?= Right (pType "{ l : P' Unit, R1 } -> [ `l : P' Unit, R2 ]"),
      testCase "Type substitution presence variable" $
        Map.fromList [("P", TySubPresence (PresenceVarWithType "P1" TyUnit))]
          `substType` pType "{ l : P, R1 } -> [ `l : P, R2 ]"
          @?= Right (pType "{ l : P1 Unit, R1 } -> [ `l : P1 Unit, R2 ]"),
      -- TODO maybe should be normalized
      testCase "Type substitution row 1" $
        Map.fromList [("R", TySubRow (pTypeRow "l2 : Absent, ·"))]
          `substType` pType "{ l : P Unit, R } -> [ `l : P Unit, R ]"
          @?= Right
            ( pType
                "{ l : P Unit, l2 : Absent, · } -> [ `l : P Unit, `l2 : Absent, · ]"
            ),
      testCase "Type substitution row 2" $
        Map.fromList [("R", TySubRow (pTypeRow "l2 : Absent, R'"))]
          `substType` pType "{ l : P Unit, R } -> [ `l : P Unit, R ]"
          @?= Right
            ( pType
                "{ l : P Unit, l2 : Absent, R' } -> [ `l : P Unit, `l2 : Absent, R' ]"
            ),
      testCase "Type substitution row 3" $
        Map.fromList [("R", TySubRow (pTypeRow "R'"))]
          `substType` pType "{ l : P Unit, R } -> [ `l : P Unit, R ]"
          @?= Right (pType "{ l : P Unit, R' } -> [ `l : P Unit, R' ]"),
      testCase "Type substitution row and presence" $
        Map.fromList
          [ ("P", TySubPresenceWithType PresenceWithTypePresent),
            ("R", TySubRow (pTypeRow "l2 : P Nat, R'"))
          ]
          `substType` pType "{ l : P Unit, R } -> [ `l : P Unit, R ]"
          @?= Right
            ( pType
                "{ l : Present Unit, l2 : P Nat, R' } -> [ `l : Present Unit, `l2 : P Nat, R' ]"
            )
    ]
