module Myml.Subst.Spec
  ( tests
  )
where

import           Myml.Subst
import           Myml.Syntax
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.SmallCheck         as SC
import qualified Data.Map                      as Map

tests :: TestTree
tests = testGroup "Myml.Subst.Spec" [scProps, unitTests]

scProps :: TestTree
scProps = testGroup
  "SmallCheck Properties"
  [ localOption (SmallCheckDepth 1)
      $ SC.testProperty "[] term = term" propEmptySubstTerm
  ]

propEmptySubstTerm :: Term -> Bool
propEmptySubstTerm t = applySubst (Map.empty :: Subst Term) t == t

unitTests :: TestTree
unitTests = testGroup
  "Unit tests"
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
  , testCase "term substitution 1"
  $            Map.fromList [("x", TmVar "y")]
  `applySubst` TmVar "x"
  @?=          TmVar "y"
  , testCase "term substitution 2"
  $            Map.fromList [("x", TmVar "y")]
  `applySubst` TmAbs "x" (TmVar "x")
  @?=          TmAbs "x" (TmVar "x")
  , testCase "term substitution 3"
  $            Map.fromList [("x", TmVar "y")]
  `applySubst` TmAbs "z" (TmVar "x")
  @?=          TmAbs "z" (TmVar "y")
  , testCase "term substitution 4"
  $            Map.fromList [("x", TmVar "y")]
  `applySubst` TmAbs "y"  (TmVar "x")
  @?=          TmAbs "y0" (TmVar "y")
  , testCase "term substitution 5"
  $            Map.fromList [("x", TmVar "y")]
  `applySubst` TmLet "y"  (TmVar "x") (TmVar "x")
  @?=          TmLet "y0" (TmVar "y") (TmVar "y")
  , testCase "term substitution 6"
  $            Map.fromList [("x", TmVar "y")]
  `applySubst` TmMatch (Map.singleton "l" (TmCase "y" (TmVar "x")))
  @?=          TmMatch (Map.singleton "l" (TmCase "y0" (TmVar "y")))
  ]
