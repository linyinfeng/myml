module Myml.Eval.Spec
  ( tests
  )
where

import           Myml.Syntax
import           Myml.Eval.Store
import           Myml.Eval
import           Myml.Test.Helper
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Data.Map                      as Map

tests :: TestTree
tests = testGroup "Myml.Eval.Spec" [unitTests]

unitTests :: TestTree
unitTests = testGroup
  "Unit tests"
  [ testCase "bigStep ref 1"
  $   runEvalState (bigStep (pTerm "ref unit")) emptyStore
  @?= ( Left (ExcNoRuleApplied (TmLoc 0))
      , Store { storeData    = Map.fromList [(0, WithMark False TmUnit)]
              , storeMinFree = 1
              }
      )
  , testCase "bigStep ref 2"
  $   runEvalState (bigStep (pTerm "ref (ref unit)")) emptyStore
  @?= ( Left (ExcNoRuleApplied (TmLoc 1))
      , Store
        { storeData    = Map.fromList
          [(0, WithMark False TmUnit), (1, WithMark False (TmLoc 0))]
        , storeMinFree = 2
        }
      )
  ]
  -- TODO more test for bigStep
