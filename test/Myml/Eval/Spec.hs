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
import           Control.Monad.State
import qualified Data.Map                      as Map

tests :: TestTree
tests = testGroup "Myml.Eval.Spec" [unitTests]

assertBigStep
  :: Term -> Store (WithMark Term) -> Term -> Store (WithMark Term) -> Assertion
assertBigStep t1 s1 t2 s2 = runState (bigStep t1) s1 @?= (t2, s2)

assertSmallStep
  :: Term
  -> Store (WithMark Term)
  -> Either EvalExcept Term
  -> Store (WithMark Term)
  -> Assertion
assertSmallStep t1 s1 t2 s2 = runSmallStepState (smallStep t1) s1 @?= (t2, s2)

unitTests :: TestTree
unitTests = testGroup
  "Unit tests"
  [ testCase "bigStep application of abstraction" $ assertBigStep
    (pTerm "(\x3bb x . x) (\x3bb x . x)")
    emptyStore
    (pTerm "(\x3bb x . x)")
    emptyStore
  , testCase "smallStep application order 1" $ assertSmallStep
    (pTerm
      "(if true then zero else (succ zero)) (if false then zero else (succ zero))"
    )
    emptyStore
    (Right (pTerm "zero (if false then zero else (succ zero))"))
    emptyStore
  , testCase "smallStep application order 2" $ assertSmallStep
    (pTerm "zero (if false then zero else (succ zero))")
    emptyStore
    (Right (pTerm "zero (succ zero)"))
    emptyStore
  , testCase "smallStep diverge" $ assertSmallStep
    (pTerm "(\x3bb x . x x) (\x3bb x . x x)")
    emptyStore
    (Right (pTerm "(\x3bb x . x x) (\x3bb x . x x)"))
    emptyStore
  , testCase "bigStep let" $ assertBigStep
    (pTerm "let id = \x3bb x . x in id unit")
    emptyStore
    (pTerm "unit")
    emptyStore
  , testCase "bigStep record" $ assertBigStep
    (pTerm
      "{ l1 = if true then false else true, l2 = if false then true else false }"
    )
    emptyStore
    (pTerm "{ l1 = false, l2 = false }")
    emptyStore
  , testCase "bigStep record extend" $ assertBigStep
    (pTerm "{ l1 = ref unit } with { l2 = ref unit }")
    emptyStore
    (TmRcd (Map.fromList [("l1", TmLoc 0), ("l2", TmLoc 1)]))
    (emptyStore `allocate'` TmUnit `allocate'` TmUnit)
  , testCase "bigStep record access" $ assertBigStep
    (pTerm "{ l1 = ref unit }.l1")
    emptyStore
    (TmLoc 0)
    (emptyStore `allocate'` TmUnit)
  , testCase "bigStep match extend" $ assertBigStep
    (pTerm "[ `l1 x -> x ] with [ `l2 y -> y ]")
    emptyStore
    (pTerm "[ `l1 x -> x, `l2 y -> y ]")
    emptyStore
  , testCase "bigStep variant" $ assertBigStep (pTerm "`l1 (ref unit)")
                                               emptyStore
                                               (TmVariant "l1" (TmLoc 0))
                                               (emptyStore `allocate'` TmUnit)
  , testCase "bigStep ref 1" $ assertBigStep
    (pTerm "ref unit")
    emptyStore
    (TmLoc 0)
    (Store { storeData    = Map.fromList [(0, WithMark False TmUnit)]
           , storeMinFree = 1
           }
    )
  , testCase "bigStep ref 2" $ assertBigStep
    (pTerm "ref (ref unit)")
    emptyStore
    (TmLoc 1)
    (Store
      { storeData    = Map.fromList
        [(0, WithMark False TmUnit), (1, WithMark False (TmLoc 0))]
      , storeMinFree = 2
      }
    )
  , testCase "bigStep deref" $ assertBigStep (pTerm "! (ref unit)")
                                             emptyStore
                                             (pTerm "unit")
                                             (emptyStore `allocate'` TmUnit)
  , testCase "bigStep assign" $ assertBigStep
    (pTerm "(ref zero) := succ zero")
    emptyStore
    (pTerm "unit")
    (emptyStore `allocate'` pTerm "succ zero")
  , testCase "bigStep unit"
    $ assertBigStep (pTerm "unit") emptyStore (pTerm "unit") emptyStore
  , testCase "bigStep true"
    $ assertBigStep (pTerm "true") emptyStore (pTerm "true") emptyStore
  , testCase "bigStep false"
    $ assertBigStep (pTerm "false") emptyStore (pTerm "false") emptyStore
  , testCase "bigStep if" $ assertBigStep
    (pTerm "if if true then false else true then succ zero else zero")
    emptyStore
    (pTerm "zero")
    emptyStore
    , testCase "bigStep zero" $ assertBigStep
    (pTerm "zero")
    emptyStore
    (pTerm "zero")
    emptyStore
        , testCase "bigStep succ" $ assertBigStep
    (pTerm "succ ((\x3bb x . x) (succ zero))")
    emptyStore
    (pTerm "succ (succ zero)")
    emptyStore
  ]
