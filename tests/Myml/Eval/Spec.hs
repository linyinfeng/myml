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
assertBigStep t1 s1 t2 s2 =
  runStateT (bigStep t1) (Just s1) >>= (@?= (t2, (Just s2)))

assertSmallStep
  :: Term
  -> Store (WithMark Term)
  -> Either EvalExcept Term
  -> Store (WithMark Term)
  -> Assertion
assertSmallStep t1 s1 t2 s2 =
  runSmallStepState (smallStep t1) (Just s1) >>= (@?= (t2, Just s2))

unitTests :: TestTree
unitTests = testGroup
  "Unit tests"
  [ testCase "bigStep application of abstraction" $ assertBigStep
    (pTerm "((\x3bb x . x) (\x3bb x . x)) ((\x3bb x . x) (\x3bb x . x))")
    emptyStore
    (pTerm "(\x3bb x . x)")
    emptyStore
  , testCase "smallStep application order 1" $ assertSmallStep
    (pTerm "(succ zero) (succ zero)")
    emptyStore
    (Right (pTerm "1 (succ zero)"))
    emptyStore
  , testCase "smallStep application order 2" $ assertSmallStep
    (pTerm "zero (succ zero)")
    emptyStore
    (Right (pTerm "0 1"))
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
    (pTerm "{ l1 = ref unit } extend { l2 = ref 1 }")
    emptyStore
    (       TmRcdExtend "l2"
    `TmApp` TmLoc 0
    `TmApp` (TmRcdExtend "l1" `TmApp` TmLoc 1 `TmApp` TmEmptyRcd)
    )
    (emptyStore `allocate'` TmNat 1 `allocate'` TmUnit)
  , testCase "bigStep record access 1" $ assertBigStep
    (pTerm "{ l1 = ref unit } extend { l2 = unit }.l1")
    emptyStore
    (TmLoc 0)
    (emptyStore `allocate'` TmUnit)
  , testCase "bigStep record access 2" $ assertBigStep
    (pTerm "{ l1 = unit } extend { l2 = unit }.l2")
    emptyStore
    (pTerm "unit")
    emptyStore
  , testCase "bigStep record access 3" $ assertBigStep
    (pTerm "{ l1 = unit } extend { l2 = unit }.l3")
    emptyStore
    (pTerm "{ }.l3")
    emptyStore
  , testCase "bigStep match extend" $ assertBigStep
    (pTerm "[ `l1 x = x ] extend [ `l2 y = y ]  extend [ `l3 z = z ]")
    emptyStore
    (pTerm "[ `l1 x = x, `l2 y = y, `l3 z = z ]")
    emptyStore
  , testCase "bigStep variant" $ assertBigStep
    (pTerm "`l1 (ref unit)")
    emptyStore
    (TmApp (TmVariant "l1") (TmLoc 0))
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
  , testCase "bigStep deref invalid location" $ assertBigStep
    (TmApp (TmAbs "_" (TmLoc 1)) (TmApp TmRef TmUnit))
    emptyStore
    (TmLoc 1)
    (emptyStore `allocate'` TmUnit)
  , testCase "bigStep assign" $ assertBigStep
    (pTerm "(ref zero) := (\x3bb x . succ x) zero")
    emptyStore
    (pTerm "unit")
    (emptyStore `allocate'` pTerm "1")
  , testCase "bigStep assign invalid location" $ assertBigStep
    (TmApp (TmApp TmAssign (TmLoc 0)) TmUnit)
    emptyStore
    (TmApp (TmApp TmAssign (TmLoc 0)) TmUnit)
    emptyStore
  , testCase "bigStep unit"
    $ assertBigStep (pTerm "unit") emptyStore (pTerm "unit") emptyStore
  , testCase "bigStep seq" $ assertBigStep
    (pTerm "let x = ref zero in x := succ zero; ! x")
    emptyStore
    (pTerm "1")
    (emptyStore `allocate'` pTerm "1")
  , testCase "bigStep true"
    $ assertBigStep (pTerm "true") emptyStore (pTerm "true") emptyStore
  , testCase "bigStep false"
    $ assertBigStep (pTerm "false") emptyStore (pTerm "false") emptyStore
  , testCase "bigStep if 1" $ assertBigStep
    (pTerm "if if true then false else true then succ zero else zero")
    emptyStore
    (pTerm "zero")
    emptyStore
  , testCase "bigStep if 2" $ assertBigStep
    (pTerm "if if false then false else true then succ zero else zero")
    emptyStore
    (pTerm "1")
    emptyStore
  , testCase "bigStep zero"
    $ assertBigStep (pTerm "zero") emptyStore (pTerm "zero") emptyStore
  , testCase "bigStep succ" $ assertBigStep
    (pTerm "succ ((\x3bb x . x) (succ zero))")
    emptyStore
    (pTerm "2")
    emptyStore
  , testCase "bigStep pred 1" $ assertBigStep (pTerm "pred (succ zero)")
                                              emptyStore
                                              (pTerm "zero")
                                              emptyStore
  , testCase "bigStep pred 2"
    $ assertBigStep (pTerm "pred zero") emptyStore (pTerm "zero") emptyStore
  , testCase "bigStep isZero" $ assertBigStep
    (pTerm "{ l1 = isZero (succ zero), l2 = isZero zero }")
    emptyStore
    (pTerm "{ l1 = false , l2 = true }")
    emptyStore
  ]
