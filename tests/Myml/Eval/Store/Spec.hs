module Myml.Eval.Store.Spec
  ( tests
  )
where

import           Myml.Eval.Store
import           Myml.Syntax
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

tests :: TestTree
tests = testGroup "Myml.Eval.Store.Spec" [unitTests]

exampleStore1 :: Store (WithMark Term)
exampleStore1 = emptyStore `allocate'` termUnit

exampleStore2 :: Store (WithMark Term)
exampleStore2 = exampleStore1 `allocate'` TmLoc 0

exampleStore3 :: Store (WithMark Term)
exampleStore3 = exampleStore2 `allocate'` termUnit

exampleStore4 :: Store (WithMark Term)
exampleStore4 =
  exampleStore3
    `allocate'` TmApp (TmLoc 1) (TmLoc 2)
    `allocate'` TmLoc 1
    `allocate'` termUnit

exampleStore4Marked :: Store (WithMark Term)
exampleStore4Marked = markStore [TmLoc 3] exampleStore4

unitTests :: TestTree
unitTests = testGroup
  "Unit tests"
  [ testCase "Example store 1" $ exampleStore1 @?= Store
    { storeData    = Map.fromList [(0, WithMark False termUnit)]
    , storeMinFree = 1
    }
  , testCase "Example store 2" $ exampleStore2 @?= Store
    { storeData    = Map.fromList
      [(0, WithMark False termUnit), (1, WithMark False (TmLoc 0))]
    , storeMinFree = 2
    }
  , testCase "Example store 3" $ exampleStore3 @?= Store
    { storeData    = Map.fromList
                       [ (0, WithMark False termUnit)
                       , (1, WithMark False (TmLoc 0))
                       , (2, WithMark False termUnit)
                       ]
    , storeMinFree = 3
    }
  , testCase "Example store 4" $ exampleStore4 @?= Store
    { storeData    = Map.fromList
                       [ (0, WithMark False termUnit)
                       , (1, WithMark False (TmLoc 0))
                       , (2, WithMark False termUnit)
                       , (3, WithMark False (TmApp (TmLoc 1) (TmLoc 2)))
                       , (4, WithMark False (TmLoc 1))
                       , (5, WithMark False termUnit)
                       ]
    , storeMinFree = 6
    }
  , testCase "Locatoins"
  $   locations (TmApp (TmLoc 1) (TmLoc 2))
  @?= Set.fromList [1, 2]
  , testCase "Mark store" $ exampleStore4Marked @?= Store
    { storeData    = Map.fromList
                       [ (0, WithMark True termUnit)
                       , (1, WithMark True (TmLoc 0))
                       , (2, WithMark True termUnit)
                       , (3, WithMark True (TmApp (TmLoc 1) (TmLoc 2)))
                       , (4, WithMark False (TmLoc 1))
                       , (5, WithMark False termUnit)
                       ]
    , storeMinFree = 6
    }
  , testCase "Sweep store" $ sweepStore exampleStore4Marked @?= Store
    { storeData    = Map.fromList
                       [ (0, WithMark True termUnit)
                       , (1, WithMark True (TmLoc 0))
                       , (2, WithMark True termUnit)
                       , (3, WithMark True (TmApp (TmLoc 1) (TmLoc 2)))
                       ]
    , storeMinFree = 6
    }
  , testCase "Clear store mark"
  $   clearStoreMark exampleStore4Marked
  @?= exampleStore4
  , testCase "Mark sweep and clear"
  $   markSweepClear [TmLoc 3] exampleStore4
  @?= Store
        { storeData    = Map.fromList
                           [ (0, WithMark False termUnit)
                           , (1, WithMark False (TmLoc 0))
                           , (2, WithMark False termUnit)
                           , (3, WithMark False (TmApp (TmLoc 1) (TmLoc 2)))
                           ]
        , storeMinFree = 6
        }
  ]
