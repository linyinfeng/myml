module Main
  ( main
  )
where

import           Test.Tasty
import qualified Myml.Parser.Spec
import qualified Myml.Eval.Store.Spec
import qualified Myml.Syntax.Spec
import qualified Myml.Subst.Spec
import qualified Myml.Eval.Spec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup
  "Tests"
  [ Myml.Parser.Spec.tests
  , Myml.Eval.Store.Spec.tests
  , Myml.Syntax.Spec.tests
  , Myml.Subst.Spec.tests
  , Myml.Eval.Spec.tests
  ]
