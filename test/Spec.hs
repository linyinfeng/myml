module Main
  ( main
  )
where

import           Test.Tasty

import qualified Myml.Parser.Spec
import qualified Myml.Eval.Store.Spec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Myml.Parser.Spec.tests, Myml.Eval.Store.Spec.tests]
