module Main
  ( main
  )
where

import           Test.Tasty

import qualified Myml.Parser.Spec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Myml.Parser.Spec.tests]
