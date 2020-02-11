module Myml.Parser.Spec
  ( tests
  )
where

import           Data.Text.Prettyprint.Doc      ( pretty
                                                , Pretty
                                                )
import           Myml.Parser
import           Myml.Syntax
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.SmallCheck         as SC
import           Text.Trifecta

tests :: TestTree
tests = testGroup "Myml.Parser.Spec" [scProps, unitTests]

scProps :: TestTree
scProps = testGroup
  "SmallCheck Properties"
  [ localOption (SmallCheckDepth 2)
  $ SC.testProperty "parse (pretty term) == term"
  $ propPrintParse parseTerm
  , localOption (SmallCheckDepth 2)
  $ SC.testProperty "parse (pretty type) == type"
  $ propPrintParse parseType
  ]

propPrintParse :: (Pretty a, Eq a) => Parser a -> a -> Bool
propPrintParse p t = case parsed of
  Success t'     -> t == t'
  Failure _error -> False
 where
  printed = show (pretty t)
  parsed  = runParser p mempty printed

testTermParser :: String -> Term -> Assertion
testTermParser s t = case parseString parseTerm mempty s of
  Success res           -> res @?= t
  Failure (ErrInfo d _) -> assertFailure (show d)

unitTests :: TestTree
unitTests = testGroup
  "Unit tests"
  [ testCase "Simple abstraction"
    $ testTermParser "\x3bbx. x" (TmAbs "x" (TmVar "x"))
  , testCase "Application in abstraction"
    $ testTermParser "\x3bbx. x x" (TmAbs "x" (TmApp (TmVar "x") (TmVar "x")))
  , testCase "Abstraction in application 1"
    $ testTermParser "x (\x3bbx. x)" (TmApp (TmVar "x") (TmAbs "x" (TmVar "x")))
  , testCase "Abstraction in application 2"
    $ testTermParser "(\x3bbx. x) x" (TmApp (TmAbs "x" (TmVar "x")) (TmVar "x"))
  ]
