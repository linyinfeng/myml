module Myml.Parser.Spec
  ( tests
  )
where

import           Data.Text.Prettyprint.Doc      ( pretty
                                                , Pretty
                                                )
import           Myml.Parser
import           Myml.Parser.Common
import           Myml.Syntax
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.SmallCheck         as SC
import           Text.Trifecta           hiding ( Parser )

tests :: TestTree
tests = testGroup "Myml.Parser.Spec" [scProps, unitTests]

scProps :: TestTree
scProps = testGroup
  "SmallCheck properties"
  [ localOption (SmallCheckDepth 2)
  $ SC.testProperty "parse (pretty term) == term"
  $ propPrintParse parseTerm
  , localOption (SmallCheckDepth 3)
  $ SC.testProperty "parse (pretty type) == type"
  $ propPrintParse parseType
  , localOption (SmallCheckDepth 3)
  $ SC.testProperty "parse (pretty kind) == kind"
  $ propPrintParse parseKind
  , localOption (SmallCheckDepth 3)
  $ SC.testProperty "parse (pretty scheme) == scheme"
  $ propPrintParse parseScheme
  ]

propPrintParse :: (Pretty a, Eq a) => Parser a -> a -> Bool
propPrintParse p t = case parsed of
  Success t'     -> t == t'
  Failure _error -> False
 where
  printed = show (pretty t)
  parsed  = runParser (unParser p) mempty printed

testTermParser :: String -> Term -> Assertion
testTermParser s t = case parseString (unParser parseTerm) mempty s of
  Success res           -> res @?= t
  Failure (ErrInfo d _) -> assertFailure (show d)

unitTests :: TestTree
unitTests = testGroup
  "Unit tests"
  [ testCase "Simple abstraction"
    $ testTermParser "\x3bb x . x" (TmAbs "x" (TmVar "x"))
  , testCase "Application in abstraction"
    $ testTermParser "\x3bb x . x x" (TmAbs "x" (TmApp (TmVar "x") (TmVar "x")))
  , testCase "Abstraction in application 1" $ testTermParser
    "x (\x3bb x . x)"
    (TmApp (TmVar "x") (TmAbs "x" (TmVar "x")))
  , testCase "Abstraction in application 2" $ testTermParser
    "(\x3bb x . x) x"
    (TmApp (TmAbs "x" (TmVar "x")) (TmVar "x"))
  , testCase "Assignment in abstraction" $ testTermParser
    "\x3bb x . x := x"
    (TmAbs "x" (TmApp (TmApp TmAssign (TmVar "x")) (TmVar "x")))
  , testCase "Assignment in let" $ testTermParser
    "let x = x in x := x"
    (TmLet "x" (TmVar "x") (TmApp (TmApp TmAssign (TmVar "x")) (TmVar "x")))
  , testCase "Application and record in assignment" $ testTermParser
    "x x := x x; x := x.l"
    (TmSeq
      (TmApp (TmApp TmAssign (TmApp (TmVar "x") (TmVar "x")))
             (TmApp (TmVar "x") (TmVar "x"))
      )
      (TmApp (TmApp TmAssign (TmVar "x")) (TmApp (TmRcdAccess "l") (TmVar "x")))
    )
  , testCase "Tuple 0" $ testTermParser "()" TmUnit
  , testCase "Tuple 1" $ testTermParser "(x)" (TmVar "x")
  , testCase "Tuple 3" $ testTermParser
    "(x, y, z)"
    (       TmRcdExtend "3"
    `TmApp` TmVar "z"
    `TmApp` (       TmRcdExtend "2"
            `TmApp` TmVar "y"
            `TmApp` (TmRcdExtend "1" `TmApp` TmVar "x" `TmApp` TmEmptyRcd)
            )
    )
  ]
