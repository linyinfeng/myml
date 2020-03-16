module Myml.Test.Helper
  ( pTerm
  , pType
  , pTypeRow
  , pScheme
  , pKind
  , testInfer
  )
where

import           Myml.Syntax
import           Myml.Parser
import           Myml.Parser.Style
import           Myml.Parser.Common
import           Text.Trifecta           hiding ( Parser )
import           Myml.Typing
import           Data.Text.Prettyprint.Doc
import qualified Data.Map                      as Map

parseHelper :: Parser a -> String -> a
parseHelper p s = case parseString (unParser p) mempty s of
  Success t -> t
  Failure e -> error (show e)

pTerm :: String -> Term
pTerm = parseHelper parseTerm

pType :: String -> Type
pType = parseHelper parseType

pTypeRow :: String -> TypeRow
pTypeRow = parseHelper (parseTypeRow (ident identStyle))

pScheme :: String -> TypeScheme
pScheme = parseHelper parseScheme

pKind :: String -> Kind
pKind = parseHelper parseKind

testInfer :: String -> IO ()
testInfer t =
  let parsed   = pTerm t
      (res, _) = runInference (infer parsed >>= generalize parsed)
                              Map.empty
                              (InferenceState (NewVar Map.empty) True)
  in  case res of
        Right s -> print (pretty s)
        Left  e -> print e
