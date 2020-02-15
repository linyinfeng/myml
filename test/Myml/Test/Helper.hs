module Myml.Test.Helper
  ( pTerm
  , pType
  , pTypeRow
  , pScheme
  , pKind
  )
where

import           Myml.Syntax
import           Myml.Parser
import           Text.Trifecta

parseHelper :: Parser a -> String -> a
parseHelper p s = case parseString p mempty s of
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
