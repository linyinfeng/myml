module Myml.Lang.Parser
  ( parseTopLevel
  , parseTopLevels
  )
where

import           Myml.Lang.Syntax
import           Myml.Parser
import           Myml.Parser.Style
import           Text.Trifecta
import           Control.Applicative

parseTopLevel :: Parser TopLevel
parseTopLevel =
  (parseTopBind <|> parseTopTerm <|> parseTopImport) <* symbol ";;"

parseTopLevels :: Parser [TopLevel]
parseTopLevels = many parseTopLevel

parseTopBind :: Parser TopLevel
parseTopBind = do
  x <- try (ident identStyle <* reserve identStyle "=")
  TopBind x <$> parseTerm

parseTopTerm :: Parser TopLevel
parseTopTerm = TopTerm <$> parseTerm

parseTopImport :: Parser TopLevel
parseTopImport = do
  reserve identStyle "import"
  TopImport <$> stringLiteral
