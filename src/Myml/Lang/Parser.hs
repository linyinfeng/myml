module Myml.Lang.Parser
  ( parseTopLevel,
    parseTopLevels,
    parseTopLevelsCareted,
  )
where

import Control.Applicative
import Myml.Lang.Syntax
import Myml.Parser
import Myml.Parser.Common
import Myml.Parser.Style
import Text.Trifecta hiding (Parser)

parseTopLevel :: Parser TopLevel
parseTopLevel =
  (parseTopBind <|> parseTopTerm <|> parseTopImport) <* symbol ";;"

parseTopLevels :: Parser [TopLevel]
parseTopLevels = many parseTopLevel

parseTopLevelsCareted :: Parser [Careted TopLevel]
parseTopLevelsCareted = many (careted parseTopLevel)

parseTopBind :: Parser TopLevel
parseTopBind = do
  (x, t) <- try letBindingPair
  return (TopBind x t)

parseTopTerm :: Parser TopLevel
parseTopTerm = TopTerm <$> parseTerm

parseTopImport :: Parser TopLevel
parseTopImport = do
  reserve identStyle "import"
  TopImport <$> stringLiteral
