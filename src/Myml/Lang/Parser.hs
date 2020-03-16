module Myml.Lang.Parser
  ( parseTopLevel
  , parseTopLevels
  )
where

import           Myml.Syntax
import           Myml.Lang.Syntax
import           Myml.Parser
import           Myml.Parser.Common
import           Myml.Parser.Style
import           Text.Trifecta hiding (Parser)
import           Control.Applicative

parseTopLevel :: Parser TopLevel
parseTopLevel =
  (parseTopBind <|> parseTopTerm <|> parseTopImport) <* symbol ";;"

parseTopLevels :: Parser [TopLevel]
parseTopLevels = many parseTopLevel

parseTopBind :: Parser TopLevel
parseTopBind = do
  b <- try
    (do
      x      <- ident identStyle
      params <- many (ident identStyle)
      reserve identStyle "="
      return (\t -> TopBind x (foldr TmAbs t params))
    )
  b <$> parseTerm

parseTopTerm :: Parser TopLevel
parseTopTerm = TopTerm <$> parseTerm

parseTopImport :: Parser TopLevel
parseTopImport = do
  reserve identStyle "import"
  TopImport <$> stringLiteral
