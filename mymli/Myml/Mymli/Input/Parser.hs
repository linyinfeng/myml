module Myml.Mymli.Input.Parser
  ( parseInput
  , parseInputs
  )
where

import           Myml.Mymli.Input
import           Myml.Parser
import           Text.Trifecta
import           Control.Applicative

parseInput :: Parser Input
parseInput = parseInputBind <|> parseInputTerm <|> parseInputEmpty

parseInputs :: Parser [Input]
parseInputs = many (parseInput <* symbol ",")

parseInputBind :: Parser Input
parseInputBind = do
  x <- try (ident identStyle <* symbol "=")
  InputBind x <$> parseTerm

parseInputTerm :: Parser Input
parseInputTerm = InputTerm <$> parseTerm

parseInputEmpty :: Parser Input
parseInputEmpty = return InputEmpty
