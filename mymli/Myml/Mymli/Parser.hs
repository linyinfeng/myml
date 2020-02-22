module Myml.Mymli.Parser
  ( parseInput
  , parseInputs
  , parseCommand
  )
where

import           Myml.Mymli.Input
import           Myml.Mymli.Command
import           Myml.Parser
import           Text.Trifecta
import           Control.Applicative

parseInput :: Parser Input
parseInput = parseInputBind <|> parseInputTerm <|> parseInputEmpty

parseInputs :: Parser [Input]
parseInputs = many parseInput

parseInputBind :: Parser Input
parseInputBind = do
  x <- try (ident identStyle <* symbol "=")
  InputBind x <$> parseTerm

parseInputTerm :: Parser Input
parseInputTerm = InputTerm <$> parseTerm

parseInputEmpty :: Parser Input
parseInputEmpty = return InputEmpty

parseCommand :: Parser Command
parseCommand =
  parseHelpCommand
    <|> parseExitCommand
    <|> parseShowTypeCommand
    <|> parseShowStoreCommand

parseHelpCommand :: Parser Command
parseHelpCommand = CmdHelp <$ (symbol "help" <|> symbol "h" <|> symbol "?")

parseExitCommand :: Parser Command
parseExitCommand = CmdExit <$ (symbol "exit" <|> symbol "quit" <|> symbol "q")

parseShowStoreCommand :: Parser Command
parseShowStoreCommand = CmdShowStore <$ symbol "store"

parseShowTypeCommand :: Parser Command
parseShowTypeCommand = do
  _ <- symbol "type" <|> symbol "ty" <|> symbol "t"
  CmdShowType <$> parseTerm
