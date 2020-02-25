module Myml.Mymli.Command.Parser
  ( parseCommand
  )
where

import           Myml.Mymli.Command
import           Myml.Parser
import           Text.Trifecta
import           Control.Applicative

parseCommand :: Parser Command
parseCommand =
  parseHelpCommand
    <|> parseExitCommand
    <|> parseShowTypeCommand
    <|> parseShowStoreCommand
    <|> parseShowBindingsCommand
    <|> parseInputCommand
    <|> parseLoadFileCommand

parseHelpCommand :: Parser Command
parseHelpCommand = CmdHelp <$ symbol "help"

parseExitCommand :: Parser Command
parseExitCommand = CmdExit <$ (symbol "exit" <|> symbol "quit")

parseShowStoreCommand :: Parser Command
parseShowStoreCommand = CmdShowStore <$ symbol "store"

parseShowTypeCommand :: Parser Command
parseShowTypeCommand = do
  _ <- symbol "type"
  CmdShowType <$> parseTerm

parseShowBindingsCommand :: Parser Command
parseShowBindingsCommand =
  (symbol "bindings" <|> symbol "b") *> (v <|> t <|> ty)
 where
  v  = CmdShowValueBindings <$ symbol "value"
  t  = CmdShowTermBindings <$ symbol "term"
  ty = CmdShowTypeBindings <$ symbol "type"

parseInputCommand :: Parser Command
parseInputCommand = CmdInput <$ symbol "input"

parseLoadFileCommand :: Parser Command
parseLoadFileCommand = CmdLoadFile <$> (symbol "load" *> stringLiteral)
