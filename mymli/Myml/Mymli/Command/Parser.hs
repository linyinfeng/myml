module Myml.Mymli.Command.Parser
  ( parseCommand,
  )
where

import Control.Applicative
import Myml.Mymli.Command
import Myml.Parser
import Myml.Parser.Common
import Text.Trifecta hiding (Parser)

parseCommand :: Parser Command
parseCommand =
  symbol ":"
    *> ( parseHelpCommand
           <|> parseExitCommand
           <|> parseShowTypeCommand
           <|> parseShowStoreCommand
           <|> parseShowBindingsCommand
       )

parseHelpCommand :: Parser Command
parseHelpCommand = CmdHelp <$ symbol "help"

parseExitCommand :: Parser Command
parseExitCommand = CmdExit <$ symbol "quit"

parseShowStoreCommand :: Parser Command
parseShowStoreCommand = CmdShowStore <$ symbol "store"

parseShowTypeCommand :: Parser Command
parseShowTypeCommand = do
  _ <- symbol "type"
  CmdShowType <$> parseTerm

parseShowBindingsCommand :: Parser Command
parseShowBindingsCommand = symbol "bindings" *> (v <|> t <|> ty)
  where
    v = CmdShowValueBindings <$ symbol "value"
    t = CmdShowTermBindings <$ symbol "term"
    ty = CmdShowTypeBindings <$ symbol "type"
