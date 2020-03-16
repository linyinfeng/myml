{-# LANGUAGE LambdaCase #-}

module Myml.Mymli.Input
  ( Input(..)
  , processInput
  , parseMymliInput
  , getMymliInput
  )
where

import           Myml.Lang.Syntax
import           Myml.Lang.Parser
import           Myml.Parser.Common
import           Myml.Mymli.Common
import           Myml.Mymli.Lang
import           Myml.Mymli.Command
import           Myml.Mymli.Command.Parser
import           Myml.Mymli.Environment
import           Text.Trifecta hiding (Parser)
import           Control.Applicative
import           Control.Monad.Trans
import           System.Console.Haskeline

data Input = InputTopLevel TopLevel
           | InputCommand Command

type ParseResult = Either ErrInfo Input

parseMymliInput :: Parser Input
parseMymliInput =
  (InputCommand <$> parseCommand) <|> (InputTopLevel <$> parseTopLevel)

getMymliInput :: (InputT IO) ParseResult
getMymliInput =
  getMymliInput' True (stepParser (unParser (parseMymliInput <* eof)) mempty)

getMymliInput' :: Bool -> Step Input -> (InputT IO) ParseResult
getMymliInput' firstLine step = case step of
  StepDone _ res             -> return (Right res)
  StepFail _ info            -> return (Left info)
  StepCont _ (Success res) _ -> return (Right res)
  StepCont _ (Failure _  ) _ -> getInputLine p >>= \case
    Nothing -> return (Right (InputCommand CmdExit))
    Just l  -> getMymliInput' False (feed (l ++ "\n") step)
    where p = if firstLine then prompt else promptMultiLine

processInput :: MonadIO m => Input -> Mymli m MymliRequest
processInput (InputTopLevel t) = processTopLevel' t
processInput (InputCommand  c) = processCommand c
