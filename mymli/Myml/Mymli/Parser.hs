module Myml.Mymli.Parser
  ( parseMymliInput
  , getMymliInput
  )
where

import           Myml.Mymli.Input
import           Myml.Mymli.Command
import           Myml.Mymli.Input.Parser
import           Myml.Mymli.Command.Parser
import           Text.Trifecta
import           Control.Applicative

type ParseResult = Either ErrInfo InputAndCommand
type InputAndCommand = Either Input Command

parseMymliInput :: Parser InputAndCommand
parseMymliInput = (Left <$> parseInput) <|> (Right <$> parseCommand)

-- TODO: refactor input and command structure
getMymliInput :: IO ParseResult
getMymliInput = getMymliInput' (stepParser parseMymliInput mempty)

getMymliInput' :: Step InputAndCommand -> IO ParseResult
getMymliInput' step = case step of
  StepDone _ res  -> return (Right res)
  StepFail _ info -> return (Left info)
  StepCont _ (Success res) _ -> return (Right res)
  StepCont _ (Failure _) _ -> getLine >>= \l -> getMymliInput' (feed (l ++ "\n") step)
