{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Myml.Mymli.Command
  ( Command(..)
  , processCommand
  )
where

import           Myml.Mymli.Environment
import           Myml.Syntax
import           Myml.Eval.Store
import           Myml.Mymli.Common
import           Control.Monad.Trans
import           Control.Monad.State
import           Data.Text.Prettyprint.Doc
import qualified Data.Map                      as Map

data Command = CmdExit
             | CmdHelp
             | CmdShowType Term
             | CmdShowStore
             deriving (Show)

processCommand :: MonadIO m => Command -> Mymli m MymliRequest
processCommand CmdExit = return MymliExit
processCommand CmdHelp = do
  liftIO (putStrLn "Help is unimplemented")
  return MymliContinue
processCommand (CmdShowType t) = do
  inferRes <- mymliInferTypeAndUpdateBinding t
  case inferRes of
    Left  e -> liftIO (putStrLn ("[Typing Error] " ++ show e))
    Right s -> liftIO (print (pretty s))
  return MymliContinue
processCommand CmdShowStore = do
  Store { storeData, ..} <- gets envStore
  liftIO (print (pretty (Map.toList (Map.map removeMark storeData))))
  return MymliContinue
