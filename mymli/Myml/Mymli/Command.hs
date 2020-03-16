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
import           Myml.Mymli.Output
import           Myml.Mymli.Text
import qualified Data.Text.IO                  as Text.IO
import           Control.Monad.Trans
import           Control.Monad.State
import           Data.Text.Prettyprint.Doc
import qualified Data.Map                      as Map

data Command = CmdExit
             | CmdHelp
             | CmdShowType Term
             | CmdShowStore
             | CmdShowValueBindings
             | CmdShowTermBindings
             | CmdShowTypeBindings
             deriving (Show)

processCommand :: MonadIO m => Command -> Mymli m MymliRequest
processCommand CmdExit = return MymliExit
processCommand CmdHelp = do
  liftIO (Text.IO.putStrLn mymliHelpText)
  return MymliContinue
processCommand (CmdShowType t) = do
  inferRes <- mymliInferTypeAndUpdateBinding t
  case inferRes of
    Left  e -> liftIO (typingErrorLabel >> print e)
    Right s -> liftIO (print (pretty s))
  return MymliContinue
processCommand CmdShowStore = do
  maybeStore <- gets envStore
  case maybeStore of
    Nothing -> return ()
    Just Store { storeData, ..} ->
      liftIO (print (pretty (Map.toList (Map.map removeMark storeData))))
  return MymliContinue
processCommand CmdShowValueBindings = do
  valueBindings <- gets envValueBindings
  liftIO (print (pretty (Map.toList valueBindings)))
  return MymliContinue
processCommand CmdShowTermBindings = do
  termBindings <- gets envTermBindings
  liftIO (print (pretty (Map.toList termBindings)))
  return MymliContinue
processCommand CmdShowTypeBindings = do
  typeBindings <- gets envTypeBindings
  liftIO (print (pretty (Map.toList typeBindings)))
  return MymliContinue
