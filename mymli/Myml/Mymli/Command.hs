{-# LANGUAGE RecordWildCards, NamedFieldPuns, ScopedTypeVariables #-}

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
import           Myml.Mymli.Input
import           Myml.Mymli.Input.Parser
import           Myml.Mymli.Text
import qualified Data.Text.IO                  as Text.IO
import           Control.Monad.Trans
import           Control.Monad.State
import           Text.Trifecta
import           System.Console.Haskeline
import           Data.Text.Prettyprint.Doc
import qualified Data.Map                      as Map

data Command = CmdExit
             | CmdHelp
             | CmdShowType Term
             | CmdShowStore
             | CmdShowValueBindings
             | CmdShowTermBindings
             | CmdShowTypeBindings
             | CmdInput
             | CmdLoadFile String -- filename
             deriving (Show)

processCommand :: Command -> Mymli (InputT IO) MymliRequest
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
  Store { storeData, ..} <- gets envStore
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
processCommand CmdInput = do
  inputRes <- multiLineInput
  case inputRes of
    Nothing          -> return MymliExit
    Just inputString -> do
      res <- liftIO (parseAndPrintError (parseInput <* eof) inputString)
      case res of
        Nothing    -> return MymliContinue
        Just input -> processInput input
 where
  multiLineInput = do
    res <- lift (getInputLine inputCmdPrompt)
    case res of
      Nothing -> return Nothing
      Just "" -> return (Just "")
      Just l  -> fmap (l ++) <$> multiLineInput
processCommand (CmdLoadFile file) = do
  result <- liftIO
    (handle (\(e :: IOException) -> print e >> return Nothing)
            (parseFromFile (whiteSpace *> parseInputs <* eof) file)
    )
  case result of
    Nothing     -> return MymliContinue
    Just inputs -> do
      env                <- mymliEnvForFile
      (success, fileEnv) <- liftIO (runMymli (processInputsFromFile inputs) env)
      when success (mymliMergeFileEnv fileEnv >> mymliGc)
      return MymliContinue

mymliEnvForFile :: Monad m => Mymli m MymliEnv
mymliEnvForFile = do
  MymliEnv { envStore, envTermBindings = _, envValueBindings = _, envTypeBindings = _, envInferState } <-
    get
  return MymliEnv { envStore         = Store Map.empty (storeMinFree envStore)
                  , envTermBindings  = Map.empty
                  , envValueBindings = Map.empty
                  , envTypeBindings  = Map.empty
                  , envInferState    = envInferState
                  }

mymliMergeFileEnv :: Monad m => MymliEnv -> Mymli m ()
mymliMergeFileEnv fileEnv = do
  let
    MymliEnv { envStore = Store fileStoreMap fileStoreMinFree, envTermBindings = fileTermBindings, envValueBindings = fileValueBindings, envTypeBindings = fileTypeBindings, envInferState = fileInferState }
      = fileEnv
  MymliEnv { envStore = Store currentStoreMap _currentStoreMinFree, envTermBindings = currentTermBindings, envValueBindings = currentValueBindings, envTypeBindings = currentTypeBindings, envInferState = _currentInferState } <-
    get
  let
    env = MymliEnv
      { envStore         = Store
        { storeData    = Map.unionWith
                           (\_ _ -> error "store collied after file loading")
                           currentStoreMap
                           fileStoreMap
        , storeMinFree = fileStoreMinFree
        }
      , envTermBindings  = Map.unionWith const
                                         fileTermBindings
                                         currentTermBindings
      , envValueBindings = Map.unionWith const
                                         fileValueBindings
                                         currentValueBindings
      , envTypeBindings  = Map.unionWith const
                                         fileTypeBindings
                                         currentTypeBindings
      , envInferState    = fileInferState
      }
  put env

processInputsFromFile :: MonadIO m => [Input] -> Mymli m Bool
processInputsFromFile []               = return True
processInputsFromFile (input : remain) = do
  success <- processInputFromFile input
  if success then processInputsFromFile remain else return False

processInputFromFile :: MonadIO m => Input -> Mymli m Bool
processInputFromFile (InputTerm t) = do
  inferRes <- mymliInferTypeAndUpdateBinding t
  case inferRes of
    Left e -> do
      liftIO (typingErrorLabel >> print e)
      return False
    Right _ -> do
      _ <- mymliEval t
      mymliGc
      return True
processInputFromFile (InputBind x t) = do
  inferRes <- mymliInferTypeAndUpdateBinding t
  case inferRes of
    Left e -> do
      liftIO (typingErrorLabel >> print e)
      return False
    Right s -> do
      v <- mymliEval t
      mymliAddBinding x t v s
      mymliGc
      return True
processInputFromFile InputEmpty = return True
