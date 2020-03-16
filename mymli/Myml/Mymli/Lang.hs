{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables, TupleSections #-}

module Myml.Mymli.Lang
  ( processTopLevel
  , processTopLevel'
  , processTopLevels
  )
where

import           Myml.Mymli.Common
import           Myml.Mymli.Environment
import           Myml.Mymli.Output
import           Myml.Syntax
import           Myml.Parser.Common
import           Myml.Lang.Syntax
import           Myml.Lang.Parser
import           Myml.Eval.Store
import           Text.Trifecta           hiding ( Parser )
import           System.Console.ANSI
import           Data.Text.Prettyprint.Doc
import           System.FilePath
import           System.Directory
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Control.Exception
import           Control.Monad
import qualified Data.Map                      as Map

processTopLevel' :: MonadIO m => TopLevel -> Mymli m MymliRequest
processTopLevel' t = processTopLevel False t >> return MymliContinue

processTopLevel :: MonadIO m => Bool -> TopLevel -> Mymli m Bool
processTopLevel silent (TopTerm t) = do
  inferRes <- mymliInferTypeAndUpdateBinding t
  case inferRes of
    Left  e -> liftIO (typingErrorLabel >> print e) >> return False
    Right s -> do
      v <- mymliEval t
      unless
        silent
        (liftIO
          (print (pretty v) >> withColor Dull Green (putStr ": ") >> print
            (pretty s)
          )
        )
      mymliGc
      return True
processTopLevel silent (TopBind x t) = do
  inferRes <- mymliInferTypeAndUpdateBinding t
  case inferRes of
    Left  e -> liftIO (typingErrorLabel >> print e) >> return False
    Right s -> do
      v <- mymliEval t
      mymliAddBinding x t v s
      unless
        silent
        (liftIO
          (print (pretty x) >> withColor Dull Green (putStr ": ") >> print
            (pretty s)
          )
        )
      mymliGc
      return True
processTopLevel _ (TopImport file) = do
  result <- searchAndParseFile file
  case result of
    Nothing             -> return False
    Just (inputs, path) -> do
      env                <- mymliEnvForFile path
      (success, fileEnv) <- liftIO (runMymli (processTopLevels True inputs) env)
      when success (mymliMergeFileEnv fileEnv >> mymliGc)
      return success

searchAndParseFile
  :: MonadIO m => FilePath -> Mymli m (Maybe ([TopLevel], FilePath))
searchAndParseFile file = do
  searchPath <- gets envSearchPath
  exists     <- liftIO
    (mapM (\sp -> let p = sp </> file in (, p) <$> doesFileExist p) searchPath)
  let res = lookup True exists
  case res of
    Nothing -> Nothing <$ liftIO
      (ioErrorLabel >> putStrLn ("unable to find file \"" ++ file ++ "\""))
    Just path -> fmap (, path) <$> liftIO (parseSingleFile path)
 where
  parseSingleFile = handle (\(e :: IOException) -> print e >> return Nothing)
    . parseFromFile (unParser (whiteSpace *> parseTopLevels <* eof))

processTopLevels :: MonadIO m => Bool -> [TopLevel] -> Mymli m Bool
processTopLevels _      []           = return True
processTopLevels silent (t : remain) = do
  success <- processTopLevel silent t
  if success then processTopLevels silent remain else return False

mymliEnvForFile :: Monad m => FilePath -> Mymli m MymliEnv
mymliEnvForFile path = do
  MymliEnv { envOption, envStore = _, envTermBindings = _, envValueBindings = _, envTypeBindings = _, envInferState, envSearchPath } <-
    get
  return MymliEnv { envOption
                  , envStore         = emptyEnvStore envOption
                  , envTermBindings  = Map.empty
                  , envValueBindings = Map.empty
                  , envTypeBindings  = Map.empty
                  , envInferState    = envInferState
                  , envSearchPath    = takeDirectory path : envSearchPath
                  }

mymliMergeFileEnv :: Monad m => MymliEnv -> Mymli m ()
mymliMergeFileEnv fileEnv = do
  let
    MymliEnv { envOption = _, envStore = fileStore, envTermBindings = fileTermBindings, envValueBindings = fileValueBindings, envTypeBindings = fileTypeBindings, envInferState = fileInferState, envSearchPath = _ }
      = fileEnv
  MymliEnv { envOption = envOption, envStore = currentStore, envTermBindings = currentTermBindings, envValueBindings = currentValueBindings, envTypeBindings = currentTypeBindings, envInferState = _currentInferState, envSearchPath } <-
    get
  let
    env = MymliEnv
      { envOption
      , envStore         = case (fileStore, currentStore) of
        (Nothing, Nothing) -> Nothing
        (Just f, Just c) -> Just (mymliMergeFileStore f c)
        _ -> error "file imperative option mismatch with current environment"
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
      , envSearchPath    = envSearchPath
      }
  put env

mymliMergeFileStore
  :: Store (WithMark Term) -> Store (WithMark Term) -> Store (WithMark Term)
mymliMergeFileStore (Store fm fmf) (Store cm _) = Store
  { storeData    = Map.unionWith
                     (\_ _ -> error "store collied after file loading")
                     cm
                     fm
  , storeMinFree = fmf
  }
