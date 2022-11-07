{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Myml.Mymli.Lang
  ( processTopLevel,
    processTopLevel',
    processTopLevels,
    searchAndParseFile,
    mymliEnvForFile,
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Myml.Eval.Store
import Myml.Lang.Parser
import Myml.Lang.Syntax
import Myml.Mymli.Common
import Myml.Mymli.Environment
import Myml.Mymli.Option
import Myml.Mymli.Output
import Myml.Parser.Common
import Myml.Syntax
import Prettyprinter
import Prettyprinter.Render.Text
import System.Directory
import System.FilePath
import System.IO
import Text.Trifecta hiding
  ( Parser,
    line,
  )
import Text.Trifecta.Delta

processTopLevel' :: MonadIO m => Careted TopLevel -> Mymli m MymliRequest
processTopLevel' t = processTopLevel False t >> return MymliContinue

printValueSchemePair :: MymliOptions -> Term -> TypeScheme -> IO ()
printValueSchemePair opt t s = do
  renderDoc
    ( group
        (displayTerm t <> line <> pretty ":" <> line <> align (displayScheme s))
    )
  putStr "\n"
  where
    displayTerm :: Term -> Doc ann
    displayTerm = if optVerbose opt then pretty else displayValue
    renderDoc :: Doc ann -> IO ()
    renderDoc d = renderIO stdout (layoutSmart defaultLayoutOptions d)

printCaret :: Caret -> IO ()
printCaret (Caret d _) = do
  putStr (show (prettyDelta d))
  putStr ": "

processTopLevel :: MonadIO m => Bool -> Careted TopLevel -> Mymli m Bool
processTopLevel silent (TopTerm t :^ car) = do
  inferRes <- mymliInferTypeAndUpdateBinding t
  case inferRes of
    Left e ->
      liftIO (printCaret car >> typingErrorLabel >> print e) >> return False
    Right s -> do
      evalRes <- mymliEval t
      case evalRes of
        Left e ->
          liftIO (printCaret car >> evalErrorLabel >> print e) >> return False
        Right v -> postEval silent v s
processTopLevel silent (TopBind x t :^ car) = do
  inferRes <- mymliInferTypeAndUpdateBinding t
  case inferRes of
    Left e ->
      liftIO (printCaret car >> typingErrorLabel >> print e) >> return False
    Right s -> do
      evalRes <- mymliEval t
      case evalRes of
        Left e ->
          liftIO (printCaret car >> evalErrorLabel >> print e) >> return False
        Right v -> do
          mymliAddBinding x t v s
          postEval silent v s
processTopLevel _ (TopImport file :^ _) = do
  result <- searchAndParseFile file
  case result of
    Nothing -> return False
    Just (inputs, path) -> do
      env <- mymliEnvForFile path
      (success, fileEnv) <- liftIO (runMymli (processTopLevels True inputs) env)
      when success (mymliMergeFileEnv fileEnv >> mymliGc)
      return success

postEval :: MonadIO m => Bool -> Term -> TypeScheme -> Mymli m Bool
postEval silent v s = do
  options <- gets envOption
  unless silent (liftIO (printValueSchemePair options v s))
  mymliGc
  return True

searchAndParseFile ::
  MonadIO m => FilePath -> Mymli m (Maybe ([Careted TopLevel], FilePath))
searchAndParseFile file = do
  searchPath <- gets envSearchPath
  exists <-
    liftIO
      (mapM (\sp -> let p = sp </> file in (,p) <$> doesFileExist p) searchPath)
  let res = lookup True exists
  case res of
    Nothing ->
      Nothing
        <$ liftIO
          (ioErrorLabel >> putStrLn ("unable to find file \"" ++ file ++ "\""))
    Just path -> fmap (,path) <$> liftIO (parseSingleFile path)
  where
    parseSingleFile =
      handle (\(e :: IOException) -> print e >> return Nothing)
        . parseFromFile (unParser (whiteSpace *> parseTopLevelsCareted <* eof))

processTopLevels :: MonadIO m => Bool -> [Careted TopLevel] -> Mymli m Bool
processTopLevels _ [] = return True
processTopLevels silent (t : remain) = do
  success <- processTopLevel silent t
  if success then processTopLevels silent remain else return False

mymliEnvForFile :: Monad m => FilePath -> Mymli m MymliEnv
mymliEnvForFile path = do
  env <- get
  let opt = envOption env
  return
    MymliEnv
      { envOption = opt,
        envStore = (\s -> s {storeData = Map.empty}) <$> envStore env,
        envTermBindings = Map.empty,
        envValueBindings = Map.empty,
        envTypeBindings = Map.empty,
        envInferState = envInferState env,
        envSearchPath = takeDirectory path : envSearchPath env
      }

mymliMergeFileEnv :: Monad m => MymliEnv -> Mymli m ()
mymliMergeFileEnv file = do
  current <- get
  let env =
        MymliEnv
          { envOption = envOption current,
            envStore = case (envStore file, envStore current) of
              (Nothing, Nothing) -> Nothing
              (Just f, Just c) -> Just (mymliMergeFileStore f c)
              _ -> error "file imperative option mismatch with current environment",
            envTermBindings =
              Map.unionWith
                const
                (envTermBindings file)
                (envTermBindings current),
            envValueBindings =
              Map.unionWith
                const
                (envValueBindings file)
                (envValueBindings current),
            envTypeBindings =
              Map.unionWith
                const
                (envTypeBindings file)
                (envTypeBindings current),
            envInferState = envInferState file,
            envSearchPath = envSearchPath current
          }
  put env

mymliMergeFileStore ::
  Store (WithMark Term) -> Store (WithMark Term) -> Store (WithMark Term)
mymliMergeFileStore (Store fm fmf) (Store cm _) =
  Store
    { storeData =
        Map.unionWith
          (\_ _ -> error "store collied after file loading")
          cm
          fm,
      storeMinFree = fmf
    }
