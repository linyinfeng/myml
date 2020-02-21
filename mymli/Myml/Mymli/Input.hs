module Myml.Mymli.Input
  ( Input(..)
  , processInput
  )
where

import           Myml.Mymli.Common
import           Myml.Mymli.Environment
import           Myml.Syntax
import           Data.Text.Prettyprint.Doc
import           Control.Monad.Trans

data Input = InputBind VarName Term
           | InputTerm Term

processInput :: MonadIO m => Input -> Mymli m MymliRequest
processInput (InputTerm t) = do
  inferRes <- mymliInferTypeAndUpdateBinding t
  case inferRes of
    Left  e -> liftIO (putStrLn ("[Typing Error] " ++ show e))
    Right s -> do
      v <- mymliEval t
      liftIO (putStrLn (show (pretty v) ++ " : " ++ show (pretty s)))
      mymliGc
  return MymliContinue
processInput (InputBind x t) = do
  inferRes <- mymliInferTypeAndUpdateBinding t
  case inferRes of
    Left  e -> liftIO (putStrLn ("[Typing Error] " ++ show e))
    Right s -> do
      v <- mymliEval t
      mymliAddBinding x v s
      liftIO (putStrLn (x ++ " : " ++ show (pretty s)))
      mymliGc
  return MymliContinue
