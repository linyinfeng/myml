module Myml.Mymli.Input
  ( Input(..)
  , processInput
  )
where

import           Myml.Mymli.Common
import           Myml.Mymli.Environment
import           Myml.Mymli.Output
import           Myml.Syntax
import           Data.Text.Prettyprint.Doc
import           Control.Monad.Trans

data Input = InputBind VarName Term
           | InputTerm Term
           | InputEmpty

processInput :: MonadIO m => Input -> Mymli m MymliRequest
processInput (InputTerm t) = do
  inferRes <- mymliInferTypeAndUpdateBinding t
  case inferRes of
    Left  e -> liftIO (typingErrorLabel >> print e)
    Right _s -> do
      v <- mymliEval t
      liftIO (print (pretty v))
      mymliGc
  return MymliContinue
processInput (InputBind x t) = do
  inferRes <- mymliInferTypeAndUpdateBinding t
  case inferRes of
    Left  e -> liftIO (typingErrorLabel >> print e)
    Right s -> do
      v <- mymliEval t
      mymliAddBinding x t v s
      mymliGc
  return MymliContinue
processInput InputEmpty = return MymliContinue
