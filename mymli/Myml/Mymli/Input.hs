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
  t' <- mymliAddLets t
  case mymliInferType t' of
    Left  e -> liftIO (putStrLn ("[Typing Error] " ++ show e))
    Right s -> do
      v <- mymliEval t'
      liftIO (putStrLn (show (pretty v) ++ " : " ++ show (pretty s)))
  return MymliContinue
processInput (InputBind x t) = do
  t' <- mymliAddLets t
  case mymliInferType t' of
    Left  e -> liftIO (putStrLn ("[Typing Error] " ++ show e))
    Right s -> do
      v <- mymliEval t'
      mymliAddBinding x v
      liftIO (putStrLn (x ++ " : " ++ show (pretty s)))
  return MymliContinue
