{-# LANGUAGE RankNTypes #-}
module Myml.Mymli.Common
  ( prompt
  , promptMultiLine
  , mymliInferTypeAndUpdateBinding
  , mymliSubstEnv
  , mymliEval
  , mymliGc
  )
where

import           Myml.Syntax
import           Myml.Typing
import           Myml.Subst
import           Myml.Eval
import           Myml.Eval.Store
import           Myml.Mymli.Environment
import           Control.Monad.State
import           Control.Monad.Except
import qualified Data.Map                      as Map

prompt :: String
prompt = "\ESC[1;32m\STXmymli\ESC[0m\STX> "

promptMultiLine :: String
promptMultiLine = "     | "

mymliInferTypeAndUpdateBinding
  :: Monad m => Term -> Mymli m (Either Error TypeScheme)
mymliInferTypeAndUpdateBinding t = do
  termBindings <- gets envTermBindings
  typeBindings <- gets envTypeBindings
  inferState   <- gets envInferState
  let (result, inferState') = runInference
        (inference termBindings typeBindings t)
        typeBindings
        inferState
  modify (\e -> e { envInferState = inferState' })
  case result of
    Left e -> return (Left e)
    Right (bindings', s) ->
      modify (\e -> e { envTypeBindings = bindings' }) >> return (Right s)
 where
  inference termBindings typeBindings term = do
    ty'           <- infer term -- maybe failed
    inferred      <- generalize term ty'
    -- update bindings
    typeBindings' <- sequence
      (Map.intersectionWith updateBinding termBindings typeBindings)
    return (typeBindings', inferred)

updateBinding :: Term -> TypeScheme -> Inference TypeScheme
updateBinding t s = do
  fv <- liftEither (fvScheme s)
  if Map.null fv then return s else instantiate s >>= generalize t

mymliSubstEnv :: Monad m => Term -> Mymli m Term
mymliSubstEnv t = flip substTerm t <$> gets envValueBindings

mymliEval :: MonadIO m => Term -> Mymli m (Either Error Term)
mymliEval t = do
  t'          <- mymliSubstEnv t
  store       <- gets envStore
  (v, store') <- liftIO (runStateT (bigStepSafe t') store)
  modify (\e -> e { envStore = store' })
  return v

mymliGc :: Monad m => Mymli m ()
mymliGc = do
  bindings   <- gets envValueBindings
  maybeStore <- gets envStore
  case maybeStore of
    Nothing    -> return ()
    Just store -> do
      let store' = markSweepClear (Map.elems bindings) store
      modify (\e -> e { envStore = Just store' })
