{-# LANGUAGE RankNTypes #-}
module Myml.Mymli.Common
  ( mymliInferTypeAndUpdateBinding
  , mymliAddLetsForEval
  , mymliEval
  , mymliGc
  )
where

import           Myml.Syntax
import           Myml.Typing
import           Myml.Eval
import           Myml.Eval.Store
import           Myml.Mymli.Environment
import           Control.Monad.State
import qualified Data.Map                      as Map

mymliAddLetsForEval :: Monad m => Term -> Mymli m Term
mymliAddLetsForEval t = Map.foldrWithKey TmLet t <$> gets envValueBindings

mymliInferTypeAndUpdateBinding
  :: Monad m => Term -> Mymli m (Either TypingExcept TypeScheme)
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
updateBinding t s = instantiate s >>= generalize t

mymliEval :: Monad m => Term -> Mymli m Term
mymliEval t = do
  t'    <- mymliAddLetsForEval t
  store <- gets envStore
  let (v, store') = runState (bigStep t') store
  modify (\e -> e { envStore = store' })
  return v

mymliGc :: Monad m => Mymli m ()
mymliGc = do
  bindings <- gets envValueBindings
  store    <- gets envStore
  let store' = markSweepClear (Map.elems bindings) store
  modify (\e -> e { envStore = store' })
