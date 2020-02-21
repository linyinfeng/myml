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
import           Myml.Subst
import           Myml.Eval.Store
import           Myml.Mymli.Environment
import           Control.Monad.State
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

mymliAddLetsForEval :: Monad m => Term -> Mymli m Term
mymliAddLetsForEval t = Map.foldrWithKey TmLet t <$> gets envValueBindings

mymliInferTypeAndUpdateBinding
  :: Monad m => Term -> Mymli m (Either TypingExcept TypeScheme)
mymliInferTypeAndUpdateBinding t = do
  tb <- gets envTypeBindings
  let (result, _) = runInference (inference tb t) tb emptyInferenceState
  case result of
    Left e -> return (Left e)
    Right (bindings', s) ->
      modify (\e -> e { envTypeBindings = bindings' }) >> return (Right s)
 where
  inference bindings ty = do
    ty'       <- infer ty -- maybe failed
    inferred  <- generalize ty'
    -- update bindings
    bindings' <- sequence (Map.map updateBinding bindings)
    return (bindings', inferred)

updateBinding :: TypeScheme -> Inference TypeScheme
updateBinding s = do
  let fv = freeVariable s
  sub <- sequence
    (Map.fromList
      [ (x, describeProper Set.empty (TyVar x)) | x <- Set.toList fv ]
    )
  return (applySubst (Map.map TySubProper sub) s)

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

emptyInferenceState :: InferenceState
emptyInferenceState = InferenceState (NewVar Map.empty)
