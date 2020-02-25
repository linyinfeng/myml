{-# LANGUAGE RankNTypes #-}
module Myml.Mymli.Common
  ( prompt
  , inputCmdPrompt
  , parseAndPrintError
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
import           Text.Trifecta
import           Control.Monad.State
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

prompt :: String
prompt = "\ESC[1;32m\STXmymli\ESC[0m\STX> "

inputCmdPrompt :: String
inputCmdPrompt = "input| "

parseAndPrintError :: Parser a -> String -> IO (Maybe a)
parseAndPrintError parser input = case runParser parser mempty input of
                                Failure (ErrInfo d _) -> print d >> return Nothing
                                Success res -> return (Just res)

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
updateBinding t s =
  let fv = freeVariable s
  in  if Set.null fv then return s else instantiate s >>= generalize t

mymliSubstEnv :: Monad m => Term -> Mymli m Term
mymliSubstEnv t = flip applySubst t <$> gets envValueBindings

mymliEval :: Monad m => Term -> Mymli m Term
mymliEval t = do
  t'    <- mymliSubstEnv t
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
