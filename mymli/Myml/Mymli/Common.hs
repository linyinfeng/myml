module Myml.Mymli.Common
  ( mymliInferType
  , mymliAddLets
  , mymliEval
  )
where

import           Myml.Syntax
import           Myml.Typing
import           Myml.Eval
import           Myml.Mymli.Environment
import           Control.Monad.State
import qualified Data.Map                      as Map

mymliAddLets :: Monad m => Term -> Mymli m Term
mymliAddLets t = Map.foldrWithKey TmLet t <$> gets envBindings

mymliInferType :: Term -> Either TypingExcept TypeScheme
mymliInferType t =
  let (result, _) = runInference (infer t >>= generalize)
                                 emptyTypingEnv
                                 emptyInferenceState
  in  result

mymliEval :: Monad m => Term -> Mymli m Term
mymliEval t = do
  store <- gets envStore
  let (v, store') = runState (bigStep t) store
  modify (\e -> e { envStore = store' })
  return v

emptyTypingEnv :: TypingEnv
emptyTypingEnv = Map.empty

emptyInferenceState :: InferenceState
emptyInferenceState = InferenceState (NewVar Map.empty)
