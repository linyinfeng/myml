module Myml.Mymli.Environment
  ( MymliEnv (..),
    emptyMymlEnv,
    emptyEnvStore,
    emptyInferenceState,
    Mymli,
    runMymli,
    execMymli,
    evalMymli,
    MymliRequest (..),
    mymliAddBinding,
  )
where

import Control.Monad.State
import qualified Data.Map as Map
import Myml.Eval.Store
import Myml.Mymli.Option
import Myml.Syntax
import Myml.Typing

data MymliEnv = MymliEnv
  { envOption :: MymliOptions,
    envStore :: Maybe (Store (WithMark Term)),
    envTermBindings :: Map.Map VarName Term,
    envValueBindings :: Map.Map VarName Term,
    envTypeBindings :: Map.Map VarName TypeScheme,
    envInferState :: InferenceState,
    envSearchPath :: [FilePath]
  }

emptyMymlEnv :: MymliOptions -> MymliEnv
emptyMymlEnv opt =
  MymliEnv
    { envOption = opt,
      envStore = emptyEnvStore opt,
      envTermBindings = Map.empty,
      envValueBindings = Map.empty,
      envTypeBindings = Map.empty,
      envInferState = emptyInferenceState opt,
      envSearchPath = [".", ""]
    }

emptyEnvStore :: MymliOptions -> Maybe (Store (WithMark Term))
emptyEnvStore opt = if optPure opt then Nothing else Just emptyStore

emptyInferenceState :: MymliOptions -> InferenceState
emptyInferenceState opt = InferenceState (NewVar Map.empty) (not (optPure opt))

type Mymli m = (StateT MymliEnv m)

runMymli :: Mymli m a -> MymliEnv -> m (a, MymliEnv)
runMymli = runStateT

execMymli :: (Monad m) => Mymli m a -> MymliEnv -> m MymliEnv
execMymli = execStateT

evalMymli :: (Monad m) => Mymli m a -> MymliEnv -> m a
evalMymli = evalStateT

data MymliRequest
  = MymliContinue
  | MymliExit

mymliAddBinding ::
  (Monad m) => VarName -> Term -> Term -> TypeScheme -> Mymli m ()
mymliAddBinding x t v ty = do
  termBindings <- gets envTermBindings
  valueBindings <- gets envValueBindings
  typeBindings <- gets envTypeBindings
  let termBindings' = Map.insert x t termBindings
      valueBindings' = Map.insert x v valueBindings
      typeBindings' = Map.insert x ty typeBindings
  modify
    ( \e ->
        e
          { envTermBindings = termBindings',
            envValueBindings = valueBindings',
            envTypeBindings = typeBindings'
          }
    )
