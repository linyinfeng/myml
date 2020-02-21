module Myml.Mymli.Environment
  ( MymliEnv(..)
  , emptyMymlEnv
  , Mymli
  , runMymli
  , execMymli
  , evalMymli
  , MymliRequest(..)
  , mymliAddBinding
  )
where

import           Myml.Syntax
import           Myml.Typing
import           Myml.Eval.Store
import           Control.Monad.State
import qualified Data.Map                      as Map

data MymliEnv = MymliEnv {
  envStore :: Store (WithMark Term),
  envTermBindings :: Map.Map VarName Term,
  envValueBindings :: Map.Map VarName Term,
  envTypeBindings :: Map.Map VarName TypeScheme,
  envInferState :: InferenceState
}

emptyMymlEnv :: MymliEnv
emptyMymlEnv = MymliEnv { envStore         = emptyStore
                        , envTermBindings  = Map.empty
                        , envValueBindings = Map.empty
                        , envTypeBindings  = Map.empty
                        , envInferState    = emptyInferenceState
                        }

emptyInferenceState :: InferenceState
emptyInferenceState = InferenceState (NewVar Map.empty)

type Mymli m = (StateT MymliEnv m)

runMymli :: Mymli m a -> MymliEnv -> m (a, MymliEnv)
runMymli = runStateT

execMymli :: Monad m => Mymli m a -> MymliEnv -> m MymliEnv
execMymli = execStateT

evalMymli :: Monad m => Mymli m a -> MymliEnv -> m a
evalMymli = evalStateT

data MymliRequest = MymliContinue
                  | MymliExit

mymliAddBinding
  :: Monad m => VarName -> Term -> Term -> TypeScheme -> Mymli m ()
mymliAddBinding x t v ty = do
  termBindings  <- gets envTermBindings
  valueBindings <- gets envValueBindings
  typeBindings  <- gets envTypeBindings
  let termBindings'  = Map.insert x t termBindings
      valueBindings' = Map.insert x v valueBindings
      typeBindings'  = Map.insert x ty typeBindings
  modify
    (\e -> e { envTermBindings  = termBindings'
             , envValueBindings = valueBindings'
             , envTypeBindings  = typeBindings'
             }
    )
