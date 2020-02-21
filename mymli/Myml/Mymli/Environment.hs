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
import           Myml.Eval.Store
import           Control.Monad.State
import qualified Data.Map                      as Map

data MymliEnv = MymliEnv {
  envStore :: Store (WithMark Term),
  envValueBindings :: Map.Map VarName Term,
  envTypeBindings :: Map.Map VarName TypeScheme
}

emptyMymlEnv :: MymliEnv
emptyMymlEnv = MymliEnv { envStore         = emptyStore
                        , envValueBindings = Map.empty
                        , envTypeBindings  = Map.empty
                        }

type Mymli m = (StateT MymliEnv m)

runMymli :: Mymli m a -> MymliEnv -> m (a, MymliEnv)
runMymli = runStateT

execMymli :: Monad m => Mymli m a -> MymliEnv -> m MymliEnv
execMymli = execStateT

evalMymli :: Monad m => Mymli m a -> MymliEnv -> m a
evalMymli = evalStateT

data MymliRequest = MymliContinue
                  | MymliExit

mymliAddBinding :: Monad m => VarName -> Term -> TypeScheme -> Mymli m ()
mymliAddBinding x v ty = do
  vb <- gets envValueBindings
  tb <- gets envTypeBindings
  let vb' = Map.insert x v vb
      tb' = Map.insert x ty tb
  modify (\e -> e { envValueBindings = vb', envTypeBindings = tb' })
