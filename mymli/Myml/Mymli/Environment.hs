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
  envBindings :: Map.Map VarName Term
}

emptyMymlEnv :: MymliEnv
emptyMymlEnv = MymliEnv { envStore = emptyStore, envBindings = Map.empty }

type Mymli m = (StateT MymliEnv m)

runMymli :: Mymli m a -> MymliEnv -> m (a, MymliEnv)
runMymli = runStateT

execMymli :: Monad m => Mymli m a -> MymliEnv -> m MymliEnv
execMymli = execStateT

evalMymli :: Monad m => Mymli m a -> MymliEnv -> m a
evalMymli = evalStateT

data MymliRequest = MymliContinue
                  | MymliExit

mymliAddBinding :: Monad m => VarName -> Term -> Mymli m ()
mymliAddBinding x t = do
  bindings <- gets envBindings
  let bindings' = Map.insert x t bindings
  modify (\e -> e { envBindings = bindings' })
