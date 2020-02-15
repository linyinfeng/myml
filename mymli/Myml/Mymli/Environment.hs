module Myml.Mymli.Environment
  ( RuntimeEnvironment(..)
  , initialRuntimeEnvironment
  )
where

import           Myml.Syntax
import           Myml.Eval.Store
import qualified Data.Map                      as Map

data RuntimeEnvironment = RuntimeEnvironment {
  envStore :: Store (WithMark Term),
  envBindings :: Map.Map VarName Term
}

initialRuntimeEnvironment :: RuntimeEnvironment
initialRuntimeEnvironment =
  RuntimeEnvironment { envStore = emptyStore, envBindings = Map.empty }
