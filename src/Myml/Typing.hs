{-# LANGUAGE RankNTypes #-}

module Myml.Typing
  ( unify
  )
where

import           Myml.Subst
import           Data.Equivalence.Monad

unify
  :: TypeSubstitutor
  -> TypeSubstitutor
  -> (forall s . EquivT s TypeSubstitutor TypeSubstitutor m ())
unify = error ""
