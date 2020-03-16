{-# LANGUAGE LambdaCase #-}

module Myml.Eval
  ( EvalExcept(..)
  , SmallStepState
  , runSmallStepState
  , smallStep
  , bigStep
  )
where

import           Myml.Syntax
import           Myml.Eval.Store
import           Myml.Subst
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map                      as Map

data EvalExcept = ExcNoRuleApplied
  deriving (Show, Eq)

bigStep :: Term -> (State (Maybe (Store (WithMark Term)))) Term
bigStep t = runExceptT (smallStep t) >>= \case
  Left  ExcNoRuleApplied -> return t
  Right t'               -> bigStep t'

type SmallStepState
  = ExceptT EvalExcept (State (Maybe (Store (WithMark Term)))) Term

runSmallStepState
  :: SmallStepState
  -> Maybe (Store (WithMark Term))
  -> (Either EvalExcept Term, Maybe (Store (WithMark Term)))
runSmallStepState s = runState (runExceptT s)

maybeToExcept :: (Monad m) => Maybe a -> ExceptT EvalExcept m a
maybeToExcept m = case m of
  Just x  -> return x
  Nothing -> throwError ExcNoRuleApplied

smallStep :: Term -> SmallStepState
smallStep (TmApp (TmAbs x t1) v2) | isValue v2 =
  return (applySubst (Map.singleton x v2) t1)
smallStep (TmApp (TmMatch m) (TmVariant l v2)) | isValue v2 =
  case Map.lookup l m of
    Nothing            -> throwError ExcNoRuleApplied
    Just (TmCase x t1) -> return (applySubst (Map.singleton x v2) t1)
smallStep (TmApp TmRef v) | isValue v = do
  s <- get >>= maybeToExcept
  let (l, s') = allocate s v
  put (Just s')
  return (TmLoc l)
smallStep (TmApp TmDeref (TmLoc l)) =
  gets (fmap (lookupStore l)) >>= maybeToExcept >>= \case
    Nothing -> throwError ExcNoRuleApplied
    Just v  -> return v
smallStep (TmApp TmSucc (TmNat n)) = return (TmNat (succ n))
smallStep (TmApp TmPred (TmNat n)) =
  return (TmNat (if n == 0 then 0 else n - 1))
smallStep (TmApp TmIsZero (TmNat n)) =
  return (if n == 0 then TmTrue else TmFalse)
smallStep (TmApp v1 t2) | isValue v1 = TmApp v1 <$> smallStep t2
smallStep (TmApp t1 t2)              = flip TmApp t2 <$> smallStep t1
smallStep (TmLet x v1 t2) | isValue v1 =
  return (applySubst (Map.singleton x v1) t2)
smallStep (TmLet x t1 t2) = (\t1' -> TmLet x t1' t2) <$> smallStep t1
smallStep (TmRcd m      ) = if Map.null notValue
  then throwError ExcNoRuleApplied
  else
    let (minLabel, minTerm) = Map.findMin notValue
    in  (\minTerm' -> TmRcd (Map.insert minLabel minTerm' m))
          <$> smallStep minTerm
  where notValue = Map.filter (not . isValue) m
smallStep (TmRcdExtend v1@(TmRcd m) l v2) | isValue v1 && isValue v2 =
  return (TmRcd (Map.insert l v2 m))
smallStep (TmRcdExtend v1 l t2) | isValue v1 = TmRcdExtend v1 l <$> smallStep t2
smallStep (TmRcdExtend t1 l t2) =
  (\t1' -> TmRcdExtend t1' l t2) <$> smallStep t1
smallStep (TmRcdAccess (TmRcd m) l) = case Map.lookup l m of
  Nothing    -> throwError ExcNoRuleApplied
  Just inner -> return inner
smallStep (TmRcdAccess t l              ) = flip TmRcdAccess l <$> smallStep t
smallStep (TmMatchExtend (TmMatch m) l c) = return (TmMatch (Map.insert l c m))
smallStep (TmMatchExtend t l c) = (\t' -> TmMatchExtend t' l c) <$> smallStep t
smallStep (TmVariant l t                ) = TmVariant l <$> smallStep t
smallStep (TmAssign (TmLoc l) v) | isValue v =
  gets (fmap (lookupStore l)) >>= maybeToExcept >>= \case
    Nothing -> throwError ExcNoRuleApplied
    Just _  -> TmUnit <$ modify (fmap (\s -> assign s l v))
smallStep (TmAssign v1 t2) | isValue v1 = TmAssign v1 <$> smallStep t2
smallStep (TmAssign t1 t2)              = flip TmAssign t2 <$> smallStep t1
smallStep (TmSeq v1 t2) | isValue v1    = return t2
smallStep (TmSeq t1 t2       )          = flip TmSeq t2 <$> smallStep t1
smallStep (TmIf TmTrue  t2 _ )          = return t2
smallStep (TmIf TmFalse _  t3)          = return t3
smallStep (TmIf t1 t2 t3) = (\t1' -> TmIf t1' t2 t3) <$> smallStep t1
smallStep _                             = throwError ExcNoRuleApplied
