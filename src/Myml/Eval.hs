{-# LANGUAGE LambdaCase #-}

module Myml.Eval
  ( EvalExcept(..)
  , SmallStepState
  , runSmallStepState
  , smallStep
  , bigStep
  , bigStepSafe
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

bigStepSafe
  :: Term -> (State (Maybe (Store (WithMark Term)))) (Either Error Term)
bigStepSafe t = do
  t' <- bigStep t
  return (if isValue t' then Right t' else Left (ErrEvalStuck t'))

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
  return (substTerm (Map.singleton x v2) t1)
smallStep (TmApp (TmApp (TmApp (TmMatchExtend l1) c) match') (TmApp (TmVariant l2) v2))
  | isValue c && isValue match' && isValue v2
  = if l1 == l2
    then return (TmApp c v2)
    else return (TmApp match' (TmApp (TmVariant l2) v2))
smallStep (TmApp (TmRcdAccess l2) (TmApp (TmApp (TmRcdExtend l1) v) rcd'))
  | isValue v = if l1 == l2
    then return v
    else return (TmApp (TmRcdAccess l2) rcd')
smallStep (TmApp TmRef v) | isValue v = do
  s <- get >>= maybeToExcept
  let (l, s') = allocate s v
  put (Just s')
  return (TmLoc l)
smallStep (TmApp TmDeref (TmLoc l)) =
  gets (fmap (lookupStore l)) >>= maybeToExcept >>= \case
    Nothing -> throwError ExcNoRuleApplied
    Just v  -> return v
smallStep (TmApp (TmApp TmAssign (TmLoc l)) v) | isValue v =
  gets (fmap (lookupStore l)) >>= maybeToExcept >>= \case
    Nothing -> throwError ExcNoRuleApplied
    Just _  -> TmUnit <$ modify (fmap (\s -> assign s l v))
smallStep (TmApp TmSucc (TmNat n)) = return (TmNat (succ n))
smallStep (TmApp TmPred (TmNat n)) =
  return (TmNat (if n == 0 then 0 else n - 1))
smallStep (TmApp TmIsZero (TmNat n)) =
  return (if n == 0 then TmTrue else TmFalse)
smallStep (TmApp v1 t2) | isValue v1 = TmApp v1 <$> smallStep t2
smallStep (TmApp t1 t2)              = flip TmApp t2 <$> smallStep t1
smallStep (TmLet x v1 t2) | isValue v1 =
  return (substTerm (Map.singleton x v1) t2)
smallStep (TmLet x t1 t2)            = (\t1' -> TmLet x t1' t2) <$> smallStep t1
smallStep (TmSeq v1 t2) | isValue v1 = return t2
smallStep (TmSeq t1 t2       )       = flip TmSeq t2 <$> smallStep t1
smallStep (TmIf TmTrue  t2 _ )       = return t2
smallStep (TmIf TmFalse _  t3)       = return t3
smallStep (TmIf t1      t2 t3)       = (\t1' -> TmIf t1' t2 t3) <$> smallStep t1
smallStep _                          = throwError ExcNoRuleApplied
