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

bigStep :: Term -> (StateT (Maybe (Store (WithMark Term))) IO) Term
bigStep t = runExceptT (smallStep t) >>= \case
  Left  ExcNoRuleApplied -> return t
  Right t'               -> bigStep t'

bigStepSafe
  :: Term -> (StateT (Maybe (Store (WithMark Term))) IO) (Either Error Term)
bigStepSafe t = do
  t' <- bigStep t
  return (if isValue t' then Right t' else Left (ErrEvalStuck t'))

type SmallStepState
  = ExceptT EvalExcept (StateT (Maybe (Store (WithMark Term))) IO) Term

runSmallStepState
  :: SmallStepState
  -> Maybe (Store (WithMark Term))
  -> IO (Either EvalExcept Term, Maybe (Store (WithMark Term)))
runSmallStepState s = runStateT (runExceptT s)

maybeToExcept :: (Monad m) => Maybe a -> ExceptT EvalExcept m a
maybeToExcept m = case m of
  Just x  -> return x
  Nothing -> throwError ExcNoRuleApplied

smallStep :: Term -> SmallStepState
smallStep (TmApp (TmAbs x t1) v2) | isValue v2 =
  return (substTerm (Map.singleton x v2) t1)
smallStep (TmApp (TmApp (TmApp (TmMatchExtend l1) v1) match') (TmApp (TmVariant l2) v2))
  | isValue v1 && isValue match' && isValue v2
  = if l1 == l2
    then return (TmApp v1 v2)
    else return (TmApp match' (TmApp (TmVariant l2) v2))
smallStep (TmApp (TmApp (TmMatchUpdate l1) v1) (TmApp (TmApp (TmMatchExtend l2) v2) match'))
  | isValue v1 && isValue v2
  = if l1 == l2
    then return (TmApp (TmApp (TmMatchExtend l2) v1) match')
    else return
      (TmApp (TmApp (TmMatchExtend l2) v2)
             (TmApp (TmApp (TmMatchUpdate l1) v1) match')
      )
smallStep (TmApp (TmRcdAccess l1) (TmApp (TmApp (TmRcdExtend l2) v) rcd'))
  | isValue v = if l1 == l2
    then return v
    else return (TmApp (TmRcdAccess l1) rcd')
smallStep (TmApp (TmApp (TmRcdUpdate l1) v1) (TmApp (TmApp (TmRcdExtend l2) v2) rcd'))
  | isValue v1 && isValue v2
  = if l1 == l2
    then return (TmApp (TmApp (TmRcdExtend l2) v1) rcd')
    else return
      (TmApp (TmApp (TmRcdExtend l2) v2)
             (TmApp (TmApp (TmRcdUpdate l1) v1) rcd')
      )
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
smallStep (TmApp (TmApp TmIntegerPlus (TmInteger n)) (TmInteger m)) =
  return (TmInteger (n + m))
smallStep (TmApp (TmApp TmIntegerMul (TmInteger n)) (TmInteger m)) =
  return (TmInteger (n * m))
smallStep (TmApp TmIntegerAbs (TmInteger n)) = return (TmInteger (abs n))
smallStep (TmApp TmIntegerSignum (TmInteger n)) = return (TmInteger (signum n))
smallStep (TmApp TmIntegerNegate (TmInteger n)) = return (TmInteger (negate n))
smallStep (TmApp (TmApp TmIntegerQuotRem (TmInteger n)) (TmInteger m)) = return
  (recordLiteral [("quot", TmInteger q), ("rem", TmInteger r)])
  where (q, r) = quotRem n m
smallStep (TmApp (TmApp TmIntegerCompare (TmInteger n)) (TmInteger m)) =
  return (orderingToTmOrdering (compare n m))
smallStep (TmApp TmIOPutChar (TmChar c)) = liftIO (TmUnit <$ putChar c)
smallStep (TmApp TmIOGetChar TmUnit    ) = liftIO (TmChar <$> getChar)
smallStep (TmApp (TmApp TmCharCompare (TmChar c1)) (TmChar c2)) =
  return (orderingToTmOrdering (compare c1 c2))
smallStep (TmApp v1 t2) | isValue v1 = TmApp v1 <$> smallStep t2
smallStep (TmApp t1 t2)              = flip TmApp t2 <$> smallStep t1
smallStep (TmLet x v1 t2) | isValue v1 =
  return (substTerm (Map.singleton x v1) t2)
smallStep (TmLet x t1 t2) = (\t1' -> TmLet x t1' t2) <$> smallStep t1
smallStep _               = throwError ExcNoRuleApplied

orderingToTmOrdering :: Ordering -> Term
orderingToTmOrdering o = TmApp (TmVariant label) TmUnit
 where
  label = case o of
    LT -> "LT"
    EQ -> "EQ"
    GT -> "GT"
