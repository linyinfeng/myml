module Myml.Eval
  ( eval
  , evalOnce
  , applyTmSub
  , TmSub
  )
where

import           Myml.Syntax
import qualified Data.Map.Lazy                 as M
import           Control.Applicative
import           Data.Maybe

type TmSub = M.Map String Term

eval :: Term -> Term
eval t = maybe t eval (evalOnce t)

evalOnce :: Term -> Maybe Term
evalOnce (TmApp t1 t2) =
  (flip TmApp t2 <$> evalOnce t1) <|> (TmApp t1 <$> evalOnce t2) <|> case t1 of
    (TmAbs x _ty t12) | isValue t2 -> Just (applyTmSub (M.singleton x t2) t12)
    _                              -> Nothing
evalOnce TmAbs{}                = Nothing
evalOnce (TmVar _)              = Nothing
evalOnce TmTrue                 = Nothing
evalOnce TmFalse                = Nothing
evalOnce (TmIf TmTrue  t2  _t3) = Just t2
evalOnce (TmIf TmFalse _t2 t3 ) = Just t3
evalOnce (TmIf t1      t2  t3 ) = (\t1' -> TmIf t1' t2 t3) <$> evalOnce t1
evalOnce TmZero                 = Nothing
evalOnce (TmSucc t     )        = TmSucc <$> evalOnce t
evalOnce (TmPred TmZero)        = Just TmZero
evalOnce (TmPred t@(TmSucc t')) =
  if isNatValue t' then Just t' else TmPred <$> evalOnce t
evalOnce (TmPred   t     ) = TmPred <$> evalOnce t
evalOnce (TmIsZero TmZero) = Just TmTrue
evalOnce (TmIsZero t@(TmSucc t')) =
  if isNatValue t' then Just TmFalse else TmIsZero <$> evalOnce t
evalOnce (TmIsZero t) = TmIsZero <$> evalOnce t
evalOnce TmUnit       = Nothing

applyTmSub :: TmSub -> Term -> Term
applyTmSub sub  (TmVar x     ) = fromMaybe (TmVar x) (M.lookup x sub)
applyTmSub sub  (TmApp t1 t2 ) = TmApp (applyTmSub sub t1) (applyTmSub sub t2)
applyTmSub sub  (TmAbs x ty t) = TmAbs x ty (applyTmSub (M.delete x sub) t)
applyTmSub _sub TmTrue         = TmTrue
applyTmSub _sub TmFalse        = TmFalse
applyTmSub sub (TmIf t1 t2 t3) =
  let s = applyTmSub sub in TmIf (s t1) (s t2) (s t3)
applyTmSub _sub TmZero       = TmZero
applyTmSub sub  (TmSucc   t) = TmSucc (applyTmSub sub t)
applyTmSub sub  (TmPred   t) = TmPred (applyTmSub sub t)
applyTmSub sub  (TmIsZero t) = TmIsZero (applyTmSub sub t)
applyTmSub _sub TmUnit       = TmUnit
