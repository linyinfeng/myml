module Myml.Eval
  ( eval
  , evalOnce
  , applyTmSub
  , TmSub
  )
where

import           Myml.Syntax
import qualified Data.Map.Lazy                 as M
import           Data.Maybe

type TmSub = M.Map String Term

eval :: Term -> Term
eval t = maybe t eval (evalOnce t)

evalOnce :: Term -> Maybe Term
evalOnce (TmApp (TmAbs x _ty t12) v2) | isValue v2 =
  Just (applyTmSub (M.singleton x v2) t12)
evalOnce (TmApp v1 t2) | isValue v1 = TmApp v1 <$> evalOnce t2
evalOnce (TmApp t1 t2)              = flip TmApp t2 <$> evalOnce t1
evalOnce TmAbs{}                    = Nothing
evalOnce (TmVar _)                  = Nothing
evalOnce (TmLetIn x t1 t2) | isValue t1 =
  Just $ applyTmSub (M.singleton x t1) t2
evalOnce (TmLetIn x t1 t2) = (\t1' -> TmLetIn x t1' t2) <$> evalOnce t1
evalOnce TmTrue                               = Nothing
evalOnce TmFalse                              = Nothing
evalOnce (TmIf TmTrue  t2  _t3)               = Just t2
evalOnce (TmIf TmFalse _t2 t3 )               = Just t3
evalOnce (TmIf t1 t2 t3) = (\t1' -> TmIf t1' t2 t3) <$> evalOnce t1
evalOnce TmZero                               = Nothing
evalOnce (TmSucc t     )                      = TmSucc <$> evalOnce t
evalOnce (TmPred TmZero)                      = Just TmZero
evalOnce (TmPred (TmSucc v)) | isNatValue v   = Just v
evalOnce (TmPred   t     )                    = TmPred <$> evalOnce t
evalOnce (TmIsZero TmZero)                    = Just TmTrue
evalOnce (TmIsZero (TmSucc v)) | isNatValue v = Just TmFalse
evalOnce (TmIsZero t)                         = TmIsZero <$> evalOnce t
evalOnce TmUnit                               = Nothing

applyTmSub :: TmSub -> Term -> Term
applyTmSub sub (TmVar x     ) = fromMaybe (TmVar x) (M.lookup x sub)
applyTmSub sub (TmApp t1 t2 ) = TmApp (applyTmSub sub t1) (applyTmSub sub t2)
applyTmSub sub (TmAbs x ty t) = TmAbs x ty (applyTmSub (M.delete x sub) t)
applyTmSub sub (TmLetIn x t1 t2) =
  TmLetIn x (applyTmSub sub t1) (applyTmSub (M.delete x sub) t2)
applyTmSub _sub TmTrue  = TmTrue
applyTmSub _sub TmFalse = TmFalse
applyTmSub sub (TmIf t1 t2 t3) =
  let s = applyTmSub sub in TmIf (s t1) (s t2) (s t3)
applyTmSub _sub TmZero       = TmZero
applyTmSub sub  (TmSucc   t) = TmSucc (applyTmSub sub t)
applyTmSub sub  (TmPred   t) = TmPred (applyTmSub sub t)
applyTmSub sub  (TmIsZero t) = TmIsZero (applyTmSub sub t)
applyTmSub _sub TmUnit       = TmUnit
