module Myml.Eval
  ( eval
  , evalOnce
  , substitute
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
    (TmAbs x _ty t12) ->
      if isValue t2 then Just (substitute (M.singleton x t2) t12) else Nothing
    _ -> Nothing
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

substitute :: TmSub -> Term -> Term
substitute sub  (TmVar x     ) = fromMaybe (TmVar x) (M.lookup x sub)
substitute sub  (TmApp t1 t2 ) = TmApp (substitute sub t1) (substitute sub t2)
substitute sub  (TmAbs x ty t) = TmAbs x ty (substitute (M.delete x sub) t)
substitute _sub TmTrue         = TmTrue
substitute _sub TmFalse        = TmFalse
substitute sub (TmIf t1 t2 t3) =
  let s = substitute sub in TmIf (s t1) (s t2) (s t3)
substitute _sub TmZero       = TmZero
substitute sub  (TmSucc   t) = TmSucc (substitute sub t)
substitute sub  (TmPred   t) = TmPred (substitute sub t)
substitute sub  (TmIsZero t) = TmIsZero (substitute sub t)
substitute _sub TmUnit       = TmUnit
