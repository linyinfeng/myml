{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Myml.Subst
  ( Subst
  , ApplySubst(..)
  , compositeSubst
  )
where

import           Myml.Syntax
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Control.Monad.Reader
import           Data.Maybe                     ( fromMaybe
                                                , fromJust
                                                )
import           Data.List                      ( find )

type Subst a = Map.Map VarName a

infixl 6 `compositeSubst`
compositeSubst :: ApplySubst a a => Subst a -> Subst a -> Subst a
compositeSubst sa sb = Map.map (applySubst sa) sb `Map.union` sa

type SubstReader s a = Reader (Subst s) a

infixl 7 `applySubst`
class ApplySubst a b where
  subst :: b -> SubstReader a b
  applySubst :: Subst a -> b -> b
  applySubst s t = runReader (subst t) s
  {-# MINIMAL subst #-}

hide :: VarName -> SubstReader s a -> SubstReader s a
hide x = local (Map.delete x)

substFreeVariables :: FreeVariable a => Subst a -> Set.Set VarName
substFreeVariables = Map.foldl (\a b -> a `Set.union` freeVariable b) Set.empty

handleBind
  :: (FreeVariable s, ApplySubst s s)
  => VarName
  -> (VarName -> s)
  -> SubstReader s (VarName, (SubstReader s a -> SubstReader s a))
handleBind x cons = do
  m <- ask
  let fvS = substFreeVariables m
  return
    (if x `Set.notMember` fvS
      then (x, hide x)
      else
        let newX = uniqueName fvS x
        in  (newX, local (`compositeSubst` Map.singleton x (cons newX)))
    )

uniqueName :: Set.Set VarName -> VarName -> VarName
uniqueName sv base = fromJust
  (find (`Set.notMember` sv) (map (\i -> base ++ show i) [(0 :: Integer) ..]))

instance ApplySubst Term Term where
  subst (TmAbs x t) =
    handleBind x TmVar >>= \(newX, inner) -> TmAbs newX <$> inner (subst t)
  subst (TmApp t1 t2) = TmApp <$> subst t1 <*> subst t2
  subst (TmVar x) = ask >>= \s -> return (fromMaybe (TmVar x) (Map.lookup x s))
  subst (TmLet x t1 t2) = do
    t1'           <- subst t1
    (newX, inner) <- handleBind x TmVar
    TmLet newX t1' <$> inner (subst t2)
  subst (TmRcd m            ) = TmRcd <$> sequence (Map.map subst m)
  subst (TmRcdExtend t1 l t2) = flip TmRcdExtend l <$> subst t1 <*> subst t2
  subst (TmRcdAccess t l    ) = flip TmRcdAccess l <$> subst t
  subst (TmMatch m          ) = TmMatch <$> sequence (Map.map subst m)
  subst (TmMatchExtend t l c) = flip TmMatchExtend l <$> subst t <*> subst c
  subst (TmVariant l t      ) = TmVariant l <$> subst t
  subst (TmRef   t          ) = TmRef <$> subst t
  subst (TmDeref t          ) = TmDeref <$> subst t
  subst (TmAssign t1 t2     ) = TmAssign <$> subst t1 <*> subst t2
  subst (TmLoc n            ) = return (TmLoc n)
  subst TmUnit                = return TmUnit
  subst TmTrue                = return TmTrue
  subst TmFalse               = return TmFalse
  subst (TmIf t1 t2 t3)       = TmIf <$> subst t1 <*> subst t2 <*> subst t3
  subst TmZero                = return TmZero
  subst (TmSucc t)            = TmSucc <$> subst t

instance ApplySubst Term TermCase where
  subst (TmCase x t) =
    handleBind x TmVar >>= \(newX, inner) -> TmCase newX <$> inner (subst t)
