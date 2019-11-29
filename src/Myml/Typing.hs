module Myml.Typing
  ( TyAssum
  , TySub
  , TyCtx
  , TyCons
  , applyTySub
  , applyTySubToCons
  , applyTySubToTerm
  , compositeTySub
  , tyFV
  , lookupTyCtx
  , freeTyVar
  , genTyCons
  , unifyCons
  )
where

import           Myml.Syntax
import qualified Data.Map.Lazy                 as M
import qualified Data.Set                      as S
import           Data.List
import           Data.Maybe
import           Text.Printf

type TySub = M.Map String Type
data TyAssum = TyAssum String Type deriving (Eq, Ord, Show)
type TyCtx = [TyAssum]
data TyCons = TyEqual Type Type deriving (Eq, Ord, Show)

applyTySub :: TySub -> Type -> Type
applyTySub _sub TyBool            = TyBool
applyTySub _sub TyNat             = TyNat
applyTySub _sub TyUnit            = TyUnit
applyTySub sub (TyFunc t1 t2) = TyFunc (applyTySub sub t1) (applyTySub sub t2)
applyTySub sub  ty@(TyVar name  ) = fromMaybe ty (M.lookup name sub)

applyTySubToCons :: TySub -> TyCons -> TyCons
applyTySubToCons sub (TyEqual t1 t2) = TyEqual (apply t1) (apply t2)
  where apply = applyTySub sub

applyTySubToTerm :: TySub -> Term -> Term
applyTySubToTerm _sub t@(TmVar _    ) = t
applyTySubToTerm sub  (  TmApp t1 t2) = TmApp (apply t1) (apply t2)
  where apply = applyTySubToTerm sub
applyTySubToTerm sub (TmAbs x ty t) =
  TmAbs x (applyTySub sub ty) (applyTySubToTerm sub t)
applyTySubToTerm _sub TmTrue          = TmTrue
applyTySubToTerm _sub TmFalse         = TmFalse
applyTySubToTerm sub  (TmIf t1 t2 t3) = TmIf (apply t1) (apply t2) (apply t3)
  where apply = applyTySubToTerm sub
applyTySubToTerm _sub TmZero       = TmZero
applyTySubToTerm sub  (TmSucc   t) = TmSucc (applyTySubToTerm sub t)
applyTySubToTerm sub  (TmPred   t) = TmPred (applyTySubToTerm sub t)
applyTySubToTerm sub  (TmIsZero t) = TmIsZero (applyTySubToTerm sub t)
applyTySubToTerm _sub TmUnit       = TmUnit

compositeTySub :: TySub -> TySub -> TySub
compositeTySub s1 s2 = M.union (M.map (applyTySub s1) s2) (M.difference s1 s2)

tyFV :: Type -> S.Set String
tyFV TyBool         = S.empty
tyFV TyNat          = S.empty
tyFV TyUnit         = S.empty
tyFV (TyFunc t1 t2) = S.union (tyFV t1) (tyFV t2)
tyFV (TyVar name  ) = S.singleton name

lookupTyCtx :: String -> TyCtx -> Maybe Type
lookupTyCtx name ctx =
  (\(TyAssum _ ty) -> ty) <$> find (\(TyAssum x _) -> x == name) ctx

freeTyVar :: [String]
freeTyVar = [ printf "?X%d" i | i <- [(1 :: Integer) ..] ]

data TyError = TyErrorVarNotInCtx String
             | TyErrorNoSolution Type Type

instance Show TyError where
  show (TyErrorVarNotInCtx name) =
    printf "variable %s is not in typing context" name
  show (TyErrorNoSolution ty1 ty2) =
    printf "no solution for constraint %s = %s" (show ty1) (show ty2)

genTyCons
  :: TyCtx -> [String] -> Term -> Either TyError (Type, [String], S.Set TyCons)
genTyCons ctx f (TmVar name) = case lookupTyCtx name ctx of
  Nothing -> Left $ TyErrorVarNotInCtx name
  Just ty -> Right (ty, f, S.empty)
genTyCons ctx f (TmApp t1 t2) = do
  (ty1, f1, c1) <- genTyCons ctx f t1
  (ty2, f2, c2) <- genTyCons ctx f1 t2
  let (x : f3) = f2
  let tyx      = TyVar x
  let c' = S.insert (TyEqual ty1 (TyFunc ty2 tyx)) (S.union c1 c2)
  return (tyx, f3, c')
genTyCons ctx f (TmAbs x ty t) = do
  let innerCtx = TyAssum x ty : ctx
  (tyt, f', c) <- genTyCons innerCtx f t
  return (TyFunc ty tyt, f', c)
genTyCons _ctx f TmTrue          = Right (TyBool, f, S.empty)
genTyCons _ctx f TmFalse         = Right (TyBool, f, S.empty)
genTyCons ctx  f (TmIf t1 t2 t3) = do
  (ty1, f1, c1) <- genTyCons ctx f t1
  (ty2, f2, c2) <- genTyCons ctx f1 t2
  (ty3, f3, c3) <- genTyCons ctx f2 t3
  let c' =
        S.insert (TyEqual ty1 TyBool) $ S.insert (TyEqual ty2 ty3) $ S.unions
          [c1, c2, c3]
  return (ty2, f3, c')
genTyCons _ctx f TmZero     = Right (TyNat, f, S.empty)
genTyCons ctx  f (TmSucc t) = do
  (tyt, f1, c) <- genTyCons ctx f t
  let c' = S.insert (TyEqual tyt TyNat) c
  return (TyNat, f1, c')
genTyCons ctx f (TmPred   t) = genTyCons ctx f (TmSucc t)  -- Same as TmSucc case
genTyCons ctx f (TmIsZero t) = do
  (tyt, f1, c) <- genTyCons ctx f t
  let c' = S.insert (TyEqual tyt TyNat) c
  return (TyBool, f1, c')
genTyCons _ctx f TmUnit = Right (TyUnit, f, S.empty)

unifyCons :: S.Set TyCons -> Either TyError TySub
unifyCons cs = case S.minView cs of
  Nothing -> Right M.empty
  Just ((TyEqual ty1 ty2), cs') | ty1 == ty2 -> unifyCons cs'
  Just ((TyEqual (TyVar x) ty2), cs') | S.notMember x (tyFV ty2) -> do
    let sub = M.singleton x ty2
    subs <- unifyCons (S.map (applyTySubToCons sub) cs')
    return (compositeTySub subs sub)
  Just ((TyEqual ty1 (TyVar x)), cs') | S.notMember x (tyFV ty1) -> do
    let sub = M.singleton x ty1
    subs <- unifyCons (S.map (applyTySubToCons sub) cs')
    return (compositeTySub subs sub)
  Just ((TyEqual (TyFunc t1 t2) (TyFunc s1 s2)), cs') ->
    unifyCons (S.insert (TyEqual t1 s1) $ S.insert (TyEqual t2 s2) cs')
  Just ((TyEqual ty1 ty2), _cs') -> Left $ TyErrorNoSolution ty1 ty2
