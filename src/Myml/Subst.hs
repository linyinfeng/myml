{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, LambdaCase #-}

module Myml.Subst
  ( Subst
  , ApplySubst(..)
  , compositeSubst
  , TypeSubstitutor(..)
  , normalizeRow
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
import           Data.Text.Prettyprint.Doc

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

runtimeKindMismatch :: Kind -> Kind -> a
runtimeKindMismatch required actual = error
  ("Kind mismatch, required: " ++ show (pretty required) ++ ", actual: " ++ show
    (pretty actual)
  )

handleBind
  :: (FreeVariable s, ApplySubst s s)
  => VarName
  -> (VarName -> s)
  -> SubstReader s (VarName, SubstReader s a -> SubstReader s a)
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
  subst (TmSeq t1 t2)         = TmSeq <$> subst t1 <*> subst t2
  subst TmTrue                = return TmTrue
  subst TmFalse               = return TmFalse
  subst (TmIf t1 t2 t3)       = TmIf <$> subst t1 <*> subst t2 <*> subst t3
  subst TmZero                = return TmZero
  subst (TmSucc t)            = TmSucc <$> subst t

instance ApplySubst Term TermCase where
  subst (TmCase x t) =
    handleBind x TmVar >>= \(newX, inner) -> TmCase newX <$> inner (subst t)

data TypeSubstitutor = TySubProper Type
                     | TySubPresenceWithType PresenceVarWithTypeInst
                     | TySubPresence TypePresence
                     | TySubRow TypeRow
                     deriving (Show, Eq)

instance FreeVariable TypeSubstitutor where
  freeVariable (TySubProper           t) = freeVariable t
  freeVariable (TySubPresenceWithType i) = freeVariable i
  freeVariable (TySubPresence         p) = freeVariable p
  freeVariable (TySubRow              r) = freeVariable r

instance ApplySubst TypeSubstitutor TypeSubstitutor where
  subst (TySubProper           t) = TySubProper <$> subst t
  subst (TySubPresenceWithType i) = TySubPresenceWithType <$> subst i
  subst (TySubPresence         p) = TySubPresence <$> subst p
  subst (TySubRow              r) = TySubRow <$> subst r

instance ApplySubst TypeSubstitutor Type where
  subst (TyVar x) =
    (\case
        Nothing              -> (TyVar x)
        Just (TySubProper t) -> t
        Just (TySubPresenceWithType _) ->
          runtimeKindMismatch KProper properArrowPresence
        Just (TySubPresence _) -> runtimeKindMismatch KProper KPresence
        Just (TySubRow      _) -> runtimeKindMismatch KProper rowKind
      )
      <$> asks (Map.lookup x)
  subst (TyArrow t1 t2) = TyArrow <$> subst t1 <*> subst t2
  subst (TyRecord  row) = TyRecord <$> subst row
  subst (TyVariant row) = TyVariant <$> subst row
  subst (TyMu x t     ) = handleBind x (TySubProper . TyVar)
    >>= \(newX, inner) -> TyMu newX <$> inner (subst t)
  subst (TyRef t) = TyRef <$> subst t
  subst TyUnit    = return TyUnit
  subst TyBool    = return TyBool
  subst TyNat     = return TyNat

instance ApplySubst TypeSubstitutor TypeRow where
  subst (TyRow f cof) = do
    f1            <- sequence (Map.map subst f)
    TyRow f2 cof' <- mergeRow
    return (normalizeRow (TyRow (Map.unionWithKey duplicateError f1 f2) cof'))
   where
    mergeRow = case cof of
      CofAllAbsent -> return (TyRow Map.empty CofAllAbsent)
      CofRowVar x ->
        (\case
            Nothing              -> TyRow Map.empty (CofRowVar x)
            Just (TySubProper _) -> runtimeKindMismatch rowKind KProper
            Just (TySubPresenceWithType _) ->
              runtimeKindMismatch rowKind properArrowPresence
            Just (TySubPresence _  ) -> runtimeKindMismatch rowKind KPresence
            Just (TySubRow      row) -> row
          )
          <$> asks (Map.lookup x)
    duplicateError l _ _ =
      runtimeKindMismatch (KRow (Set.fromList [l, "..."])) rowKind

instance ApplySubst TypeSubstitutor TypePresence where
  subst Absent      = return Absent
  subst (Present t) = Present <$> subst t
  subst (PresenceVarWithType x t) =
    asks (Map.lookup x)
      >>= (\case
            Nothing -> PresenceVarWithType x <$> subst t
            Just (TySubProper _) ->
              runtimeKindMismatch properArrowPresence KProper
            Just (TySubPresenceWithType p) -> case p of
              PresenceWithTypeInstAbsent  -> return Absent
              PresenceWithTypeInstPresent -> Present <$> subst t
              PresenceWithTypeInstVar x'  -> PresenceVarWithType x' <$> subst t
            Just (TySubPresence _) ->
              runtimeKindMismatch properArrowPresence KPresence
            Just (TySubRow _) ->
              runtimeKindMismatch properArrowPresence rowKind
          )
  subst (PresenceVar x) =
    (\case
        Nothing              -> PresenceVar x
        Just (TySubProper _) -> runtimeKindMismatch KPresence KProper
        Just (TySubPresenceWithType _) ->
          runtimeKindMismatch KPresence properArrowPresence
        Just (TySubPresence p) -> p
        Just (TySubRow      _) -> runtimeKindMismatch KPresence rowKind
      )
      <$> asks (Map.lookup x)

instance ApplySubst TypeSubstitutor PresenceVarWithTypeInst where
  subst PresenceWithTypeInstAbsent  = return PresenceWithTypeInstAbsent
  subst PresenceWithTypeInstPresent = return PresenceWithTypeInstPresent
  subst (PresenceWithTypeInstVar x) =
    (\case
        Nothing -> PresenceWithTypeInstVar x
        Just (TySubProper _) -> runtimeKindMismatch properArrowPresence KProper
        Just (TySubPresenceWithType i) -> i
        Just (TySubPresence _) ->
          runtimeKindMismatch properArrowPresence KPresence
        Just (TySubRow _) -> runtimeKindMismatch properArrowPresence rowKind
      )
      <$> asks (Map.lookup x)

normalizeRow :: TypeRow -> TypeRow
normalizeRow (TyRow f cof) = case cof of
  CofAllAbsent -> TyRow (Map.filter (Absent /=) f) cof -- absorb absent to all absent
  CofRowVar _  -> TyRow f cof -- unchanged

rowKind :: Kind
rowKind = KRow (Set.singleton "...")

properArrowPresence :: Kind
properArrowPresence = KArrow KProper KPresence
