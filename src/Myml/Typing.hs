{-# LANGUAGE LambdaCase, RankNTypes #-}

module Myml.Typing
  ( TypingExcept(..)
  , NewVar(..)
  , TypingEnv
  , Constraint
  , Inference
  , InferenceState(..)
  , newVar
  , runInference
  , infer
  , instantiate
  , instantiateType
  , instantiateRow
  , instantiatePresence
  , generalize
  , unifyProper
  , unifyPresenceWithType
  , unifyPresence
  , unifyRow
  , describeProper
  , describePresence
  , describeRow
  , regTreeEq
  , regTreeEqRow
  , regTreeEqPresence
  , regTreeEq'
  , regTreeNeq'
  )
where

import           Myml.Syntax
import           Myml.Subst
import           Data.Equivalence.Monad
import           Data.Maybe
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Data.Text.Prettyprint.Doc
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

data TypingExcept = ExcUnifyKindMismatch Kind Kind
                  | ExcUnifyNoRuleApplied TypeSubstitutor TypeSubstitutor
                  | ExcUnifyRowLabelCollided (Set.Set LabelName)
                  | ExcUnboundedVariable VarName
                  | ExcFreeVarKindConflict VarName Kind Kind
                  | ExcStoreTypingNotImplemented
                  | ExcDescribeNoRuleApplied TypeSubstitutor
                  | ExcCanNotHandleCofMu TypeRow
                  | ExcImperativeFeaturesDisabled Term
                  deriving Eq

instance Show TypingExcept where
  show (ExcUnifyKindMismatch k1 k2) =
    "Kind mismatch in unification, kind "
      ++ show (pretty k1)
      ++ " mismatch with kind "
      ++ show (pretty k2)
  show (ExcUnifyNoRuleApplied s1 s2) =
    "Unable to unify " ++ show (pretty s1) ++ " with " ++ show (pretty s2)
  show (ExcUnifyRowLabelCollided s) = "Row label collided: " ++ show s
  show (ExcUnboundedVariable     x) = "Unbounded variable: " ++ x
  show (ExcFreeVarKindConflict x k1 k2) =
    "Free variable kind conflict in generalization, var: "
      ++ show x
      ++ ", kind 1: "
      ++ show (pretty k1)
      ++ ", kind 2:"
      ++ show (pretty k2)
  show ExcStoreTypingNotImplemented = "Store typing is not implemented"
  show (ExcDescribeNoRuleApplied s) =
    "Unable to describe from unification result: " ++ show (pretty s)
  show (ExcCanNotHandleCofMu r) =
    "Can not handle cofinite mu row: "
      ++ show
           (prettyTypeRow (pretty "(")
                          (pretty ")")
                          (\l -> pretty l <+> pretty ":")
                          r
           )
  show (ExcImperativeFeaturesDisabled t) =
    "Imperative features disabled, can not type: " ++ show (pretty t)

newtype NewVar = NewVar (Map.Map VarName Integer)
  deriving Show

type TypingEnv = Map.Map VarName TypeScheme

type Constraint = (Type, Type)

type Inference a
  =  forall s
   . ExceptT
    TypingExcept
    ( EquivT
        s
        TypeSubstitutor
        TypeSubstitutor
        (ReaderT TypingEnv (State InferenceState))
    )
    a

data InferenceState = InferenceState { inferStateNewVar :: NewVar
                                     , imperativeFeaturesEnabled :: Bool }
                                     deriving Show

runInference
  :: Inference a
  -> TypingEnv
  -> InferenceState
  -> (Either TypingExcept a, InferenceState)
runInference m =
  runState . runReaderT (runEquivT id (flip const) (runExceptT m))

newVar :: VarName -> Inference VarName
newVar prefix = do
  NewVar m <- gets inferStateNewVar
  let i = fromMaybe 0 (Map.lookup prefix m)
  modify (\s -> s { inferStateNewVar = NewVar (Map.insert prefix (i + 1) m) })
  return (if i == 0 then prefix else prefix ++ show i)

resetVarPrefix :: Set.Set VarName -> Inference ()
resetVarPrefix ps = do
  NewVar m <- gets inferStateNewVar
  let m' = Map.map (const 0) (Map.restrictKeys m ps) `Map.union` m -- union = unionWith const
  modify (\s -> s { inferStateNewVar = NewVar m' })

innerVarPrefix :: VarName
innerVarPrefix = "\x03b2"

checkImperativeFeaturesEnabled :: Term -> Inference ()
checkImperativeFeaturesEnabled t = do
  enabled <- gets imperativeFeaturesEnabled
  unless enabled (throwError (ExcImperativeFeaturesDisabled t))

infer :: Term -> Inference Type
infer (TmVar x) = reader (Map.lookup x) >>= \case
  Nothing -> throwError (ExcUnboundedVariable x)
  Just s  -> instantiate s
infer (TmApp t1 t2) = do
  ty1 <- infer t1
  ty2 <- infer t2
  nv  <- TyVar <$> newVar innerVarPrefix
  unifyProper ty1 (TyArrow ty2 nv)
  return nv
infer (TmAbs x t) = do
  nv <- TyVar <$> newVar innerVarPrefix
  ty <- local (Map.insert x (ScmMono nv)) (infer t)
  return (TyArrow nv ty)
infer (TmLet x t1 t2) = do
  ty1 <- infer t1
  s   <- generalize t1 ty1
  local (Map.insert x s) (infer t2)
infer (TmRcd m) = do
  ps <- sequence (Map.map genPresence m)
  return (TyRecord (TyRow ps CofAllAbsent))
 where
  genPresence t = PresenceVarWithType <$> newVar innerVarPrefix <*> infer t
infer (TmRcdExtend t1 l t2) = do
  ty1 <- infer t1
  ty2 <- infer t2
  p1  <- PresenceVar <$> newVar innerVarPrefix
  r   <- CofRowVar <$> newVar innerVarPrefix
  unifyProper ty1 (TyRecord (TyRow (Map.singleton l p1) r))
  p2 <- newVar innerVarPrefix
  return (TyRecord (TyRow (Map.singleton l (PresenceVarWithType p2 ty2)) r))
infer (TmRcdAccess t l) = do
  ty <- infer t
  x  <- TyVar <$> newVar innerVarPrefix
  r  <- CofRowVar <$> newVar innerVarPrefix
  unifyProper ty (TyRecord (TyRow (Map.singleton l (Present x)) r))
  return x
infer (TmMatch m) = do
  m'  <- sequence (Map.map genSingle m)
  res <- TyVar <$> newVar innerVarPrefix
  sequence_ (Map.map (unifyProper res . snd) m')
  return (TyArrow (TyVariant (TyRow (Map.map fst m') CofAllAbsent)) res)
 where
  genSingle (TmCase x t) = do
    nv <- TyVar <$> newVar innerVarPrefix
    ty <- local (Map.insert x (ScmMono nv)) (infer t)
    p  <- newVar innerVarPrefix
    return (PresenceVarWithType p nv, ty)
infer (TmMatchExtend t1 l (TmCase x t2)) = do
  ty1 <- infer t1
  tyX <- TyVar <$> newVar innerVarPrefix
  ty2 <- local (Map.insert x (ScmMono tyX)) (infer t2)
  p1  <- PresenceVar <$> newVar innerVarPrefix
  r   <- CofRowVar <$> newVar innerVarPrefix
  unifyProper ty1 (TyArrow (TyVariant (TyRow (Map.singleton l p1) r)) ty2)
  p2 <- newVar innerVarPrefix
  return
    (TyArrow
      (TyVariant (TyRow (Map.singleton l (PresenceVarWithType p2 tyX)) r))
      ty2
    )
infer (TmVariant l t) = do
  ty <- infer t
  r  <- CofRowVar <$> newVar innerVarPrefix
  return (TyVariant (TyRow (Map.singleton l (Present ty)) r))
infer TmRef = do
  checkImperativeFeaturesEnabled TmRef
  instantiate
    (ScmForall "a" KProper (ScmMono (TyArrow (TyVar "a") (TyRef (TyVar "a")))))
infer TmDeref = do
  checkImperativeFeaturesEnabled TmDeref
  instantiate
    (ScmForall "a" KProper (ScmMono (TyArrow (TyRef (TyVar "a")) (TyVar "a"))))
infer (TmAssign t1 t2) = do
  checkImperativeFeaturesEnabled (TmAssign t1 t2)
  ty1 <- infer t1
  ty2 <- infer t2
  unifyProper ty1 (TyRef ty2)
  return TyUnit
infer (TmLoc _    ) = throwError ExcStoreTypingNotImplemented
infer (TmSeq t1 t2) = do
  _ <- infer t1
  infer t2
infer TmUnit          = return TyUnit
infer TmTrue          = return TyBool
infer TmFalse         = return TyBool
infer (TmIf t1 t2 t3) = do
  ty1 <- infer t1
  ty2 <- infer t2
  ty3 <- infer t3
  unifyProper ty1 TyBool
  unifyProper ty2 ty3
  return ty2
infer (TmNat _) = return TyNat
infer TmSucc    = return (TyArrow TyNat TyNat)
infer TmPred    = return (TyArrow TyNat TyNat)
infer TmIsZero  = return (TyArrow TyNat TyBool)

instantiate :: TypeScheme -> Inference Type
instantiate (ScmForall x KProper s) = do
  nv <- newVar innerVarPrefix
  let s' = applySubst (Map.singleton x (TySubProper (TyVar nv))) s
  instantiate s'
instantiate (ScmForall x KPresence s) = do
  nv <- newVar innerVarPrefix
  let s' = applySubst (Map.singleton x (TySubPresence (PresenceVar nv))) s
  instantiate s'
instantiate (ScmForall x (KArrow KProper KPresence) s) = do
  nv <- newVar innerVarPrefix
  let s' = applySubst
        (Map.singleton x (TySubPresenceWithType (PresenceWithTypeVar nv)))
        s
  instantiate s'
instantiate (ScmForall x (KRow _) s) = do
  nv <- newVar innerVarPrefix
  let
    s' =
      applySubst (Map.singleton x (TySubRow (TyRow Map.empty (CofRowVar nv)))) s
  instantiate s'
instantiate (ScmForall _ k _) = error ("Unknown kind: " ++ show (pretty k))
instantiate (ScmMono t      ) = instantiateType t

-- instantiate mu type in scheme
instantiateType :: Type -> Inference Type
instantiateType (TyVar x) = return (TyVar x)
instantiateType (TyArrow t1 t2) =
  TyArrow <$> instantiateType t1 <*> instantiateType t2
instantiateType (TyRecord  r) = TyRecord <$> instantiateRow r
instantiateType (TyVariant r) = TyVariant <$> instantiateRow r
instantiateType (TyMu x t   ) = do
  t' <- instantiateType t
  unifyProper (TyVar x) t'
  return (TyVar x)
instantiateType (TyRef t) = TyRef <$> instantiateType t
instantiateType TyUnit    = return TyUnit
instantiateType TyBool    = return TyBool
instantiateType TyNat     = return TyNat

instantiateRow :: TypeRow -> Inference TypeRow
instantiateRow (TyRow f cof) = do
  f'   <- sequence (Map.map instantiatePresence f)
  cof' <- case cof of
    CofAllAbsent -> return CofAllAbsent
    CofRowVar x  -> return (CofRowVar x)
    CofMu x r    -> do
      r' <- instantiateRow r
      unifyRow (TyRow Map.empty (CofRowVar x)) r'
      return (CofRowVar x)
  return (TyRow f' cof')

instantiatePresence :: TypePresence -> Inference TypePresence
instantiatePresence Absent          = return Absent
instantiatePresence (Present     t) = Present <$> instantiateType t
instantiatePresence (PresenceVar x) = return (PresenceVar x)
instantiatePresence (PresenceVarWithType x t) =
  PresenceVarWithType x <$> instantiateType t

generalize :: Term -> Type -> Inference TypeScheme
generalize t ty = do
  tyDesc <- describeProper Set.empty ty
  env    <- ask
  -- TODO: is `describeScheme Set.empty` right?
  envFv  <- Map.foldl
    (\a b ->
      bind2 mergeFvWithKind a (describeScheme Set.empty b >>= fvWithKindScm)
    )
    (return Map.empty)
    env
  tFv <- fvWithKind tyDesc
  sequence_ (Map.intersectionWithKey mergeFvWithKindSingle tFv envFv)
  imperative <- gets imperativeFeaturesEnabled
  let xs  = tFv `Map.difference` envFv
      xs' = if imperative && not (isValue t)
        then xs `Map.withoutKeys` dangerousVariable tyDesc
        else xs
  (sub, newXs) <- replacePrefix xs'
  let tyDesc' = applySubst sub tyDesc
  return (Map.foldrWithKey ScmForall (ScmMono tyDesc') newXs)

dangerousVariable :: Type -> Set.Set VarName
dangerousVariable = freeVariable -- traditional value restriction
-- dangerousVariable (TyVar _) = Set.empty
-- dangerousVariable (TyArrow t1 t2) =
--   freeVariable t1 `Set.union` dangerousVariable t2
-- dangerousVariable (TyRecord  r) = dangerousVariableRecordRow r
-- dangerousVariable (TyVariant r) = freeVariable r
-- dangerousVariable (TyMu x t) =
--   let dt = dangerousVariable t
--   in  if x `Set.member` dt
--         then freeVariable t -- x included in dangerous variables
--         else dt
-- dangerousVariable (TyRef t) = freeVariable t
-- dangerousVariable TyUnit    = Set.empty
-- dangerousVariable TyBool    = Set.empty
-- dangerousVariable TyNat     = Set.empty

-- dangerousVariableRecordRow :: TypeRow -> Set.Set VarName
-- dangerousVariableRecordRow (TyRow f cof) =
--   Map.foldl (\a b -> a `Set.union` dangerousVariableRecordPresence b)
--             Set.empty
--             f
--     `Set.union` freeVariable cof

-- dangerousVariableRecordPresence :: TypePresence -> Set.Set VarName
-- dangerousVariableRecordPresence Absent                    = Set.empty
-- dangerousVariableRecordPresence (Present     t          ) = dangerousVariable t
-- dangerousVariableRecordPresence (PresenceVar x          ) = Set.singleton x
-- dangerousVariableRecordPresence (PresenceVarWithType _ t) = dangerousVariable t

kindPrefixes :: Set.Set VarName
kindPrefixes = Set.fromList ["\x03b1", "\x03c6", "\x03c8", "\x03c1"]

kindToPrefix :: Kind -> VarName
kindToPrefix KProper                    = "\x03b1"
kindToPrefix KPresence                  = "\x03c6"
kindToPrefix (KArrow KProper KPresence) = "\x03c8"
kindToPrefix (KRow _                  ) = "\x03c1"
kindToPrefix k = error ("Unknown kind for prefix" ++ show (pretty k))

replacePrefix
  :: Map.Map VarName Kind
  -> Inference (Map.Map VarName TypeSubstitutor, Map.Map VarName Kind)
replacePrefix m = do
  resetVarPrefix kindPrefixes
  nameMap <- sequence (Map.map (newVar . kindToPrefix) m)
  let inv      = Map.fromList [ (v, k) | (k, v) <- Map.toList nameMap ]
      kindMap  = Map.map (m Map.!) inv
      substMap = Map.intersectionWith varToSubstitutor m nameMap
  return (substMap, kindMap)

mergeFvWithKind
  :: Map.Map VarName Kind
  -> Map.Map VarName Kind
  -> Inference (Map.Map VarName Kind)
mergeFvWithKind m1 m2 = (\updated -> updated `Map.union` m1 `Map.union` m2) -- union == unionWith const
  <$> sequence (Map.intersectionWithKey mergeFvWithKindSingle m1 m2)

mergeFvWithKindSingle :: VarName -> Kind -> Kind -> Inference Kind
mergeFvWithKindSingle x a b = case (a, b) of
  (KRow l1, KRow l2) -> return (KRow (l1 `Set.union` l2))
  (k1, k2) | k1 == k2  -> return k1
           | otherwise -> throwError (ExcFreeVarKindConflict x k1 k2)

bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 f ma mb = do
  a <- ma
  b <- mb
  f a b

fvWithKindScm :: TypeScheme -> Inference (Map.Map VarName Kind)
fvWithKindScm (ScmForall x k s) = do
  m <- fvWithKindScm s
  case Map.lookup x m of
    Nothing -> return m
    Just k' | k == k'   -> return (Map.delete x m)
            | otherwise -> throwError (ExcFreeVarKindConflict x k k')
fvWithKindScm (ScmMono t) = fvWithKind t

fvWithKind :: Type -> Inference (Map.Map VarName Kind)
fvWithKind (TyVar x) = return (Map.singleton x KProper)
fvWithKind (TyArrow t1 t2) =
  bind2 mergeFvWithKind (fvWithKind t1) (fvWithKind t2)
fvWithKind (TyRecord  row) = fvWithKindRow row
fvWithKind (TyVariant row) = fvWithKindRow row
fvWithKind (TyMu x t     ) = do
  m <- fvWithKind t
  case Map.lookup x m of
    Nothing      -> return m
    Just KProper -> return (Map.delete x m)
    Just k       -> throwError (ExcFreeVarKindConflict x k KProper)
fvWithKind (TyRef t) = fvWithKind t
fvWithKind TyUnit    = return Map.empty
fvWithKind TyBool    = return Map.empty
fvWithKind TyNat     = return Map.empty

fvWithKindRow :: TypeRow -> Inference (Map.Map VarName Kind)
fvWithKindRow (TyRow f cof) = do
  fvF <- Map.foldl (\a b -> bind2 mergeFvWithKind a (fvWithKindPresence b))
                   (return Map.empty)
                   f
  fvCof <- case cof of
    CofAllAbsent -> return Map.empty
    CofRowVar x  -> return (Map.singleton x (KRow (Map.keysSet f)))
    CofMu x r    -> do
      m <- fvWithKindRow r
      case Map.lookup x m of
        Nothing       -> return m
        Just (KRow _) -> return (Map.delete x m) -- TODO: Maybe can change to KRow (Map.keysSet f)?
        Just k ->
          throwError (ExcFreeVarKindConflict x (KRow (Map.keysSet f)) k)
  mergeFvWithKind fvF fvCof

fvWithKindPresence :: TypePresence -> Inference (Map.Map VarName Kind)
fvWithKindPresence Absent          = return Map.empty
fvWithKindPresence (Present     t) = fvWithKind t
fvWithKindPresence (PresenceVar x) = return (Map.singleton x KPresence)
fvWithKindPresence (PresenceVarWithType x t) =
  fvWithKind t >>= mergeFvWithKind (Map.singleton x (KArrow KProper KPresence))

ensureProper :: (Monad m) => TypeSubstitutor -> ExceptT TypingExcept m Type
ensureProper (TySubProper t) = return t
ensureProper s =
  throwError (ExcUnifyKindMismatch KProper (kindOfSubstitutor s))

ensurePresence
  :: (Monad m) => TypeSubstitutor -> ExceptT TypingExcept m TypePresence
ensurePresence (TySubPresence p) = return p
ensurePresence s =
  throwError (ExcUnifyKindMismatch KPresence (kindOfSubstitutor s))

ensurePresenceWithType
  :: (Monad m) => TypeSubstitutor -> ExceptT TypingExcept m PresenceWithType
ensurePresenceWithType (TySubPresenceWithType p) = return p
ensurePresenceWithType s =
  throwError (ExcUnifyKindMismatch kindOfPresenceWithType (kindOfSubstitutor s))

ensureRow :: (Monad m) => TypeSubstitutor -> ExceptT TypingExcept m TypeRow
ensureRow (TySubRow r) = return r
ensureRow s =
  throwError (ExcUnifyKindMismatch kindOfRowForExc (kindOfSubstitutor s))

unifyProper :: Type -> Type -> Inference ()
unifyProper t1 t2 = do
  t1' <- classDesc (TySubProper t1) >>= ensureProper
  t2' <- classDesc (TySubProper t2) >>= ensureProper
  if t1' == t2'
    then return ()
    else case (t1', t2') of
      (TyVar x1, _       ) -> equate (TySubProper (TyVar x1)) (TySubProper t2')
      (_       , TyVar x2) -> equate (TySubProper (TyVar x2)) (TySubProper t1')
      _                    -> do
        -- record t1' already unified with t2'
        equate (TySubProper t1') (TySubProper t2')
        unifyProper' t1' t2' -- unify structures

unifyProper' :: Type -> Type -> Inference ()
unifyProper' (TyArrow t11 t12) (TyArrow t21 t22) =
  unifyProper t11 t21 >> unifyProper t12 t22
unifyProper' (TyRecord  r1) (TyRecord  r2) = unifyRow r1 r2
unifyProper' (TyVariant r1) (TyVariant r2) = unifyRow r1 r2
unifyProper' (TyRef     t1) (TyRef     t2) = unifyProper t1 t2
unifyProper' TyUnit         TyUnit         = return ()
unifyProper' TyBool         TyBool         = return ()
unifyProper' TyNat          TyNat          = return ()
unifyProper' t1 t2 =
  throwError (ExcUnifyNoRuleApplied (TySubProper t1) (TySubProper t2))

unifyPresenceWithType :: PresenceWithType -> PresenceWithType -> Inference ()
unifyPresenceWithType p1 p2 = do
  p1' <- classDesc (TySubPresenceWithType p1) >>= ensurePresenceWithType
  p2' <- classDesc (TySubPresenceWithType p2) >>= ensurePresenceWithType
  if p1' == p2'
    then return ()
    else case (p1', p2') of
      (PresenceWithTypeVar x1, _) -> equate
        (TySubPresenceWithType (PresenceWithTypeVar x1))
        (TySubPresenceWithType p2')
      (_, PresenceWithTypeVar x2) -> equate
        (TySubPresenceWithType (PresenceWithTypeVar x2))
        (TySubPresenceWithType p1')
      _ -> throwError
        (ExcUnifyNoRuleApplied (TySubPresenceWithType p1')
                               (TySubPresenceWithType p2')
        )

unifyPresence :: TypePresence -> TypePresence -> Inference ()
unifyPresence p1 p2 = do
  p1' <- classDesc (TySubPresence p1) >>= ensurePresence
  p2' <- classDesc (TySubPresence p2) >>= ensurePresence
  unifyPresence' p1' p2'

unifyPresence' :: TypePresence -> TypePresence -> Inference ()
unifyPresence' Absent       Absent                          = return ()
unifyPresence' (Present t1) (Present t2)                    = unifyProper t1 t2
unifyPresence' (PresenceVar x1) (PresenceVar x2) | x1 == x2 = return ()
unifyPresence' (PresenceVar x1) p2 =
  equate (TySubPresence (PresenceVar x1)) (TySubPresence p2)
unifyPresence' p1 (PresenceVar x2) =
  equate (TySubPresence (PresenceVar x2)) (TySubPresence p1)
unifyPresence' (PresenceVarWithType x1 _) Absent =
  unifyPresenceWithType (PresenceWithTypeVar x1) PresenceWithTypeAbsent
unifyPresence' (PresenceVarWithType x1 t1) (Present t2) =
  unifyPresenceWithType (PresenceWithTypeVar x1) PresenceWithTypePresent
    >> unifyProper t1 t2
unifyPresence' (PresenceVarWithType x1 t1) (PresenceVarWithType x2 t2) =
  unifyPresenceWithType (PresenceWithTypeVar x1) (PresenceWithTypeVar x2)
    >> unifyProper t1 t2
unifyPresence' Absent (PresenceVarWithType x2 _) =
  unifyPresenceWithType (PresenceWithTypeVar x2) PresenceWithTypeAbsent
unifyPresence' (Present t1) (PresenceVarWithType x1 t2) =
  unifyPresenceWithType (PresenceWithTypeVar x1) PresenceWithTypePresent
    >> unifyProper t1 t2
unifyPresence' p1 p2 =
  throwError (ExcUnifyNoRuleApplied (TySubPresence p1) (TySubPresence p2))

unifyRow :: TypeRow -> TypeRow -> Inference ()
unifyRow r1 r2 = do
  r1' <- classDesc (TySubRow r1) >>= ensureRow >>= flattenRow
  r2' <- classDesc (TySubRow r2) >>= ensureRow >>= flattenRow
  unifyRow' r1' r2'

unifyRow' :: TypeRow -> TypeRow -> Inference ()
unifyRow' (TyRow f1 cof1) (TyRow f2 cof2) = do
  -- handle intersection part
  sequence_ (Map.intersectionWith unifyPresence f1 f2)
  let f1' = f1 `Map.difference` f2
      f2' = f2 `Map.difference` f1
  case (cof1, cof2) of
    (CofAllAbsent, CofAllAbsent) -> do
      sequence_ (Map.map (unifyPresence Absent) f1')
      sequence_ (Map.map (unifyPresence Absent) f2')
    (CofAllAbsent, CofRowVar x2) -> do
      equate (TySubRow (TyRow Map.empty (CofRowVar x2)))
             (TySubRow (TyRow f1' CofAllAbsent))
      sequence_ (Map.map (unifyPresence Absent) f2')
    (CofRowVar x1, CofAllAbsent) -> do
      equate (TySubRow (TyRow Map.empty (CofRowVar x1)))
             (TySubRow (TyRow f2' CofAllAbsent))
      sequence_ (Map.map (unifyPresence Absent) f1')
    (CofRowVar x1, CofRowVar x2) | x1 == x2 -> do
      sequence_ (Map.map (unifyPresence Absent) f1')
      sequence_ (Map.map (unifyPresence Absent) f2')
    (CofRowVar x1, CofRowVar x2) -> do
      newX <- newVar innerVarPrefix
      equate (TySubRow (TyRow Map.empty (CofRowVar x1)))
             (TySubRow (TyRow f2' (CofRowVar newX)))
      equate (TySubRow (TyRow Map.empty (CofRowVar x2)))
             (TySubRow (TyRow f1' (CofRowVar newX)))
    (_, _) -> throwError
      (ExcUnifyNoRuleApplied (TySubRow (TyRow f1 cof1))
                             (TySubRow (TyRow f2 cof2))
      )

flattenRow :: TypeRow -> Inference TypeRow
flattenRow (TyRow f CofAllAbsent)               = return (TyRow f CofAllAbsent)
flattenRow (TyRow f (CofRowVar x)) | Map.null f = do
  (TyRow f' inf) <- classDesc (TySubRow (TyRow f (CofRowVar x))) >>= ensureRow
  if TyRow f' inf == TyRow f (CofRowVar x)
    then return (TyRow f (CofRowVar x))
    else flattenRow (TyRow f' inf)
flattenRow (TyRow f1 (CofRowVar x)) = do
  (TyRow f2 inf2) <- flattenRow (TyRow Map.empty (CofRowVar x))
  let overlapped = Map.keysSet (f1 `Map.intersection` f2)
  unless (Set.null overlapped)
         (throwError (ExcUnifyRowLabelCollided overlapped))
  return (TyRow (Map.union f1 f2) inf2)
flattenRow r@(TyRow _ (CofMu _ _)) = throwError (ExcCanNotHandleCofMu r)

describeScheme :: Set.Set VarName -> TypeScheme -> Inference TypeScheme
describeScheme ctx (ScmForall x k s) =
  ScmForall x k <$> describeScheme (Set.insert x ctx) s
describeScheme ctx (ScmMono t) = ScmMono <$> describeProper ctx t

describeProper :: Set.Set VarName -> Type -> Inference Type
describeProper ctx (TyVar x) | x `Set.member` ctx = return (TyVar x)
describeProper ctx (TyVar x)                      = do
  t' <- classDesc (TySubProper (TyVar x)) >>= ensureProper
  if t' == TyVar x
    then return (TyVar x)
    else do
      t'' <- describeProper (Set.insert x ctx) t'
      let fv = freeVariable t''
      return (if x `Set.member` fv then TyMu x t'' else t'')
describeProper ctx (TyArrow t1 t2) =
  TyArrow <$> describeProper ctx t1 <*> describeProper ctx t2
describeProper ctx (TyRecord  row) = TyRecord <$> describeRow ctx row
describeProper ctx (TyVariant row) = TyVariant <$> describeRow ctx row
describeProper ctx (TyRef     t  ) = TyRef <$> describeProper ctx t
describeProper _   TyUnit          = return TyUnit
describeProper _   TyBool          = return TyBool
describeProper _   TyNat           = return TyNat
-- describeProper _ t@(TyMu _ _) =
--   throwError (ExcDescribeNoRuleApplied (TySubProper t))
describeProper ctx (TyMu x t) = TyMu x <$> describeProper (Set.insert x ctx) t

describePresence :: Set.Set VarName -> TypePresence -> Inference TypePresence
describePresence _   Absent          = return Absent
describePresence ctx (Present     t) = Present <$> describeProper ctx t
describePresence ctx (PresenceVar x) = do
  p <- classDesc (TySubPresence (PresenceVar x)) >>= ensurePresence
  if p == PresenceVar x then return (PresenceVar x) else describePresence ctx p
describePresence ctx (PresenceVarWithType x t) = do
  p <-
    classDesc (TySubPresenceWithType (PresenceWithTypeVar x))
      >>= ensurePresenceWithType
  if p == PresenceWithTypeVar x
    then PresenceVarWithType x <$> describeProper ctx t
    else case p of
      PresenceWithTypeAbsent  -> return Absent
      PresenceWithTypePresent -> Present <$> describeProper ctx t
      PresenceWithTypeVar x'  -> PresenceVarWithType x' <$> describeProper ctx t

describeRow :: Set.Set VarName -> TypeRow -> Inference TypeRow
describeRow ctx (TyRow f CofAllAbsent) =
  flip TyRow CofAllAbsent <$> sequence (Map.map (describePresence ctx) f)
describeRow ctx (TyRow f (CofRowVar x)) | Map.null f && x `Set.member` ctx =
  return (TyRow f (CofRowVar x))
describeRow ctx (TyRow f (CofRowVar x)) | Map.null f = do
  (TyRow f' inf) <- classDesc (TySubRow (TyRow f (CofRowVar x))) >>= ensureRow
  if TyRow f' inf == TyRow f (CofRowVar x)
    then return (TyRow f (CofRowVar x))
    else do
      r' <- describeRow (Set.insert x ctx) (TyRow f' inf)
      let fv = freeVariable r'
      return (if x `Set.member` fv then TyRow Map.empty (CofMu x r') else r')
describeRow ctx (TyRow f1 (CofRowVar x)) = do
  (TyRow f2 inf2) <- describeRow ctx (TyRow Map.empty (CofRowVar x))
  f1'             <- sequence (Map.map (describePresence ctx) f1)
  let overlapped = Map.keysSet (f1' `Map.intersection` f2)
  unless (Set.null overlapped)
         (throwError (ExcUnifyRowLabelCollided overlapped))
  return (TyRow (Map.union f1' f2) inf2)
describeRow _ r@(TyRow _ (CofMu _ _)) = throwError (ExcCanNotHandleCofMu r)

regTreeEq' :: Type -> Type -> Bool
regTreeEq' t1 t2 = case runState (runMaybeT (regTreeEq t1 t2)) Set.empty of
  (Nothing, _) -> False
  (Just (), _) -> True

regTreeNeq' :: Type -> Type -> Bool
regTreeNeq' t1 t2 = not (regTreeEq' t1 t2)

regTreeEq
  :: (Monad m) => Type -> Type -> MaybeT (StateT (Set.Set (Type, Type)) m) ()
regTreeEq t1 t2 = gets (Set.member (t1, t2)) >>= \case
  True  -> return ()
  False -> modify (Set.insert (t1, t2)) >> case (t1, t2) of
    (TyUnit, TyUnit) -> return ()
    (TyBool, TyBool) -> return ()
    (TyNat , TyNat ) -> return ()
    (TyArrow t11 t12, TyArrow t21 t22) ->
      regTreeEq t11 t21 >> regTreeEq t12 t22
    (TyRef     t1', TyRef t2'   )   -> regTreeEq t1' t2'
    (TyRecord  r1 , TyRecord r2 )   -> regTreeEqRow r1 r2
    (TyVariant r1 , TyVariant r2)   -> regTreeEqRow r1 r2
    (TyVar x1, TyVar x2) | x1 == x2 -> return ()
    (TyMu x1 t12, _) ->
      regTreeEq (applySubst (Map.singleton x1 (TySubProper t1)) t12) t2
    (_, TyMu x2 t22) ->
      regTreeEq t1 (applySubst (Map.singleton x2 (TySubProper t2)) t22)
    _ -> mzero

regTreeEqRow
  :: (Monad m)
  => TypeRow
  -> TypeRow
  -> MaybeT (StateT (Set.Set (Type, Type)) m) ()
regTreeEqRow (TyRow f1 cof1) (TyRow f2 cof2)
  | Map.keysSet f1 == Map.keysSet f2
  = sequence_ (Map.intersectionWith regTreeEqPresence f1 f2)
    >> case (cof1, cof2) of
         (CofAllAbsent, CofAllAbsent) -> return ()
         (CofRowVar x1, CofRowVar x2) | x1 == x2 -> return ()
         _                            -> mzero
  | otherwise
  = mzero

regTreeEqPresence
  :: (Monad m)
  => TypePresence
  -> TypePresence
  -> MaybeT (StateT (Set.Set (Type, Type)) m) ()
regTreeEqPresence p1 p2 = case (p1, p2) of
  (Absent    , Absent    )                    -> return ()
  (Present t1, Present t2)                    -> regTreeEq t1 t2
  (PresenceVar x1, PresenceVar x2) | x1 == x2 -> return ()
  (PresenceVarWithType x1 t1, PresenceVarWithType x2 t2) | x1 == x2 ->
    regTreeEq t1 t2
  _ -> mzero
