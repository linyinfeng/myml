{-# LANGUAGE LambdaCase, RankNTypes #-}

module Myml.Typing
  ( NewVar(..)
  , TypingEnv
  , Inference
  , InferenceState(..)
  , newVar
  , runInference
  -- main infer function
  , infer
  -- instantiate and generalize
  , instantiate
  , instantiateType
  , instantiateRow
  , instantiatePresence
  , generalize
  , replaceSafePresent
  -- unify
  , unifyProper
  , unifyPresenceWithType
  , unifyPresence
  , unifyRow
  -- describe
  , describeScheme
  , describeProper
  , describePresence
  , describeRow
  )
where

import           Myml.Syntax
import           Myml.Subst
import           Data.Equivalence.Monad
import           Data.Maybe
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Data.Text.Prettyprint.Doc
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

newtype NewVar = NewVar (Map.Map VarName Integer)
  deriving Show

type TypingEnv = Map.Map VarName TypeScheme

type Inference a
  =  forall s
   . ExceptT
    Error
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
  -> (Either Error a, InferenceState)
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
  -- union = unionWith const
  let m' = Map.map (const 0) (Map.restrictKeys m ps) `Map.union` m
  modify (\s -> s { inferStateNewVar = NewVar m' })

newVarInner :: Kind -> Inference VarName
newVarInner = newVar . ('_' :) . kindToPrefix

kindPrefixes :: Set.Set VarName
kindPrefixes = Set.fromList ["\x03b1", "\x03c6", "\x03c8", "\x03c1"]

kindToPrefix :: Kind -> VarName
kindToPrefix KProper           = "\x03b1"
kindToPrefix KPresence         = "\x03c6"
kindToPrefix KPresenceWithType = "\x03c8"
kindToPrefix KRow              = "\x03c1"
kindToPrefix k = error ("Unknown kind for prefix" ++ show (pretty k))

checkImperativeFeaturesEnabled :: Term -> Inference ()
checkImperativeFeaturesEnabled t = do
  enabled <- gets imperativeFeaturesEnabled
  unless enabled (throwError (ErrImperativeFeaturesDisabled t))

infer :: Term -> Inference Type
infer (TmVar x) = reader (Map.lookup x) >>= \case
  Nothing -> throwError (ErrUnboundedVariable x)
  Just s  -> instantiate s
infer (TmApp t1 t2) = do
  ty1 <- infer t1
  ty2 <- infer t2
  nv  <- TyVar <$> newVarInner KProper
  unifyProper ty1 (TyArrow ty2 nv)
  return nv
infer (TmAbs x t) = do
  nv <- TyVar <$> newVarInner KProper
  ty <- local (Map.insert x (ScmMono nv)) (infer t)
  return (TyArrow nv ty)
infer (TmLet x t1 t2) = do
  ty1  <- infer t1
  ctx  <- ask
  ctx' <- mapM (describeScheme True Set.empty) ctx
  local
    (const ctx')
    (do
      s <- generalize t1 ty1
      local (Map.insert x s) (infer t2)
    )
infer TmEmptyRcd      = return (TyRecord RowEmpty)
infer (TmRcdExtend l) = instantiate
  ( ScmForall "a"  KProper
  $ ScmForall "p"  KPresence
  $ ScmForall "r"  KRow
  $ ScmForall "pt" KPresenceWithType
  $ ScmMono
      (         TyVar "a"
      `TyArrow` TyRecord (RowPresence l (PresenceVar "p") (RowVar "r"))
      `TyArrow` TyRecord
                  (RowPresence l
                               (PresenceVarWithType "pt" (TyVar "a"))
                               (RowVar "r")
                  )
      )
  )
infer (TmRcdUpdate l) = instantiate
  ( ScmForall "a1" KProper
  $ ScmForall "a2" KProper
  $ ScmForall "r"  KRow
  $ ScmForall "pt" KPresenceWithType
  $ ScmMono
      (         TyVar "a1"
      `TyArrow` TyRecord (RowPresence l (Present (TyVar "a2")) (RowVar "r"))
      `TyArrow` TyRecord
                  (RowPresence l
                               (PresenceVarWithType "pt" (TyVar "a1"))
                               (RowVar "r")
                  )
      )
  )
infer (TmRcdAccess l) = instantiate
  (ScmForall "a" KProper $ ScmForall "r" KRow $ ScmMono
    (         TyRecord (RowPresence l (Present (TyVar "a")) (RowVar "r"))
    `TyArrow` TyVar "a"
    )
  )
infer TmEmptyMatch = instantiate
  (ScmForall "a" KProper $ ScmMono (TyVariant RowEmpty `TyArrow` TyVar "a"))
infer (TmMatchExtend l) = instantiate
  ( ScmForall "a"   KProper
  $ ScmForall "p"   KPresence
  $ ScmForall "ret" KProper
  $ ScmForall "row" KRow
  $ ScmForall "pt"  KPresenceWithType
  $ ScmMono
      (         (TyVar "a" `TyArrow` TyVar "ret")
      `TyArrow` (TyVariant (RowPresence l (PresenceVar "p") (RowVar "row"))
                `TyArrow` TyVar "ret"
                )
      `TyArrow` TyVariant
                  (RowPresence l
                               (PresenceVarWithType "pt" (TyVar "a"))
                               (RowVar "row")
                  )
      `TyArrow` TyVar "ret"
      )
  )
infer (TmMatchUpdate l) = instantiate
  ( ScmForall "a"   KProper
  $ ScmForall "b"   KProper
  $ ScmForall "ret" KProper
  $ ScmForall "row" KRow
  $ ScmForall "pt"  KPresenceWithType
  $ ScmMono
      (         (TyVar "a" `TyArrow` TyVar "ret")
      `TyArrow` (         TyVariant
                    (RowPresence l (Present (TyVar "b")) (RowVar "row"))
                `TyArrow` TyVar "ret"
                )
      `TyArrow` TyVariant
                  (RowPresence l
                               (PresenceVarWithType "pt" (TyVar "a"))
                               (RowVar "row")
                  )
      `TyArrow` TyVar "ret"
      )
  )
infer (TmVariant l) = instantiate
  (ScmForall "a" KProper $ ScmForall "r" KRow $ ScmMono
    (         TyVar "a"
    `TyArrow` TyVariant (RowPresence l (Present (TyVar "a")) (RowVar "r"))
    )
  )
infer TmRef = do
  checkImperativeFeaturesEnabled TmRef
  instantiate
    (ScmForall "a" KProper (ScmMono (TyArrow (TyVar "a") (TyRef (TyVar "a")))))
infer TmDeref = do
  checkImperativeFeaturesEnabled TmDeref
  instantiate
    (ScmForall "a" KProper (ScmMono (TyArrow (TyRef (TyVar "a")) (TyVar "a"))))
infer TmAssign = do
  checkImperativeFeaturesEnabled TmAssign
  instantiate
    (ScmForall
      "a"
      KProper
      (ScmMono (TyArrow (TyRef (TyVar "a")) (TyArrow (TyVar "a") TyUnit)))
    )
infer (TmLoc     _) = throwError ErrStoreTypingNotImplemented
infer (TmInteger _) = return TyInteger
infer TmIntegerPlus =
  return (TyInteger `TyArrow` TyInteger `TyArrow` TyInteger)
infer TmIntegerMul = return (TyInteger `TyArrow` TyInteger `TyArrow` TyInteger)
infer TmIntegerAbs     = return (TyInteger `TyArrow` TyInteger)
infer TmIntegerSignum  = return (TyInteger `TyArrow` TyInteger)
infer TmIntegerNegate  = return (TyInteger `TyArrow` TyInteger)
infer TmIntegerQuotRem = instantiate
  (ScmForall "p1" KPresenceWithType $ ScmForall "p2" KPresenceWithType $ ScmMono
    (TyInteger `TyArrow` TyInteger `TyArrow` typeQuotRem "p1" "p2")
  )
infer TmIntegerCompare = instantiate
  ( ScmForall "r" KRow
  $ ScmMono (TyInteger `TyArrow` TyInteger `TyArrow` typeOrdering "r")
  )
infer (TmChar _)  = return TyChar
infer TmIOGetChar = instantiate
  (ScmForall "r" KRow $ ScmMono (TyArrow TyUnit (typeMaybe TyChar "r")))
infer TmIOPutChar   = return (TyArrow TyChar TyUnit)
infer TmCharCompare = instantiate
  ( ScmForall "r" KRow
  $ ScmMono (TyChar `TyArrow` TyChar `TyArrow` typeOrdering "r")
  )

instantiate :: TypeScheme -> Inference Type
instantiate (ScmForall x KProper s) = do
  nv <- newVarInner KProper
  s' <- liftEither (substType (Map.singleton x (TySubProper (TyVar nv))) s)
  instantiate s'
instantiate (ScmForall x KPresence s) = do
  nv <- newVarInner KPresence
  s' <- liftEither
    (substType (Map.singleton x (TySubPresence (PresenceVar nv))) s)
  instantiate s'
instantiate (ScmForall x KPresenceWithType s) = do
  nv <- newVarInner KPresenceWithType
  s' <- liftEither
    (substType
      (Map.singleton x (TySubPresenceWithType (PresenceWithTypeVar nv)))
      s
    )
  instantiate s'
instantiate (ScmForall x KRow s) = do
  nv <- newVarInner KRow
  s' <- liftEither (substType (Map.singleton x (TySubRow (RowVar nv))) s)
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
instantiateType TyInteger = return TyInteger
instantiateType TyChar    = return TyChar

instantiateRow :: TypeRow -> Inference TypeRow
instantiateRow RowEmpty   = return RowEmpty
instantiateRow (RowVar x) = return (RowVar x)
instantiateRow (RowPresence l p r) =
  RowPresence l <$> instantiatePresence p <*> instantiateRow r
instantiateRow (RowMu x r) = do
  r' <- instantiateRow r
  unifyRow (RowVar x) r'
  return (RowVar x)

instantiatePresence :: TypePresence -> Inference TypePresence
instantiatePresence Absent          = return Absent
instantiatePresence (Present     t) = Present <$> instantiateType t
instantiatePresence (PresenceVar x) = return (PresenceVar x)
instantiatePresence (PresenceVarWithType x t) =
  PresenceVarWithType x <$> instantiateType t

generalize :: Term -> Type -> Inference TypeScheme
generalize t ty = do
  tyDesc <- describeProper False Set.empty ty
  -- this step replace all Presents only appear in covariant location to a fresh variable
  tyRep  <- replaceSafePresent tyDesc
  tFv    <- liftEither (fvType tyRep)
  env    <- ask
  envFv  <- liftEither
    (Map.foldl (\a b -> bind2AndLift mapUnionWithKind a (fvScheme b))
               (return Map.empty)
               env
    )
  -- check kind conflicts
  _          <- liftEither (mapUnionWithKind tFv envFv)
  imperative <- gets imperativeFeaturesEnabled
  let xs = tFv `Map.difference` envFv
  xs' <- if imperative && maybeExpansive t
    then liftEither (dangerousVar tyRep >>= mapDiffWithKind xs)
    else return xs
  (sub, newXs) <- replacePrefix xs'
  tyRep'       <- liftEither (substType sub tyRep)
  return (Map.foldrWithKey ScmForall (ScmMono tyRep') newXs)
 where
  bind2AndLift f ma mb = do
    a <- ma
    b <- mb
    liftEither (f a b)

maybeExpansive :: Term -> Bool
maybeExpansive = not . isNonExpansive

isNonExpansive :: Term -> Bool
-- record extend and update
isNonExpansive (TmApp (TmApp (TmRcdExtend _) t1) t2) =
  isNonExpansive t1 && isNonExpansive t2
isNonExpansive (TmApp (TmApp (TmRcdUpdate _) t1) t2) =
  isNonExpansive t1 && isNonExpansive t2
isNonExpansive (TmApp (TmRcdAccess _) t) = isNonExpansive t
isNonExpansive TmEmptyRcd                = True
-- match value
isNonExpansive (TmApp (TmApp (TmMatchExtend _) t1) t2) =
  isNonExpansive t1 && isNonExpansive t2
isNonExpansive (TmApp (TmApp (TmMatchUpdate _) t1) t2) =
  isNonExpansive t1 && isNonExpansive t2
isNonExpansive TmEmptyMatch                = True
-- variant value
isNonExpansive (TmApp (TmVariant     _) t) = isNonExpansive t
-- partial applied
isNonExpansive (TmApp (TmRcdExtend   _) t) = isNonExpansive t
isNonExpansive (TmApp (TmRcdUpdate   _) t) = isNonExpansive t
isNonExpansive (TmApp (TmMatchExtend _) t) = isNonExpansive t
isNonExpansive (TmApp (TmMatchUpdate _) t) = isNonExpansive t
isNonExpansive (TmApp TmAssign          t) = isNonExpansive t
isNonExpansive (TmApp TmCharCompare     t) = isNonExpansive t
isNonExpansive (TmApp TmIntegerPlus     t) = isNonExpansive t
isNonExpansive (TmApp TmIntegerMul      t) = isNonExpansive t
isNonExpansive (TmApp TmIntegerQuotRem  t) = isNonExpansive t
isNonExpansive (TmApp TmIntegerCompare  t) = isNonExpansive t
-- Basic
isNonExpansive (TmApp _                 _) = False
isNonExpansive (TmVar _                  ) = True
isNonExpansive (TmAbs _ _                ) = True
isNonExpansive (TmLet _ t1 t2) = isNonExpansive t1 && isNonExpansive t2
isNonExpansive _                           = True

dangerousVar :: Type -> Either Error (Map.Map VarName Kind)
-- dangerousVar = fvType -- traditional value restriction
dangerousVar (TyVar _      ) = return Map.empty
dangerousVar (TyArrow t1 t2) = do
  d1 <- fvType t1
  d2 <- dangerousVar t2
  mapUnionWithKind d1 d2
dangerousVar (TyRecord  r) = dangerousVarRow r
dangerousVar (TyVariant r) = dangerousVarRow r
dangerousVar (TyMu x t   ) = do
  dt <- dangerousVar t
  case Map.lookup x dt of
    Nothing      -> return dt
    Just KProper -> fvType (TyMu x t) -- x included in dangerous variables
    Just k       -> Left (ErrVarKindConflict x KProper k)
dangerousVar t = fvType t

dangerousVarRow :: TypeRow -> Either Error (Map.Map VarName Kind)
dangerousVarRow (RowPresence _label p r) = do
  dp <- dangerousVarPresence p
  dr <- dangerousVarRow r
  mapUnionWithKind dp dr
dangerousVarRow (RowMu x r) = do
  dr <- dangerousVarRow r
  case Map.lookup x dr of
    Nothing   -> return dr
    Just KRow -> fvRow (RowMu x r)
    Just k    -> Left (ErrVarKindConflict x KRow k)
dangerousVarRow (RowVar _) = return Map.empty -- for variant
dangerousVarRow RowEmpty   = return Map.empty

dangerousVarPresence :: TypePresence -> Either Error (Map.Map VarName Kind)
dangerousVarPresence (Present t              ) = dangerousVar t
-- for record, mark generalization of PresenceVarWithType safe
dangerousVarPresence (PresenceVarWithType _ t) = dangerousVar t
dangerousVarPresence p                         = fvPresence p

replaceSafePresent :: Type -> Inference Type
replaceSafePresent (TyArrow t1 t2) = TyArrow t1 <$> replaceSafePresent t2
replaceSafePresent (TyMu    x  t ) = do
  dt <- liftEither (dangerousVar t)
  case Map.lookup x dt of
    Nothing      -> TyMu x <$> replaceSafePresent t
    Just KProper -> return (TyMu x t) -- x included in dangerous variables
    Just k       -> throwError (ErrVarKindConflict x KProper k)
replaceSafePresent (TyRecord  r) = TyRecord <$> replaceSafePresentRow True r
replaceSafePresent (TyVariant r) = TyVariant <$> replaceSafePresentRow False r
replaceSafePresent t             = return t

replaceSafePresentRow :: Bool -> TypeRow -> Inference TypeRow
replaceSafePresentRow shouldReplace (RowPresence label p r) =
  RowPresence label
    <$> replaceSafePresentPresence shouldReplace p
    <*> replaceSafePresentRow shouldReplace r
replaceSafePresentRow shouldReplace (RowMu x r) = do
  dr <- liftEither (dangerousVarRow r)
  case Map.lookup x dr of
    Nothing   -> RowMu x <$> replaceSafePresentRow shouldReplace r
    Just KRow -> return (RowMu x r) -- x included in dangerous variables
    Just k    -> throwError (ErrVarKindConflict x KRow k)
replaceSafePresentRow _shouldReplace r = return r

replaceSafePresentPresence :: Bool -> TypePresence -> Inference TypePresence
replaceSafePresentPresence shouldReplace (Present t) = if shouldReplace
  then do
    x <- newVarInner KPresenceWithType
    PresenceVarWithType x <$> replaceSafePresent t
  else Present <$> replaceSafePresent t
replaceSafePresentPresence _shouldReplace (PresenceVarWithType x t) =
  PresenceVarWithType x <$> replaceSafePresent t
replaceSafePresentPresence _shouldReplace p = return p

replacePrefix
  :: Map.Map VarName Kind
  -> Inference (Map.Map VarName TypeSubstitutor, Map.Map VarName Kind)
replacePrefix m = do
  resetVarPrefix kindPrefixes
  nameMap <- sequence (Map.map (newVar . kindToPrefix) m)
  let inv     = Map.fromList [ (v, k) | (k, v) <- Map.toList nameMap ]
      kindMap = Map.map (m Map.!) inv
  substMap <- liftEither (sequence (Map.intersectionWith varToTySub m nameMap))
  return (substMap, kindMap)

ensureProper :: (Monad m) => TypeSubstitutor -> ExceptT Error m Type
ensureProper (TySubProper t) = return t
ensureProper s = throwError (ErrUnifyKindMismatch KProper (kindOfTySub s))

ensurePresence :: (Monad m) => TypeSubstitutor -> ExceptT Error m TypePresence
ensurePresence (TySubPresence p) = return p
ensurePresence s = throwError (ErrUnifyKindMismatch KPresence (kindOfTySub s))

ensurePresenceWithType
  :: (Monad m) => TypeSubstitutor -> ExceptT Error m PresenceWithType
ensurePresenceWithType (TySubPresenceWithType p) = return p
ensurePresenceWithType s =
  throwError (ErrUnifyKindMismatch KPresenceWithType (kindOfTySub s))

ensureRow :: (Monad m) => TypeSubstitutor -> ExceptT Error m TypeRow
ensureRow (TySubRow r) = return r
ensureRow s            = throwError (ErrUnifyKindMismatch KRow (kindOfTySub s))

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
unifyProper' t1 t2 =
  throwError (ErrUnifyNoRuleApplied (TySubProper t1) (TySubProper t2))

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
        (ErrUnifyNoRuleApplied (TySubPresenceWithType p1')
                               (TySubPresenceWithType p2')
        )

unifyPresence :: TypePresence -> TypePresence -> Inference ()
unifyPresence p1 p2 = do
  p1' <- classDesc (TySubPresence p1) >>= ensurePresence
  p2' <- classDesc (TySubPresence p2) >>= ensurePresence
  if p1' == p2'
    then return ()
    else case (p1', p2') of
      (PresenceVar x1, _) ->
        equate (TySubPresence (PresenceVar x1)) (TySubPresence p2')
      (_, PresenceVar x2) ->
        equate (TySubPresence (PresenceVar x2)) (TySubPresence p1')
      _ -> unifyPresence' p1' p2'

unifyPresence' :: TypePresence -> TypePresence -> Inference ()
unifyPresence' (Present t1) (Present t2) = unifyProper t1 t2
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
unifyPresence' (Present t1) (PresenceVarWithType x2 t2) =
  unifyPresenceWithType (PresenceWithTypeVar x2) PresenceWithTypePresent
    >> unifyProper t1 t2
unifyPresence' p1 p2 =
  throwError (ErrUnifyNoRuleApplied (TySubPresence p1) (TySubPresence p2))

unifyRow :: TypeRow -> TypeRow -> Inference ()
unifyRow r1 r2 = do
  r1' <- classDesc (TySubRow r1) >>= ensureRow
  r2' <- classDesc (TySubRow r2) >>= ensureRow
  if r1' == r2'
    then return ()
    else case (r1', r2') of
      (RowVar x1, _        ) -> equate (TySubRow (RowVar x1)) (TySubRow r2')
      (_        , RowVar x2) -> equate (TySubRow (RowVar x2)) (TySubRow r1')
      -- _                      -> unifyRow' r1' r2'
      _                      -> do
        -- record r1' already unified with r2'
        -- equate (TySubRow r1') (TySubRow r2')
        unifyRow' r1' r2' -- unify structures

unifyRow' :: TypeRow -> TypeRow -> Inference ()
unifyRow' RowEmpty (RowPresence _l p r) =
  unifyPresence p Absent >> unifyRow r RowEmpty
unifyRow' (RowPresence _l p r) RowEmpty =
  unifyPresence p Absent >> unifyRow r RowEmpty
unifyRow' (RowPresence l1 p1 r1) (RowPresence l2 p2 r2) = if l1 == l2
  then unifyPresence p1 p2 >> unifyRow r1 r2
  else do
    r3 <- newVarInner KRow
    let r3' = RowVar r3
    unifyRow r1 (RowPresence l2 p2 r3')
    unifyRow r2 (RowPresence l1 p1 r3')
unifyRow' r1 r2 =
  throwError (ErrUnifyNoRuleApplied (TySubRow r1) (TySubRow r2))

-- add kind check
describeScheme :: Bool -> Set.Set VarName -> TypeScheme -> Inference TypeScheme
describeScheme allowMu ctx (ScmForall x k s) =
  ScmForall x k <$> describeScheme allowMu (Set.insert x ctx) s
describeScheme allowMu ctx (ScmMono t) =
  ScmMono <$> describeProper allowMu ctx t

describeProper :: Bool -> Set.Set VarName -> Type -> Inference Type
describeProper _ ctx (TyVar x) | x `Set.member` ctx = return (TyVar x)
describeProper allowMu ctx (TyVar x)                = do
  t <- classDesc (TySubProper (TyVar x)) >>= ensureProper
  if t == TyVar x
    then return (TyVar x)
    else do
      t' <- describeProper allowMu (Set.insert x ctx) t
      fv <- liftEither (fvType t')
      case Map.lookup x fv of
        Nothing      -> return t'
        Just KProper -> return (TyMu x t')
        Just k       -> throwError (ErrVarKindConflict x KProper k)
describeProper allowMu ctx (TyArrow t1 t2) =
  TyArrow <$> describeProper allowMu ctx t1 <*> describeProper allowMu ctx t2
describeProper allowMu ctx (TyRecord row) =
  TyRecord <$> describeRow allowMu ctx row
describeProper allowMu ctx (TyVariant row) =
  TyVariant <$> describeRow allowMu ctx row
describeProper allowMu ctx (TyRef t ) = TyRef <$> describeProper allowMu ctx t
describeProper allowMu ctx (TyMu x t) = if allowMu
  then TyMu x <$> describeProper allowMu (Set.insert x ctx) t
  else throwError (ErrCanNotHandleMuType (TyMu x t))
describeProper _ _ TyInteger = return TyInteger
describeProper _ _ TyChar    = return TyChar

describePresence
  :: Bool -> Set.Set VarName -> TypePresence -> Inference TypePresence
describePresence _ _ Absent = return Absent
describePresence allowMu ctx (Present t) =
  Present <$> describeProper allowMu ctx t
describePresence allowMu ctx (PresenceVar x) = do
  p <- classDesc (TySubPresence (PresenceVar x)) >>= ensurePresence
  if p == PresenceVar x
    then return (PresenceVar x)
    else describePresence allowMu ctx p
describePresence allowMu ctx (PresenceVarWithType x t) = do
  pt <- describePresenceWithType allowMu ctx (PresenceWithTypeVar x)
  case pt of
    PresenceWithTypeAbsent  -> return Absent
    PresenceWithTypePresent -> Present <$> describeProper allowMu ctx t
    PresenceWithTypeVar x' ->
      PresenceVarWithType x' <$> describeProper allowMu ctx t

describePresenceWithType
  :: Bool -> Set.Set VarName -> PresenceWithType -> Inference PresenceWithType
describePresenceWithType _ _ PresenceWithTypeAbsent =
  return PresenceWithTypeAbsent
describePresenceWithType _ _ PresenceWithTypePresent =
  return PresenceWithTypePresent
describePresenceWithType allowMu ctx (PresenceWithTypeVar x) = do
  pt <-
    classDesc (TySubPresenceWithType (PresenceWithTypeVar x))
      >>= ensurePresenceWithType
  if pt == PresenceWithTypeVar x
    then return (PresenceWithTypeVar x)
    else describePresenceWithType allowMu ctx pt

describeRow :: Bool -> Set.Set VarName -> TypeRow -> Inference TypeRow
describeRow _ _ RowEmpty                          = return RowEmpty
describeRow _ ctx (RowVar x) | x `Set.member` ctx = return (RowVar x)
describeRow allowMu ctx (RowVar x)                = do
  r <- classDesc (TySubRow (RowVar x)) >>= ensureRow
  if r == RowVar x
    then return (RowVar x)
    else do
      r' <- describeRow allowMu (Set.insert x ctx) r
      fv <- liftEither (fvRow r')
      case Map.lookup x fv of
        Nothing   -> return r'
        Just KRow -> return (RowMu x r')
        Just k    -> throwError (ErrVarKindConflict x KRow k)
describeRow allowMu ctx (RowPresence l p r) =
  RowPresence l <$> describePresence allowMu ctx p <*> describeRow allowMu ctx r
describeRow allowMu ctx (RowMu x r) = if allowMu
  then RowMu x <$> describeRow allowMu (Set.insert x ctx) r
  else throwError (ErrCanNotHandleMuRow (RowMu x r))

-- regTreeEq' :: Type -> Type -> Bool
-- regTreeEq' t1 t2 = case runState (runMaybeT (regTreeEq t1 t2)) Set.empty of
--   (Nothing, _) -> False
--   (Just (), _) -> True

-- regTreeNeq' :: Type -> Type -> Bool
-- regTreeNeq' t1 t2 = not (regTreeEq' t1 t2)

-- regTreeEq
--   :: (Monad m) => Type -> Type -> MaybeT (StateT (Set.Set (Type, Type)) m) ()
-- regTreeEq t1 t2 = gets (Set.member (t1, t2)) >>= \case
--   True  -> return ()
--   False -> modify (Set.insert (t1, t2)) >> case (t1, t2) of
--     (TyBool, TyBool) -> return ()
--     (TyNat , TyNat ) -> return ()
--     (TyArrow t11 t12, TyArrow t21 t22) ->
--       regTreeEq t11 t21 >> regTreeEq t12 t22
--     (TyRef     t1', TyRef t2'   )   -> regTreeEq t1' t2'
--     (TyRecord  r1 , TyRecord r2 )   -> regTreeEqRow r1 r2
--     (TyVariant r1 , TyVariant r2)   -> regTreeEqRow r1 r2
--     (TyVar x1, TyVar x2) | x1 == x2 -> return ()
--     (TyMu x1 t12, _) ->
--       regTreeEq (applySubst (Map.singleton x1 (TySubProper t1)) t12) t2
--     (_, TyMu x2 t22) ->
--       regTreeEq t1 (applySubst (Map.singleton x2 (TySubProper t2)) t22)
--     _ -> mzero

-- regTreeEqRow
--   :: (Monad m)
--   => TypeRow
--   -> TypeRow
--   -> MaybeT (StateT (Set.Set (Type, Type)) m) ()
-- regTreeEqRow (TyRow f1 cof1) (TyRow f2 cof2)
--   | Map.keysSet f1 == Map.keysSet f2
--   = sequence_ (Map.intersectionWith regTreeEqPresence f1 f2)
--     >> case (cof1, cof2) of
--          (CofAllAbsent, CofAllAbsent) -> return ()
--          (CofRowVar x1, CofRowVar x2) | x1 == x2 -> return ()
--          _                            -> mzero
--   | otherwise
--   = mzero

-- regTreeEqPresence
--   :: (Monad m)
--   => TypePresence
--   -> TypePresence
--   -> MaybeT (StateT (Set.Set (Type, Type)) m) ()
-- regTreeEqPresence p1 p2 = case (p1, p2) of
--   (Absent    , Absent    )                    -> return ()
--   (Present t1, Present t2)                    -> regTreeEq t1 t2
--   (PresenceVar x1, PresenceVar x2) | x1 == x2 -> return ()
--   (PresenceVarWithType x1 t1, PresenceVarWithType x2 t2) | x1 == x2 ->
--     regTreeEq t1 t2
--   _ -> mzero
