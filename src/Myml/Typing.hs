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
  -- unify
  , unifyProper
  , unifyPresenceWithType
  , unifyPresence
  , unifyRow
  -- describe
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

innerVarPrefix :: VarName
innerVarPrefix = "\x03b2"

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
  return (TyRecord (foldr (uncurry RowPresence) RowEmpty (Map.toList ps)))
 where
  genPresence t = PresenceVarWithType <$> newVar innerVarPrefix <*> infer t
infer (TmRcdExtend t1 l t2) = do
  ty1 <- infer t1
  ty2 <- infer t2
  p1  <- PresenceVar <$> newVar innerVarPrefix
  r   <- RowVar <$> newVar innerVarPrefix
  unifyProper ty1 (TyRecord (RowPresence l p1 r))
  p2 <- newVar innerVarPrefix
  return (TyRecord (RowPresence l (PresenceVarWithType p2 ty2) r))
infer (TmRcdAccess t l) = do
  ty <- infer t
  x  <- TyVar <$> newVar innerVarPrefix
  r  <- RowVar <$> newVar innerVarPrefix
  unifyProper ty (TyRecord (RowPresence l (Present x) r))
  return x
infer (TmMatch m) = do
  m'  <- sequence (Map.map genSingle m)
  res <- TyVar <$> newVar innerVarPrefix
  sequence_ (Map.map (unifyProper res . snd) m')
  return
    (TyArrow
      (TyVariant
        (foldr (\(l, (p, _)) -> RowPresence l p) RowEmpty (Map.toList m'))
      )
      res
    )
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
  r   <- RowVar <$> newVar innerVarPrefix
  unifyProper ty1 (TyArrow (TyVariant (RowPresence l p1 r)) ty2)
  p2 <- newVar innerVarPrefix
  return
    (TyArrow (TyVariant (RowPresence l (PresenceVarWithType p2 tyX) r)) ty2)
infer (TmVariant l t) = do
  ty <- infer t
  r  <- RowVar <$> newVar innerVarPrefix
  return (TyVariant (RowPresence l (Present ty) r))
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
infer (TmLoc _    ) = throwError ErrStoreTypingNotImplemented
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
  s' <- liftEither (substType (Map.singleton x (TySubProper (TyVar nv))) s)
  instantiate s'
instantiate (ScmForall x KPresence s) = do
  nv <- newVar innerVarPrefix
  s' <- liftEither
    (substType (Map.singleton x (TySubPresence (PresenceVar nv))) s)
  instantiate s'
instantiate (ScmForall x (KArrow KProper KPresence) s) = do
  nv <- newVar innerVarPrefix
  s' <- liftEither
    (substType
      (Map.singleton x (TySubPresenceWithType (PresenceWithTypeVar nv)))
      s
    )
  instantiate s'
instantiate (ScmForall x KRow s) = do
  nv <- newVar innerVarPrefix
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
instantiateType TyUnit    = return TyUnit
instantiateType TyBool    = return TyBool
instantiateType TyNat     = return TyNat

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
  tFv    <- liftEither (fvType tyDesc)
  env    <- ask
  envFv  <-
    (Map.foldl
      (\a b -> bind2AndLift
        mapUnionWithKind
        a
        (describeScheme True Set.empty b >>= liftEither . fvScheme)
      )
      (return Map.empty)
      env
    )
  -- check kind conflicts
  _          <- liftEither (mapUnionWithKind tFv envFv)
  imperative <- gets imperativeFeaturesEnabled
  let xs = tFv `Map.difference` envFv
  xs' <- if imperative && not (isValue t)
    then liftEither (dangerousVariable tyDesc >>= mapDiffWithKind xs)
    else return xs
  (sub, newXs) <- replacePrefix xs'
  tyDesc'      <- liftEither (substType sub tyDesc)
  return (Map.foldrWithKey ScmForall (ScmMono tyDesc') newXs)
 where
  bind2AndLift f ma mb = do
    a <- ma
    b <- mb
    liftEither (f a b)

dangerousVariable :: Type -> Either Error (Map.Map VarName Kind)
dangerousVariable = fvType -- traditional value restriction
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
kindToPrefix KProper           = "\x03b1"
kindToPrefix KPresence         = "\x03c6"
kindToPrefix KPresenceWithType = "\x03c8"
kindToPrefix KRow              = "\x03c1"
kindToPrefix k = error ("Unknown kind for prefix" ++ show (pretty k))

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
unifyProper' TyUnit         TyUnit         = return ()
unifyProper' TyBool         TyBool         = return ()
unifyProper' TyNat          TyNat          = return ()
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
  throwError (ErrUnifyNoRuleApplied (TySubPresence p1) (TySubPresence p2))

unifyRow :: TypeRow -> TypeRow -> Inference ()
unifyRow r1 r2 = do
  r1' <- classDesc (TySubRow r1) >>= ensureRow
  r2' <- classDesc (TySubRow r2) >>= ensureRow
  unifyRow' r1' r2'

unifyRow' :: TypeRow -> TypeRow -> Inference ()
unifyRow' RowEmpty RowEmpty              = return ()
unifyRow' RowEmpty x@(RowVar _         ) = unifyRow' x RowEmpty
unifyRow' RowEmpty r@(RowPresence _ _ _) = unifyRow' r RowEmpty
unifyRow' (RowVar x) RowEmpty =
  equate (TySubRow (RowVar x)) (TySubRow RowEmpty)
unifyRow' (RowVar x1) (RowVar x2)
  | x1 == x2  = return ()
  | otherwise = equate (TySubRow (RowVar x1)) (TySubRow (RowVar x2))
unifyRow' (RowVar x) r@(RowPresence _ _ _) =
  equate (TySubRow (RowVar x)) (TySubRow r)
unifyRow' (RowPresence _l p r) RowEmpty =
  unifyPresence p Absent >> unifyRow r RowEmpty
unifyRow' r@(RowPresence _  _  _ ) x@(RowVar _            ) = unifyRow' x r
unifyRow' (  RowPresence l1 p1 r1) (  RowPresence l2 p2 r2) = if l1 == l2
  then unifyPresence p1 p2 >> unifyRow r1 r2
  else do
    r3 <- newVar innerVarPrefix
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
describeProper allowMu ctx (TyRef t)  = TyRef <$> describeProper allowMu ctx t
describeProper _       _   TyUnit     = return TyUnit
describeProper _       _   TyBool     = return TyBool
describeProper _       _   TyNat      = return TyNat
describeProper allowMu ctx (TyMu x t) = if allowMu
  then TyMu x <$> describeProper allowMu (Set.insert x ctx) t
  else throwError (ErrCanNotHandleMuType (TyMu x t))

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
  p <-
    classDesc (TySubPresenceWithType (PresenceWithTypeVar x))
      >>= ensurePresenceWithType
  if p == PresenceWithTypeVar x
    then PresenceVarWithType x <$> describeProper allowMu ctx t
    else case p of
      PresenceWithTypeAbsent  -> return Absent
      PresenceWithTypePresent -> Present <$> describeProper allowMu ctx t
      PresenceWithTypeVar x' ->
        PresenceVarWithType x' <$> describeProper allowMu ctx t

describeRow :: Bool -> Set.Set VarName -> TypeRow -> Inference TypeRow
describeRow _ _ RowEmpty                          = return RowEmpty
describeRow _ ctx (RowVar x) | x `Set.member` ctx = return (RowVar x)
describeRow allowMu ctx (RowVar x)                = do
  r <- classDesc (TySubRow (RowVar x)) >>= ensureRow
  if r == (RowVar x)
    then return (RowVar x)
    else do
      r' <- describeRow allowMu (Set.insert x ctx) r
      fv <- liftEither (fvRow r')
      case Map.lookup x fv of
        Nothing   -> return r'
        Just KRow -> return (RowMu x r')
        Just k    -> throwError (ErrVarKindConflict x KRow k)
describeRow allowMu ctx (RowPresence l p r) =
  RowPresence l
    <$> (describePresence allowMu ctx p)
    <*> describeRow allowMu ctx r
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
--     (TyUnit, TyUnit) -> return ()
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
