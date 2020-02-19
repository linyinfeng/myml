{-# LANGUAGE LambdaCase, RankNTypes #-}

module Myml.Typing
  ( TypingExcept(..)
  , NewVar(..)
  , TypingEnv
  , Constraint
  , ConstraintGen
  , GenState(..)
  , Solve
  , SolveState(..)
  , runConstraintGen
  , runSolve
  , newVarSolve
  , newVarGen
  , instantiate
  , generalize
  , constraintGen
  , solve
  , unify
  , unifyProper
  , unifyPresenceWithType
  , unifyPresence
  , unifyRow
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
import           Control.Monad.RWS
import           Data.Text.Prettyprint.Doc
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

data TypingExcept = ExcUnifyKindMismatch Kind Kind
                  | ExcUnifyNoRuleApplied TypeSubstitutor TypeSubstitutor
                  | ExcUnifyRowLabelCollided LabelName
                  | ExcGenUnboundedVariable VarName
                  | ExcFreeVarKindConflict VarName Kind Kind
                  | ExcStoreTypingNotImplemented

newtype NewVar = NewVar (Map.Map VarName Integer)

type TypingEnv = Map.Map VarName TypeScheme

type Constraint = (Type, Type)

type ConstraintGen a
  = ExceptT TypingExcept (RWS TypingEnv [Constraint] GenState) a

newtype GenState = GenState { genStateNewVar :: NewVar }

type Solve a
  =  forall s
   . ExceptT
    TypingExcept
    (EquivT s TypeSubstitutor TypeSubstitutor (State SolveState))
    a

data SolveState = SolveState { solveStateNewVar :: NewVar, solveStateUnified :: Set.Set (Type, Type) }

runConstraintGen :: ConstraintGen a -> TypingEnv -> GenState -> (Either TypingExcept a, GenState, [Constraint])
runConstraintGen c = runRWS (runExceptT c)

runSolve :: Solve a -> SolveState -> (Either TypingExcept a, SolveState)
runSolve m = runState (runEquivT id (flip const) (runExceptT m))

newVarSolve :: VarName -> Solve VarName
newVarSolve prefix = do
  NewVar m <- gets solveStateNewVar
  let i = fromMaybe 0 (Map.lookup prefix m)
  modify (\s -> s { solveStateNewVar = NewVar (Map.insert prefix (i + 1) m) })
  return (prefix ++ show i)

newVarGen :: VarName -> ConstraintGen VarName
newVarGen prefix = do
  NewVar m <- gets genStateNewVar
  let i = fromMaybe 0 (Map.lookup prefix m)
  modify (\s -> s { genStateNewVar = NewVar (Map.insert prefix (i + 1) m) })
  return (prefix ++ show i)

properPrefix :: VarName
properPrefix = "'\x03b1"

presencePrefix :: VarName
presencePrefix = "'\x03c6"

presenceWithTypePrefix :: VarName
presenceWithTypePrefix = "'\x03c8"

rowPrefix :: VarName
rowPrefix = "'\x03c1"

constraintGen :: Term -> ConstraintGen Type
constraintGen (TmVar x) = reader (Map.lookup x) >>= \case
  Nothing -> throwError (ExcGenUnboundedVariable x)
  Just s  -> instantiate s
constraintGen (TmApp t1 t2) = do
  ty1 <- constraintGen t1
  ty2 <- constraintGen t2
  nv  <- TyVar <$> newVarGen properPrefix
  tell [(ty1, TyArrow ty2 nv)]
  return nv
constraintGen (TmAbs x t) = do
  nv <- TyVar <$> newVarGen properPrefix
  ty <- local (Map.insert x (ScmMono nv)) (constraintGen t)
  return (TyArrow nv ty)
constraintGen (TmLet x t1 t2) = do
  ty1 <- constraintGen t1
  s   <- generalize ty1
  local (Map.insert x s) (constraintGen t2)
constraintGen (TmRcd m) = do
  ps  <- sequence (Map.map genPresence m)
  row <- CofRowVar <$> newVarGen rowPrefix
  return (TyRecord (TyRow ps row))
 where
  genPresence t =
    PresenceVarWithType <$> newVarGen presenceWithTypePrefix <*> constraintGen t
constraintGen (TmRcdExtend t1 l t2) = do
  ty1 <- constraintGen t1
  ty2 <- constraintGen t2
  p1  <- PresenceVar <$> newVarGen presencePrefix
  r   <- CofRowVar <$> newVarGen rowPrefix
  tell [(ty1, TyRecord (TyRow (Map.singleton l p1) r))]
  p2 <- newVarGen presenceWithTypePrefix
  return (TyRecord (TyRow (Map.singleton l (PresenceVarWithType p2 ty2)) r))
constraintGen (TmRcdAccess t l) = do
  ty <- constraintGen t
  x  <- TyVar <$> newVarGen properPrefix
  r  <- CofRowVar <$> newVarGen rowPrefix
  tell [(ty, TyRecord (TyRow (Map.singleton l (Present x)) r))]
  return x
constraintGen (TmMatch m) = do
  m'  <- sequence (Map.map genSingle m)
  res <- TyVar <$> newVarGen properPrefix
  sequence_ (Map.map (\a -> tell [(res, snd a)]) m')
  return (TyArrow (TyVariant (TyRow (Map.map fst m') CofAllAbsent)) res)
 where
  genSingle (TmCase x t) = do
    nv <- TyVar <$> newVarGen properPrefix
    ty <- local (Map.insert x (ScmMono nv)) (constraintGen t)
    p  <- newVarGen presenceWithTypePrefix
    return (PresenceVarWithType p nv, ty)
constraintGen (TmMatchExtend t1 l (TmCase x t2)) = do
  ty1 <- constraintGen t1
  tyX <- TyVar <$> newVarGen properPrefix
  ty2 <- local (Map.insert x (ScmMono tyX)) (constraintGen t2)
  p1  <- PresenceVar <$> newVarGen presencePrefix
  r   <- CofRowVar <$> newVarGen rowPrefix
  tell [(ty1, TyArrow (TyVariant (TyRow (Map.singleton l p1) r)) ty2)]
  p2 <- newVarGen presenceWithTypePrefix
  return
    (TyArrow
      (TyVariant (TyRow (Map.singleton l (PresenceVarWithType p2 tyX)) r))
      ty2
    )
constraintGen (TmVariant l t) = do
  ty <- constraintGen t
  r  <- CofRowVar <$> newVarGen rowPrefix
  return (TyVariant (TyRow (Map.singleton l (Present ty)) r))
constraintGen (TmRef   t) = TyRef <$> constraintGen t
constraintGen (TmDeref t) = do
  ty <- constraintGen t
  x  <- TyVar <$> newVarGen properPrefix
  tell [(ty, TyRef x)]
  return x
constraintGen (TmAssign t1 t2) = do
  ty1 <- constraintGen t1
  ty2 <- constraintGen t2
  tell [(ty1, TyRef ty2)]
  return TyUnit
constraintGen (TmLoc _    ) = throwError ExcStoreTypingNotImplemented
constraintGen (TmSeq t1 t2) = do
  _ <- constraintGen t1
  constraintGen t2
constraintGen TmUnit          = return TyUnit
constraintGen TmTrue          = return TyBool
constraintGen TmFalse         = return TyBool
constraintGen (TmIf t1 t2 t3) = do
  ty1 <- constraintGen t1
  ty2 <- constraintGen t2
  ty3 <- constraintGen t3
  tell [(ty1, TyBool)]
  tell [(ty2, ty3)]
  return ty2
constraintGen TmZero     = return TyNat
constraintGen (TmSucc t) = do
  ty <- constraintGen t
  tell [(ty, TyNat)]
  return TyNat

instantiate :: TypeScheme -> ConstraintGen Type
instantiate (ScmForall x KProper s) = do
  nv <- newVarGen properPrefix
  let s' = applySubst (Map.singleton x (TySubProper (TyVar nv))) s
  instantiate s'
instantiate (ScmForall x KPresence s) = do
  nv <- newVarGen presencePrefix
  let s' = applySubst (Map.singleton x (TySubPresence (PresenceVar nv))) s
  instantiate s'
instantiate (ScmForall x (KArrow KProper KPresence) s) = do
  nv <- newVarGen presenceWithTypePrefix
  let s' = applySubst
        (Map.singleton x (TySubPresenceWithType (PresenceWithTypeVar nv)))
        s
  instantiate s'
instantiate (ScmForall x (KRow _) s) = do
  nv <- newVarGen rowPrefix
  let
    s' =
      applySubst (Map.singleton x (TySubRow (TyRow Map.empty (CofRowVar nv)))) s
  instantiate s'
instantiate (ScmForall _ k _) = error ("Unknown kind: " ++ show (pretty k))
instantiate (ScmMono t      ) = return t

generalize :: Type -> ConstraintGen TypeScheme
generalize t = do
  env   <- ask
  envFv <- liftEither
    (Map.foldl (\a b -> bind2 mergeFvWithKind a (fvWithKindScm b))
               (Right Map.empty)
               env
    )
  tFv <- liftEither (fvWithKind t)
  liftEither
    (sequence_ (Map.intersectionWithKey mergeFvWithKindSingle tFv envFv))
  let xs = tFv `Map.difference` envFv
  return (Map.foldrWithKey ScmForall (ScmMono t) xs)

mergeFvWithKind
  :: Map.Map VarName Kind
  -> Map.Map VarName Kind
  -> Either TypingExcept (Map.Map VarName Kind)
mergeFvWithKind m1 m2 = (\updated -> updated `Map.union` m1 `Map.union` m2)
  <$> sequence (Map.intersectionWithKey mergeFvWithKindSingle m1 m2)

mergeFvWithKindSingle :: VarName -> Kind -> Kind -> Either TypingExcept Kind
mergeFvWithKindSingle x a b = case (a, b) of
  (KRow l1, KRow l2) -> return (KRow (l1 `Set.union` l2))
  (k1, k2) | k1 == k2  -> return k1
           | otherwise -> Left (ExcFreeVarKindConflict x k1 k2)

bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 f ma mb = do
  a <- ma
  b <- mb
  f a b

fvWithKind :: Type -> Either TypingExcept (Map.Map VarName Kind)
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
    Just k       -> Left (ExcFreeVarKindConflict x k KProper)
fvWithKind (TyRef t) = fvWithKind t
fvWithKind TyUnit    = return Map.empty
fvWithKind TyBool    = return Map.empty
fvWithKind TyNat     = return Map.empty

fvWithKindScm :: TypeScheme -> Either TypingExcept (Map.Map VarName Kind)
fvWithKindScm (ScmForall x k s) = do
  m <- fvWithKindScm s
  case Map.lookup x m of
    Nothing -> return m
    Just k' | k == k'   -> return (Map.delete x m)
            | otherwise -> Left (ExcFreeVarKindConflict x k k')
fvWithKindScm (ScmMono t) = fvWithKind t

fvWithKindRow :: TypeRow -> Either TypingExcept (Map.Map VarName Kind)
fvWithKindRow (TyRow f cof) = do
  fvF <- Map.foldl (\a b -> bind2 mergeFvWithKind a (fvWithKindPresence b))
                   (Right Map.empty)
                   f
  let fvCof = case cof of
        CofAllAbsent -> Map.empty
        CofRowVar x  -> Map.singleton x (KRow (Map.keysSet f))
  mergeFvWithKind fvF fvCof

fvWithKindPresence :: TypePresence -> Either TypingExcept (Map.Map VarName Kind)
fvWithKindPresence Absent          = return Map.empty
fvWithKindPresence (Present     t) = fvWithKind t
fvWithKindPresence (PresenceVar x) = return (Map.singleton x KPresence)
fvWithKindPresence (PresenceVarWithType x t) =
  fvWithKind t >>= mergeFvWithKind (Map.singleton x (KArrow KProper KPresence))

solve :: [Constraint] -> Solve ()
solve = mapM_ (uncurry unifyProper)

unify :: TypeSubstitutor -> TypeSubstitutor -> Solve ()
unify (TySubProper t1) (TySubProper t2) = unifyProper t1 t2
unify (TySubPresenceWithType p1) (TySubPresenceWithType p2) =
  unifyPresenceWithType p1 p2
unify (TySubPresence p1) (TySubPresence p2) = unifyPresence p1 p2
unify (TySubRow      r1) (TySubRow      r2) = unifyRow r1 r2
unify t1                 t2                 = throwError
  (ExcUnifyKindMismatch (kindOfSubstitutor t1) (kindOfSubstitutor t2))

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

unifyProper :: Type -> Type -> Solve ()
unifyProper t1 t2 = do
  t1'     <- classDesc (TySubProper t1) >>= ensureProper
  t2'     <- classDesc (TySubProper t2) >>= ensureProper
  unified <- gets (Set.member (t1', t2') . solveStateUnified)
  if unified then return () else unifyProper' t1' t2'

unifyProper' :: Type -> Type -> Solve ()
unifyProper' (TyArrow t11 t12) (TyArrow t21 t22) =
  unifyProper t11 t21 >> unifyProper t12 t22
unifyProper' (TyRecord  r1) (TyRecord  r2) = unifyRow r1 r2
unifyProper' (TyVariant r1) (TyVariant r2) = unifyRow r1 r2
unifyProper' (TyRef     t1) (TyRef     t2) = unifyProper t1 t2
unifyProper' TyUnit         TyUnit         = return ()
unifyProper' TyBool         TyBool         = return ()
unifyProper' TyNat          TyNat          = return ()
unifyProper' (TyVar x1) t2 = equate (TySubProper (TyVar x1)) (TySubProper t2)
unifyProper' t1 (TyVar x2) = equate (TySubProper (TyVar x2)) (TySubProper t1)
unifyProper' t1@(TyMu x1 t12) t2 =
  unifyProper (applySubst (Map.singleton x1 (TySubProper t1)) t12) t2
unifyProper' t1 t2@(TyMu x2 t22) =
  unifyProper t1 (applySubst (Map.singleton x2 (TySubProper t2)) t22)
unifyProper' t1 t2 =
  throwError (ExcUnifyNoRuleApplied (TySubProper t1) (TySubProper t2))

unifyPresenceWithType :: PresenceWithType -> PresenceWithType -> Solve ()
unifyPresenceWithType p1 p2 = do
  p1' <- classDesc (TySubPresenceWithType p1) >>= ensurePresenceWithType
  p2' <- classDesc (TySubPresenceWithType p2) >>= ensurePresenceWithType
  unifyPresenceWithType' p1' p2'

unifyPresenceWithType' :: PresenceWithType -> PresenceWithType -> Solve ()
unifyPresenceWithType' PresenceWithTypeAbsent PresenceWithTypeAbsent =
  return ()
unifyPresenceWithType' PresenceWithTypePresent PresenceWithTypePresent =
  return ()
unifyPresenceWithType' (PresenceWithTypeVar x1) p2 = equate
  (TySubPresenceWithType (PresenceWithTypeVar x1))
  (TySubPresenceWithType p2)
unifyPresenceWithType' p1 (PresenceWithTypeVar x2) = equate
  (TySubPresenceWithType (PresenceWithTypeVar x2))
  (TySubPresenceWithType p1)
unifyPresenceWithType' p1 p2 = throwError
  (ExcUnifyNoRuleApplied (TySubPresenceWithType p1) (TySubPresenceWithType p2))

unifyPresence :: TypePresence -> TypePresence -> Solve ()
unifyPresence p1 p2 = do
  p1' <- classDesc (TySubPresence p1) >>= ensurePresence
  p2' <- classDesc (TySubPresence p2) >>= ensurePresence
  unifyPresence' p1' p2'

unifyPresence' :: TypePresence -> TypePresence -> Solve ()
unifyPresence' Absent       Absent       = return ()
unifyPresence' (Present t1) (Present t2) = unifyProper t1 t2
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

rowDesc :: TypeRow -> Solve TypeRow
rowDesc (TyRow f CofAllAbsent ) = return (TyRow f CofAllAbsent)
rowDesc (TyRow f (CofRowVar x)) = do
  (TyRow f' inf) <-
    classDesc (TySubRow (TyRow Map.empty (CofRowVar x))) >>= ensureRow
  if TyRow f' inf == TyRow Map.empty (CofRowVar x)
    then return (TyRow f (CofRowVar x))
    else
      let
        rf   = Map.map return f
        rf'  = Map.map return f'
        newF = sequence
          (Map.unionWithKey
            (\k _ _ -> throwError (ExcUnifyRowLabelCollided k))
            rf
            rf'
          )
      in
        flip TyRow inf <$> newF >>= rowDesc

unifyRow :: TypeRow -> TypeRow -> Solve ()
unifyRow r1 r2 = do
  r1' <- rowDesc r1
  r2' <- rowDesc r2
  unifyRow' r1' r2'

unifyRow' :: TypeRow -> TypeRow -> Solve ()
unifyRow' (TyRow f1 cof1) (TyRow f2 cof2) = do
  -- handle intersection part
  sequence_ (Map.intersectionWith unifyPresence f1 f2)
  let f1'    = f1 `Map.difference` f2
      f2'    = f2 `Map.difference` f1
      error' = throwError
        (ExcUnifyNoRuleApplied (TySubRow (TyRow f1' cof1))
                               (TySubRow (TyRow f2' cof2))
        )
  case (Map.null f1', Map.null f2') of
    (True, True) -> case (cof1, cof2) of
      (CofAllAbsent, CofAllAbsent) -> return ()
      (CofRowVar x1, _) -> equate (TySubRow (TyRow Map.empty (CofRowVar x1)))
                                  (TySubRow (TyRow Map.empty cof2))
      (_, CofRowVar x2) -> equate (TySubRow (TyRow Map.empty (CofRowVar x2)))
                                  (TySubRow (TyRow Map.empty cof1))
    (True, False) -> case cof1 of
      CofAllAbsent -> error'
      CofRowVar x1 -> equate (TySubRow (TyRow Map.empty (CofRowVar x1)))
                             (TySubRow (TyRow f2' cof2))
    (False, True) -> case cof2 of
      CofAllAbsent -> error'
      CofRowVar x2 -> equate (TySubRow (TyRow Map.empty (CofRowVar x2)))
                             (TySubRow (TyRow f2' cof2))
    (False, False) -> case (cof1, cof2) of
      (CofAllAbsent, _           ) -> error'
      (_           , CofAllAbsent) -> error'
      (CofRowVar x1, CofRowVar x2) -> do
        newX <- newVarSolve rowPrefix
        equate (TySubRow (TyRow Map.empty (CofRowVar x1)))
               (TySubRow (TyRow f2' (CofRowVar newX)))
        equate (TySubRow (TyRow Map.empty (CofRowVar x2)))
               (TySubRow (TyRow f1' (CofRowVar newX)))

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
