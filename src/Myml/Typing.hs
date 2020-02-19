{-# LANGUAGE LambdaCase, RankNTypes #-}

module Myml.Typing
  ( TypingExcept
  , NewVar
  , newVar
  , TypingEnv(..)
  , Typing
  , runTyping
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
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

data TypingExcept = ExcUnifyKindMismatch Kind Kind
                  | ExcUnifyNoRuleApplied TypeSubstitutor TypeSubstitutor
                  | ExcUnifyRowLabelCollided String

data NewVar = NewVar String Integer

newVar :: (Monad m) => Typing s m String
newVar = do
  NewVar prefix i <- get
  let v = prefix ++ show i
  put (NewVar prefix (i + 1))
  return v

newtype TypingEnv = TypingEnv (Map.Map String TypeScheme)

type Typing s m a
  = ExceptT
      TypingExcept
      ( EquivT
          s
          TypeSubstitutor
          TypeSubstitutor
          (ReaderT TypingEnv (StateT NewVar m))
      )
      a

runTyping
  :: (Monad m)
  => TypingEnv
  -> NewVar
  -> (forall s . Typing s m a)
  -> m (Either TypingExcept a, NewVar)
runTyping initEnv initNewVar t =
  runStateT (runReaderT (runEquivT id const (runExceptT t)) initEnv) initNewVar

unify :: (Monad m) => TypeSubstitutor -> TypeSubstitutor -> Typing s m ()
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

unifyProper :: (Monad m) => Type -> Type -> Typing s m ()
unifyProper t1 t2 = do
  t1' <- classDesc (TySubProper t1) >>= ensureProper
  t2' <- classDesc (TySubProper t2) >>= ensureProper
  unifyProper' t1' t2'

unifyProper' :: (Monad m) => Type -> Type -> Typing s m ()
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
unifyProper' t1 t2 =
  throwError (ExcUnifyNoRuleApplied (TySubProper t1) (TySubProper t2))

unifyPresenceWithType
  :: (Monad m) => PresenceWithType -> PresenceWithType -> Typing s m ()
unifyPresenceWithType p1 p2 = do
  p1' <- classDesc (TySubPresenceWithType p1) >>= ensurePresenceWithType
  p2' <- classDesc (TySubPresenceWithType p2) >>= ensurePresenceWithType
  unifyPresenceWithType' p1' p2'

unifyPresenceWithType'
  :: (Monad m) => PresenceWithType -> PresenceWithType -> Typing s m ()
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

unifyPresence :: (Monad m) => TypePresence -> TypePresence -> Typing s m ()
unifyPresence p1 p2 = do
  p1' <- classDesc (TySubPresence p1) >>= ensurePresence
  p2' <- classDesc (TySubPresence p2) >>= ensurePresence
  unifyPresence' p1' p2'

unifyPresence' :: (Monad m) => TypePresence -> TypePresence -> Typing s m ()
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

rowDesc :: (Monad m) => TypeRow -> Typing s m TypeRow
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

unifyRow :: (Monad m) => TypeRow -> TypeRow -> Typing s m ()
unifyRow r1 r2 = do
  r1' <- rowDesc r1
  r2' <- rowDesc r2
  unifyRow' r1' r2'

unifyRow' :: (Monad m) => TypeRow -> TypeRow -> Typing s m ()
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
        newX <- newVar
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
