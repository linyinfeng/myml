{-# LANGUAGE LambdaCase #-}

module Myml.Subst
  ( substTerm
  , TypeSubst(..)
  , compositeTypeSubst
  , compositeTermSubst
  )
where

import           Myml.Syntax
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Maybe                     ( fromMaybe )
import           Data.Text.Prettyprint.Doc

substTerm :: Map.Map VarName Term -> Term -> Term
substTerm s t = runReader (substTerm' t) s

substTerm' :: Term -> Reader (Map.Map VarName Term) Term
substTerm' (TmAbs x t) =
  handleTermBind x >>= \(newX, inner) -> TmAbs newX <$> inner (substTerm' t)
substTerm' (TmApp t1 t2) = TmApp <$> substTerm' t1 <*> substTerm' t2
substTerm' (TmVar x) =
  ask >>= \s -> return (fromMaybe (TmVar x) (Map.lookup x s))
substTerm' (TmLet x t1 t2) = do
  t1'           <- substTerm' t1
  (newX, inner) <- handleTermBind x
  TmLet newX t1' <$> inner (substTerm' t2)
substTerm' t = return t

compositeTermSubst
  :: Map.Map VarName Term -> Map.Map VarName Term -> Map.Map VarName Term
compositeTermSubst sa sb = Map.map (substTerm sa) sb `Map.union` sa

class TypeSubst a where
  substType :: Map.Map VarName TypeSubstitutor -> a -> Either Error a
  substType s t = runReader (runExceptT (substType' t)) s
  substType' :: a -> ExceptT Error (Reader (Map.Map VarName TypeSubstitutor)) a

compositeTypeSubst
  :: Map.Map VarName TypeSubstitutor
  -> Map.Map VarName TypeSubstitutor
  -> Either Error (Map.Map VarName TypeSubstitutor)
compositeTypeSubst sa sb =
  flip Map.union sa <$> sequence (Map.map (substType sa) sb)

instance TypeSubst TypeSubstitutor where
  substType' (TySubProper           t) = TySubProper <$> substType' t
  substType' (TySubPresenceWithType p) = TySubPresenceWithType <$> substType' p
  substType' (TySubPresence         p) = TySubPresence <$> substType' p
  substType' (TySubRow              r) = TySubRow <$> substType' r

instance TypeSubst Type where
  substType' (TyVar x) = asks (Map.lookup x) >>= \case
    Nothing -> return (TyVar x)
    Just (TySubProper t) -> return t
    Just s -> throwError (ErrVarKindConflict x KProper (kindOfTySub s))
  substType' (TyArrow t1 t2) = TyArrow <$> substType' t1 <*> substType' t2
  substType' (TyRecord  row) = TyRecord <$> substType' row
  substType' (TyVariant row) = TyVariant <$> substType' row
  substType' (TyMu x t     ) = handleTypeBind x (TySubProper . TyVar)
    >>= \(newX, inner) -> TyMu newX <$> inner (substType' t)
  substType' (TyRef t) = TyRef <$> substType' t
  substType' TyInteger = return TyInteger
  substType' TyChar    = return TyChar

instance TypeSubst TypeRow where
  substType' RowEmpty   = return RowEmpty
  substType' (RowVar x) = asks (Map.lookup x) >>= \case
    Nothing           -> return (RowVar x)
    Just (TySubRow r) -> return r
    Just s            -> throwError (ErrVarKindConflict x KRow (kindOfTySub s))
  substType' (RowPresence l p r) =
    RowPresence l <$> substType' p <*> substType' r
  substType' (RowMu x r) = handleTypeBind x (TySubRow . RowVar)
    >>= \(newX, inner) -> RowMu newX <$> inner (substType' r)

instance TypeSubst TypePresence where
  substType' Absent                    = return Absent
  substType' (Present t              ) = Present <$> substType' t
  substType' (PresenceVarWithType x t) = asks (Map.lookup x) >>= \case
    Nothing                        -> PresenceVarWithType x <$> substType' t
    Just (TySubPresenceWithType p) -> case p of
      PresenceWithTypeAbsent  -> return Absent
      PresenceWithTypePresent -> Present <$> substType' t
      PresenceWithTypeVar x'  -> PresenceVarWithType x' <$> substType' t
    Just s ->
      throwError (ErrVarKindConflict x KPresenceWithType (kindOfTySub s))
  substType' (PresenceVar x) = asks (Map.lookup x) >>= \case
    Nothing -> return (PresenceVar x)
    Just (TySubPresence p) -> return p
    Just s -> throwError (ErrVarKindConflict x KPresence (kindOfTySub s))

instance TypeSubst PresenceWithType where
  substType' PresenceWithTypeAbsent  = return PresenceWithTypeAbsent
  substType' PresenceWithTypePresent = return PresenceWithTypePresent
  substType' (PresenceWithTypeVar x) = asks (Map.lookup x) >>= \case
    Nothing                        -> return (PresenceWithTypeVar x)
    Just (TySubPresenceWithType i) -> return i
    Just s ->
      throwError (ErrVarKindConflict x KPresenceWithType (kindOfTySub s))

instance TypeSubst TypeScheme where
  substType' (ScmForall x KProper s) = handleTypeBind x (TySubProper . TyVar)
    >>= \(newX, inner) -> ScmForall newX KProper <$> inner (substType' s)
  substType' (ScmForall x KPresence s) =
    handleTypeBind x (TySubPresence . PresenceVar)
      >>= \(newX, inner) -> ScmForall newX KPresence <$> inner (substType' s)
  substType' (ScmForall x (KArrow KProper KPresence) s) =
    handleTypeBind x (TySubPresenceWithType . PresenceWithTypeVar)
      >>= \(newX, inner) ->
            ScmForall newX (KArrow KProper KPresence) <$> inner (substType' s)
  substType' (ScmForall x KRow s) = handleTypeBind x (TySubRow . RowVar)
    >>= \(newX, inner) -> ScmForall newX KRow <$> inner (substType' s)
  substType' (ScmForall _ k _) = error ("Unknown kind: " ++ show (pretty k))
  substType' (ScmMono t      ) = ScmMono <$> substType' t

fvVarTermMap :: Map.Map VarName Term -> Set.Set VarName
fvVarTermMap = Map.foldl (\a b -> a `Set.union` fvTerm b) Set.empty

fvVarTySubMap
  :: Map.Map VarName TypeSubstitutor -> Either Error (Set.Set VarName)
fvVarTySubMap m = Map.keysSet <$> Map.foldl folder (Right Map.empty) m
 where
  folder set sub = do
    set'  <- set
    fvSub <- fvTySub sub
    mapUnionWithKind set' fvSub

handleTermBind
  :: VarName
  -> Reader
       (Map.Map VarName Term)
       ( VarName
       , Reader (Map.Map VarName Term) a -> Reader (Map.Map VarName Term) a
       )
handleTermBind x = do
  m <- ask
  let fvS = fvVarTermMap m
  return
    (if x `Set.notMember` fvS
      then (x, local (Map.delete x))
      else
        let newX = uniqueName fvS x
        in  (newX, local (`compositeTermSubst` Map.singleton x (TmVar newX)))
    )

handleTypeBind
  :: VarName
  -> (VarName -> TypeSubstitutor)
  -> ExceptT
       Error
       (Reader (Map.Map VarName TypeSubstitutor))
       (  VarName
       ,  ExceptT Error (Reader (Map.Map VarName TypeSubstitutor)) a
       -> ExceptT Error (Reader (Map.Map VarName TypeSubstitutor)) a
       )
handleTypeBind x cons = do
  m   <- ask
  fvS <- liftEither (fvVarTySubMap m)
  if x `Set.notMember` fvS
    then return (x, local (Map.delete x))
    else
      let newX = uniqueName fvS x
      in  do
            newM <- liftEither
              (compositeTypeSubst m (Map.singleton x (cons newX)))
            return (newX, local (const newM))
