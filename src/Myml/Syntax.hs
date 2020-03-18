{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternSynonyms #-}

module Myml.Syntax
  ( Error(..)
  , Term(..)
  , TermClass(..)
  , deriveTermClass
  , termZ
  , termNew
  , termSelf
  , termUnit
  , emptyRecord
  , emptyMatch
  , recordExtends
  , matchExtends
  , recordLiteral
  , matchLiteral
  , TermCase(..)
  , Type(..)
  , pattern TyUnit
  , TypeRow(..)
  , TypePresence(..)
  , PresenceWithType(..)
  , TypeSubstitutor(..)
  , varToTySub
  , kindOfTySub
  , TypeScheme(..)
  -- kind and help constructor
  , Kind(..)
  , pattern KPresenceWithType
  -- string alias
  , VarName
  , LabelName
  -- definition of value
  , isValue
  -- free variable
  , fvTerm
  , fvCase
  , fvType
  , fvPresence
  , fvPresenceWithType
  , fvRow
  , fvScheme
  , fvTySub
  -- kind helpers
  , mapUnionWithKind
  , mapDeleteWithKind
  , mapDiffWithKind
  -- pretty print
  , prettyTypeRow
  , prettyTypeRow'
  )
where

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Text.Printf
import           Data.Text.Prettyprint.Doc
import           Test.SmallCheck.Series

type VarName = String
type LabelName = String

data Term = TmAbs VarName Term
          | TmApp Term Term
          | TmVar VarName
          | TmLet VarName Term Term
          -- Polymorphic record
          | TmRcd (Map.Map LabelName Term)
          | TmRcdExtend Term LabelName Term
          | TmRcdAccess Term LabelName
          -- Polymorphic variants
          | TmMatch (Map.Map LabelName TermCase)
          | TmMatchExtend Term LabelName TermCase
          | TmVariant LabelName
          -- Reference
          | TmRef
          | TmDeref
          | TmAssign
          | TmLoc Integer
          -- Sequence
          | TmSeq Term Term
          -- Primitives
          -- Boolean
          | TmTrue
          | TmFalse
          | TmIf Term Term Term
          -- Nat
          | TmNat Integer
          | TmSucc
          | TmPred
          | TmIsZero
          deriving (Eq, Show)

data TermClass = TermClass {
    classInherits :: [(Term, VarName)],
    classRep :: VarName,
    classBody :: Term
  }
  deriving (Eq, Show)

deriveTermClass :: TermClass -> Term
deriveTermClass (TermClass inherits rep body) = TmAbs
  rep
  (TmAbs "self" (TmAbs "_" (inheritsToLet inherits body)))
 where
  inheritsToLet ((t, x) : ps) inner = TmLet
    x
    (TmApp (TmApp (TmApp t (TmVar rep)) (TmVar "self")) termUnit)
    (inheritsToLet ps inner)
  inheritsToLet [] inner = inner

-- λ f . (λ x . f (λ v . x x  v)) (λ x . f (λ v . x x v))
termZ :: Term
termZ = TmAbs "f" (TmApp half half)
 where
  half = TmAbs
    "x"
    (TmApp (TmVar "f")
           (TmAbs "v" (TmApp (TmApp (TmVar "x") (TmVar "x")) (TmVar "v")))
    )

termNew :: Term
termNew = TmAbs
  "k"
  (TmAbs "r" (TmApp (TmApp termZ (TmApp (TmVar "k") (TmVar "r"))) termUnit))

termSelf :: Term
termSelf = TmApp (TmVar "self") termUnit

termUnit :: Term
termUnit = emptyRecord

emptyRecord :: Term
emptyRecord = TmRcd Map.empty

emptyMatch :: Term
emptyMatch = TmMatch Map.empty

recordExtends :: Term -> [(LabelName, Term)] -> Term
recordExtends = foldl (\inner (l, c) -> TmRcdExtend inner l c)

matchExtends :: Term -> [(LabelName, TermCase)] -> Term
matchExtends = foldl (\inner (l, c) -> TmMatchExtend inner l c)

recordLiteral :: [(LabelName, Term)] -> Term
recordLiteral = TmRcd . Map.fromList

matchLiteral :: [(LabelName, TermCase)] -> Term
matchLiteral = TmMatch . Map.fromList

instance Monad m => Serial m Term where
  series =
    cons1 (TmAbs "x")
      \/ cons2 TmApp
      \/ cons0 (TmVar "x")
      \/ cons2 (TmLet "x")
      \/ pure (TmRcd Map.empty) -- Unit
      \/ cons1 (TmRcd . Map.singleton "l")
      \/ cons2 (\a b -> TmRcd (Map.fromList [("l1", a), ("l2", b)]))
      \/ cons2 (\t1 t2 -> TmRcdExtend t1 "l" t2)
      \/ cons1 (flip TmRcdAccess "l")
      \/ cons0 (TmMatch Map.empty)
      \/ cons1 (TmMatch . Map.singleton "l")
      \/ cons2 (\a b -> TmMatch (Map.fromList [("l1", a), ("l2", b)]))
      \/ cons2 (\t c -> TmMatchExtend t "l" c)
      \/ cons0 (TmVariant "l")
      \/ cons0 TmRef
      \/ cons0 TmDeref
      \/ cons0 TmAssign
      -- do not generate location
      \/ cons2 TmSeq
      -- do not generate some primitives
      -- \/ cons0 TmTrue
      -- \/ cons0 TmFalse
      \/ cons3 TmIf
      \/ cons0 (TmNat 0)
      -- \/ cons0 TmSucc
      -- \/ cons0 TmPred
      -- \/ cons0 TmIsZero

data TermCase = TmCase VarName Term
  deriving (Eq, Show)

instance Monad m => Serial m TermCase where
  series = TmCase "x" <$> series

data Type = TyVar VarName
          | TyArrow Type Type
          | TyRecord TypeRow
          | TyVariant TypeRow
          | TyMu VarName Type
          | TyRef Type
          -- Primitives
          | TyBool
          | TyNat
          deriving (Eq, Show, Ord)

pattern TyUnit :: Type
pattern TyUnit = TyRecord RowEmpty

instance Monad m => Serial m Type where
  series =
    cons0 (TyVar "X")
      \/ cons2 TyArrow
      \/ cons1 TyRecord
      \/ cons1 TyVariant
      \/ cons1 (TyMu "X")
      \/ cons1 TyRef
      \/ cons0 TyBool
      \/ cons0 TyNat

data TypeRow = RowEmpty
             | RowVar VarName
             | RowPresence LabelName TypePresence TypeRow
             | RowMu VarName TypeRow
              deriving (Show, Eq, Ord)

-- preserve depth
instance Monad m => Serial m TypeRow where
  series =
    cons0 RowEmpty \/ cons0 (RowVar "X") \/ cons2 (RowPresence "l") \/ cons1
      (RowMu "X")

data TypePresence = Absent
                  | Present Type
                  | PresenceVar VarName
                  | PresenceVarWithType VarName Type
                  deriving (Show, Eq, Ord)

-- preserve depth
instance Monad m => Serial m TypePresence where
  series = pure Absent \/ cons1 Present \/ pure (PresenceVar "X") \/ cons1
    (PresenceVarWithType "X")

data PresenceWithType = PresenceWithTypeAbsent
                      | PresenceWithTypePresent
                      | PresenceWithTypeVar VarName
                      deriving (Show, Eq, Ord)

data TypeScheme = ScmMono Type
                | ScmForall VarName Kind TypeScheme
                deriving (Eq, Show)

instance Monad m => Serial m TypeScheme where
  series = cons1 ScmMono \/ cons2 (ScmForall "X")

data TypeSubstitutor = TySubProper Type
                     | TySubPresenceWithType PresenceWithType
                     | TySubPresence TypePresence
                     | TySubRow TypeRow
                     deriving (Show, Eq, Ord)

varToTySub :: Kind -> VarName -> Either Error TypeSubstitutor
varToTySub KProper = Right . TySubProper . TyVar
varToTySub KPresenceWithType =
  Right . TySubPresenceWithType . PresenceWithTypeVar
varToTySub KPresence = Right . TySubPresence . PresenceVar
varToTySub KRow      = Right . TySubRow . RowVar
varToTySub k         = const (Left (ErrInvalidKind k))

kindOfTySub :: TypeSubstitutor -> Kind
kindOfTySub (TySubProper           _) = KProper
kindOfTySub (TySubPresenceWithType _) = KPresenceWithType
kindOfTySub (TySubPresence         _) = KPresence
kindOfTySub (TySubRow              _) = KRow

data Kind = KProper
          | KPresence
          | KRow
          | KArrow Kind Kind
          deriving (Eq, Show)

pattern KPresenceWithType :: Kind
pattern KPresenceWithType = KArrow KProper KPresence

instance Monad m => Serial m Kind where
  series = pure KProper \/ cons0 KPresence \/ cons0 KRow \/ cons2 KArrow

isValue :: Term -> Bool
isValue (TmApp (TmVariant _) t) = isValue t
isValue (TmApp TmAssign      t) = isValue t
isValue TmApp{}                 = False
isValue TmVar{}                 = False
isValue TmAbs{}                 = True
isValue TmLet{}                 = False
isValue (TmRcd m)               = Map.foldl (\a b -> a && isValue b) True m
isValue TmRcdExtend{}           = False
isValue TmRcdAccess{}           = False
isValue TmMatch{}               = True
isValue TmMatchExtend{}         = False
isValue TmVariant{}             = True
isValue TmRef                   = True
isValue TmDeref                 = True
isValue TmAssign                = True
isValue TmLoc{}                 = True
isValue TmSeq{}                 = False
isValue TmTrue                  = True
isValue TmFalse                 = True
isValue TmIf{}                  = False
isValue TmNat{}                 = True
isValue TmSucc                  = True
isValue TmPred                  = True
isValue TmIsZero                = True

fvTerm :: Term -> Set.Set VarName
fvTerm (TmVar x                   ) = Set.singleton x
fvTerm (TmAbs x  t                ) = Set.delete x (fvTerm t)
fvTerm (TmApp t1 t2               ) = fvTerm t1 `Set.union` fvTerm t2
fvTerm (TmLet x t1 t2) = fvTerm t1 `Set.union` Set.delete x (fvTerm t2)
fvTerm (TmRcd m) = Map.foldl (\a b -> a `Set.union` fvTerm b) Set.empty m
fvTerm (TmRcdExtend t1 _label t2  ) = fvTerm t1 `Set.union` fvTerm t2
fvTerm (TmRcdAccess t _label      ) = fvTerm t
fvTerm (TmMatch m) = Map.foldl (\a b -> a `Set.union` fvCase b) Set.empty m
fvTerm (TmMatchExtend t1 _label c2) = fvTerm t1 `Set.union` fvCase c2
fvTerm (TmVariant _label          ) = Set.empty
fvTerm TmRef                        = Set.empty
fvTerm TmDeref                      = Set.empty
fvTerm TmAssign                     = Set.empty
fvTerm (TmLoc _loc )                = Set.empty
fvTerm (TmSeq t1 t2)                = fvTerm t1 `Set.union` fvTerm t2
fvTerm TmTrue                       = Set.empty
fvTerm TmFalse                      = Set.empty
fvTerm (TmIf t1 t2 t3)              = Set.unions (map fvTerm [t1, t2, t3])
fvTerm (TmNat _n     )              = Set.empty
fvTerm TmSucc                       = Set.empty
fvTerm TmPred                       = Set.empty
fvTerm TmIsZero                     = Set.empty

fvCase :: TermCase -> Set.Set VarName
fvCase (TmCase x t) = Set.delete x (fvTerm t)

mapUnionWithKind
  :: Map.Map VarName Kind
  -> Map.Map VarName Kind
  -> Either Error (Map.Map VarName Kind)
mapUnionWithKind m1 m2 = sequence (Map.unionWithKey union m1' m2')
 where
  m1' = Map.map Right m1
  m2' = Map.map Right m2
  union x (Right k1) (Right k2) | k1 == k2  = Right k1
                                | otherwise = Left (ErrVarKindConflict x k1 k2)
  union _ l@(Left _) _ = l
  union _ _          l = l

mapDeleteWithKind
  :: VarName
  -> Kind
  -> Map.Map VarName Kind
  -> Either Error (Map.Map VarName Kind)
mapDeleteWithKind x k m = case Map.lookup x m of
  Nothing -> Right m
  Just k' | k == k'   -> Right (Map.delete x m)
          | otherwise -> Left (ErrVarKindConflict x k k')

mapDiffWithKind
  :: Map.Map VarName Kind
  -> Map.Map VarName Kind
  -> Either Error (Map.Map VarName Kind)
mapDiffWithKind m1 m2 = sequence (Map.differenceWithKey diff m1' m2')
 where
  m1' = Map.map Right m1
  m2' = Map.map Right m2
  diff x (Right k1) (Right k2)
    | k1 == k2  = Nothing
    | otherwise = Just (Left (ErrVarKindConflict x k1 k2))
  diff _ l@(Left _) _ = Just l
  diff _ _          l = Just l

fvType :: Type -> Either Error (Map.Map VarName Kind)
fvType (TyVar x        ) = return (Map.singleton x KProper)
fvType (TyArrow ty1 ty2) = do
  f1 <- fvType ty1
  f2 <- fvType ty2
  mapUnionWithKind f1 f2
fvType (TyRecord  row) = fvRow row
fvType (TyVariant row) = fvRow row
fvType (TyMu x t     ) = fvType t >>= mapDeleteWithKind x KProper
fvType (TyRef t      ) = fvType t
fvType TyBool          = return Map.empty
fvType TyNat           = return Map.empty

fvRow :: TypeRow -> Either Error (Map.Map VarName Kind)
fvRow RowEmpty                 = return Map.empty
fvRow (RowVar x              ) = return (Map.singleton x KRow)
fvRow (RowPresence _label p r) = do
  fp <- fvPresence p
  fr <- fvRow r
  mapUnionWithKind fp fr
fvRow (RowMu x r) = fvRow r >>= mapDeleteWithKind x KRow

fvPresence :: TypePresence -> Either Error (Map.Map VarName Kind)
fvPresence Absent          = return Map.empty
fvPresence (Present     t) = fvType t
fvPresence (PresenceVar x) = return (Map.singleton x KPresence)
fvPresence (PresenceVarWithType x t) =
  fvType t >>= mapUnionWithKind (Map.singleton x KPresenceWithType)

fvPresenceWithType :: PresenceWithType -> Either Error (Map.Map VarName Kind)
fvPresenceWithType PresenceWithTypeAbsent  = return Map.empty
fvPresenceWithType PresenceWithTypePresent = return Map.empty
fvPresenceWithType (PresenceWithTypeVar x) =
  return (Map.singleton x KPresenceWithType)

fvTySub :: TypeSubstitutor -> Either Error (Map.Map VarName Kind)
fvTySub (TySubProper           t) = fvType t
fvTySub (TySubPresenceWithType p) = fvPresenceWithType p
fvTySub (TySubPresence         p) = fvPresence p
fvTySub (TySubRow              r) = fvRow r

fvScheme :: TypeScheme -> Either Error (Map.Map VarName Kind)
fvScheme (ScmMono t      ) = fvType t
fvScheme (ScmForall x k t) = fvScheme t >>= mapDeleteWithKind x k

class PrettyPrec a where
  prettyPrec :: Int -> a -> Doc ann

instance Pretty Term where
  pretty = prettyPrec 0

instance PrettyPrec Term where
  prettyPrec n (TmAbs x t) = parensPrec
    (n > prec)
    (hang
      indentSpace
      (   pretty '\x3bb'
      <+> pretty x
      <+> pretty '.'
      <>  softline
      <>  prettyPrec prec t
      )
    )
    where prec = 0
  prettyPrec n (TmApp t1 t2) = parensPrec
    (n > prec)
    (align (prettyPrec prec t1 <> softline <> prettyPrec (prec + 1) t2))
    where prec = 3
  prettyPrec _ (TmVar name   ) = pretty name
  prettyPrec n (TmLet x t1 t2) = parensPrec
    (n > prec)
    (align
      (   pretty "let"
      <+> pretty x
      <+> pretty '='
      <+> prettyPrec 0 t1
      <+> pretty "in"
      <>  softline
      <>  prettyPrec prec t2
      )
    )
    where prec = 0
  prettyPrec _ (TmRcd fields) = group
    (align (encloseSep open close separator fields'))
   where
    fields'   = map prettyPair (Map.toList fields)
    open      = flatAlt (pretty "{ ") (pretty "{")
    close     = flatAlt (pretty " }") (pretty "}")
    separator = pretty ", "
    prettyPair (l, t) = pretty l <+> pretty '=' <+> prettyPrec 0 t
  prettyPrec n (TmRcdExtend t1 l t2) = parensPrec
    (n > prec)
    (hang
      indentSpace
      (   prettyPrec prec t1
      <+> pretty "with"
      <>  softline
      <>  pretty '{'
      <+> pretty l
      <+> pretty '='
      <+> prettyPrec 0 t2
      <+> pretty '}'
      )
    )
    where prec = 5
  prettyPrec n (TmRcdAccess t1 l) = parensPrec
    (n > prec)
    (prettyPrec prec t1 <> pretty '.' <> pretty l)
    where prec = 5
  prettyPrec _ (TmMatch cases) = group
    (align (encloseSep open close separator cases'))
   where
    cases'    = map prettyPair (Map.toList cases)
    open      = flatAlt (pretty "[ ") (pretty "[")
    close     = flatAlt (pretty " ]") (pretty "]")
    separator = pretty ", "
    prettyPair (l, c) = prettyVariantLabel l <+> pretty c
  prettyPrec n (TmMatchExtend t l c) = parensPrec
    (n > prec)
    (hang
      indentSpace
      (   prettyPrec prec t
      <+> pretty "with"
      <>  softline
      <>  pretty '['
      <+> prettyVariantLabel l
      <+> pretty c
      <+> pretty ']'
      )
    )
    where prec = 5
  prettyPrec _ (TmVariant l) = prettyVariantLabel l
  prettyPrec _ TmRef         = pretty "ref"
  prettyPrec _ TmDeref       = pretty "!"
  prettyPrec _ TmAssign      = pretty "_:=_"
  prettyPrec _ (TmLoc l    ) = pretty "loc(" <> pretty l <> pretty ")"
  prettyPrec n (TmSeq t1 t2) = parensPrec
    (n > prec)
    (  align (prettyPrec (prec + 1) t1)
    <> pretty ";"
    <> softline
    <> prettyPrec prec t2
    )
    where prec = 1
  prettyPrec _ TmTrue          = pretty "true"
  prettyPrec _ TmFalse         = pretty "false"
  prettyPrec n (TmIf t1 t2 t3) = parensPrec
    (n > prec)
    (align
      (group
        (   pretty "if"
        <+> prettyPrec 0 t1
        <>  line
        <>  pretty "then"
        <+> prettyPrec 0 t2
        <>  line
        <>  pretty "else"
        <+> prettyPrec prec t3
        )
      )
    )
    where prec = 0
  prettyPrec _ (TmNat n) = pretty n
  prettyPrec _ TmSucc    = pretty "succ"
  prettyPrec _ TmPred    = pretty "pred"
  prettyPrec _ TmIsZero  = pretty "isZero"

prettyVariantLabel :: LabelName -> Doc ann
prettyVariantLabel name = pretty '`' <> pretty name

instance Pretty TermCase where
  pretty (TmCase name t) = pretty name <+> pretty "->" <+> prettyPrec 0 t

instance Pretty Type where
  pretty = prettyPrec 0

instance PrettyPrec Type where
  prettyPrec _ (TyVar name     ) = pretty name
  prettyPrec n (TyArrow ty1 ty2) = parensPrec
    (n > prec)
    (align
      (   prettyPrec (prec + 1) ty1
      <+> pretty "->"
      <>  softline
      <>  prettyPrec prec ty2
      )
    )
    where prec = 1
  prettyPrec _ (TyRecord rows) =
    prettyTypeRow "{" "}" (\l -> pretty l <+> pretty ":") rows

  prettyPrec _ (TyVariant rows) =
    prettyTypeRow "[" "]" (\l -> prettyVariantLabel l <+> pretty ":") rows
  prettyPrec n (TyMu x t) = parensPrec
    (n > prec)
    (pretty "\x3bc" <+> pretty x <+> pretty '.' <+> prettyPrec prec t)
    where prec = 0
  prettyPrec n (TyRef t) = parensPrec (n > prec)
                                      (pretty "Ref" <+> prettyPrec prec t)
    where prec = 2
  prettyPrec _ TyBool = pretty "Bool"
  prettyPrec _ TyNat  = pretty "Nat"

prettyTypeRow
  :: String -> String -> (LabelName -> Doc ann) -> TypeRow -> Doc ann
prettyTypeRow open close label row = align
  (group (open' <> prettyTypeRow' label row <> close'))
 where
  open'  = flatAlt (pretty (open ++ " ")) (pretty open)
  close' = flatAlt (pretty (" " ++ close)) (pretty close)

prettyTypeRow' :: (LabelName -> Doc ann) -> TypeRow -> Doc ann
prettyTypeRow' _ RowEmpty   = pretty "\xb7" -- ·
prettyTypeRow' _ (RowVar x) = pretty x
prettyTypeRow' label (RowPresence l p r) =
  label l <+> pretty p <> line' <> pretty ',' <+> prettyTypeRow' label r
prettyTypeRow' label (RowMu x r) =
  pretty '\x3bc' <+> pretty x <+> pretty '.' <+> align
    (group (pretty "(" <+> prettyTypeRow' label r <+> pretty ")"))

instance Pretty TypePresence where
  pretty Absent                    = pretty "Absent"
  pretty (Present     t          ) = pretty "Present" <+> prettyPrec 1 t
  pretty (PresenceVar x          ) = pretty x
  pretty (PresenceVarWithType x t) = pretty x <+> prettyPrec 1 t

instance Pretty PresenceWithType where
  pretty PresenceWithTypeAbsent  = pretty "Absent"
  pretty PresenceWithTypePresent = pretty "Present"
  pretty (PresenceWithTypeVar x) = pretty x

instance Pretty TypeScheme where
  pretty (ScmMono t) = pretty t
  pretty (ScmForall name k s) =
    pretty '\x2200'
      <+> pretty name
      <+> pretty "::"
      <+> pretty k
      <+> pretty "."
      <>  softline -- no hang and align
      <>  pretty s

instance Pretty TypeSubstitutor where
  pretty (TySubProper           t) = pretty t
  pretty (TySubPresenceWithType p) = pretty p
  pretty (TySubPresence         p) = pretty p
  pretty (TySubRow r) = prettyTypeRow "(" ")" (\l -> pretty l <+> pretty ":") r

instance Pretty Kind where
  pretty = prettyPrec 0

instance PrettyPrec Kind where
  prettyPrec _ KProper        = pretty '*'
  prettyPrec _ KPresence      = pretty "Presence"
  prettyPrec _ KRow           = pretty "Row"
  prettyPrec n (KArrow k1 k2) = parensPrec
    (n > prec)
    (prettyPrec (prec + 1) k1 <+> pretty "=>" <+> prettyPrec prec k2)
    where prec = 0

parensPrec :: Bool -> Doc ann -> Doc ann
parensPrec cond = if cond then parens else id

indentSpace :: Int
indentSpace = 2

data Error = ErrVarKindConflict VarName Kind Kind
           | ErrInvalidKind Kind
           -- typing errors
           | ErrUnifyKindMismatch Kind Kind
           | ErrUnifyNoRuleApplied TypeSubstitutor TypeSubstitutor
           | ErrUnifyRowLabelCollided (Set.Set LabelName)
           | ErrUnboundedVariable VarName
           | ErrStoreTypingNotImplemented
           | ErrCanNotHandleMuType Type
           | ErrCanNotHandleMuRow TypeRow
           | ErrImperativeFeaturesDisabled Term
           -- evaluation errors
           | ErrEvalStuck Term
           deriving Eq

showPretty :: Pretty a => a -> String
showPretty = show . pretty

instance Show Error where
  show (ErrVarKindConflict x k1 k2) = printf
    "variable \"%s\" has two different kind \"%s\" and \"%s\""
    x
    (showPretty k1)
    (showPretty k2)
  show (ErrInvalidKind k) = printf "invalid kind \"%s\"" (showPretty k)
  show (ErrUnifyKindMismatch k1 k2) = printf
    "kind \"%s\" mismatch with kind \"%s\" in unification"
    (showPretty k1)
    (showPretty k2)
  show (ErrUnifyNoRuleApplied s1 s2) =
    printf "no rule to unify \"%s\" with \"%s\"" (showPretty s1) (showPretty s2)
  show (ErrUnifyRowLabelCollided s) =
    printf "row label \"%s\" collided" (show s)
  show (ErrUnboundedVariable x)     = printf "unbounded variable \"%s\"" x
  show ErrStoreTypingNotImplemented = "store typing is not implemented"
  show (ErrCanNotHandleMuType t) =
    printf "can not handle recursive type here \"%s\"" (showPretty t)
  show (ErrCanNotHandleMuRow r) = printf
    "can not handle recursive row here \"%s\""
    (show (prettyTypeRow "(" ")" (\l -> pretty l <+> pretty ":") r))
  show (ErrImperativeFeaturesDisabled t) = printf
    "imperative features disabled, can not type term \"%s\""
    (showPretty t)
  show (ErrEvalStuck t) = printf "evaluation stuck at \"%s\"" (showPretty t)
