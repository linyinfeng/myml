{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternSynonyms #-}

module Myml.Syntax
  ( Error(..)
  , Term(..)
  , TermClass(..)
  , deriveTermClass
  , deriveString
  , termZ
  , termNew
  , termSelf
  , termSeq
  , termWildcardAbs
  , uniqueName
  , pattern TmUnit
  , termTrue
  , termFalse
  , termIf
  , labaledApps
  , recordLiteral
  , matchLiteral
  , Type(..)
  , pattern TyUnit
  , typeOrdering
  , typeBool
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

import           Data.Maybe                     ( fromJust )
import           Data.List                      ( find )
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
          | TmEmptyRcd
          | TmRcdExtend LabelName
          | TmRcdUpdate LabelName
          | TmRcdAccess LabelName
          -- Polymorphic variants
          | TmEmptyMatch
          | TmMatchExtend LabelName
          | TmMatchUpdate LabelName
          | TmVariant LabelName
          -- Reference
          | TmRef
          | TmDeref
          | TmAssign
          | TmLoc Integer
          -- Primitives
          -- Nat
          | TmNat Integer
          | TmSucc
          | TmPred
          | TmIsZero
          -- Character
          | TmChar Char
          | TmPutChar
          | TmGetChar
          | TmCompareChar
          deriving (Eq, Show)

infixl 7 `TmApp`

data TermClass = TermClass {
    classInherits :: [(Term, VarName)],
    classRep :: VarName,
    classBody :: Term
  }
  deriving (Eq, Show)

deriveTermClass :: TermClass -> Term
deriveTermClass (TermClass inherits rep body) = TmAbs
  rep
  (TmAbs "self" (termWildcardAbs (inheritsToLet inherits body)))
 where
  inheritsToLet ((t, x) : ps) inner = TmLet
    x
    (TmApp (TmApp (TmApp t (TmVar rep)) (TmVar "self")) TmUnit)
    (inheritsToLet ps inner)
  inheritsToLet [] inner = inner

deriveString :: String -> Term
deriveString []       = TmVariant "nil" `TmApp` TmUnit
deriveString (c : cs) = TmVariant "cons"
  `TmApp` recordLiteral [("head", (TmChar c)), ("tail", deriveString cs)]

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
  (TmAbs "r" (TmApp (TmApp termZ (TmApp (TmVar "k") (TmVar "r"))) TmUnit))

termSelf :: Term
termSelf = TmApp (TmVar "self") TmUnit

termSeq :: Term -> Term -> Term
termSeq t1 t2 = TmApp (termWildcardAbs t2) t1

termWildcardAbs :: Term -> Term
termWildcardAbs t = TmAbs (uniqueName (fvTerm t) "_") t

uniqueName :: Set.Set VarName -> VarName -> VarName
uniqueName sv base = fromJust
  (find (`Set.notMember` sv)
        (base : map (\i -> base ++ show i) [(1 :: Integer) ..])
  )

pattern TmUnit :: Term
pattern TmUnit = TmEmptyRcd

termTrue :: Term
termTrue = TmApp (TmVariant "true") TmUnit

termFalse :: Term
termFalse = TmApp (TmVariant "false") TmUnit

termIf :: Term -> Term -> Term -> Term
termIf t1 t2 t3 = TmApp
  (matchLiteral [("true", termWildcardAbs t2), ("false", termWildcardAbs t3)])
  t1

labaledApps :: (LabelName -> Term) -> Term -> [(LabelName, Term)] -> Term
labaledApps ext = foldl (\inner (l, c) -> TmApp (TmApp (ext l) c) inner)

recordLiteral :: [(LabelName, Term)] -> Term
recordLiteral = labaledApps TmRcdExtend TmEmptyRcd

matchLiteral :: [(LabelName, Term)] -> Term
matchLiteral = labaledApps TmMatchExtend TmEmptyMatch

instance Monad m => Serial m Term where
  series =
    cons1 (TmAbs "x")
      \/ cons2 TmApp
      \/ cons0 (TmVar "x")
      \/ cons2 (TmLet "x")
      \/ cons0 TmEmptyRcd  -- TmUnit
      \/ cons0 (TmRcdExtend "l")
      \/ cons0 (TmRcdAccess "l")
      \/ cons0 TmEmptyMatch
      \/ cons0 (TmMatchExtend "l")
      \/ cons0 (TmVariant "l")
      \/ cons0 TmRef
      \/ cons0 TmDeref
      \/ cons0 TmAssign
      -- do not generate location
      \/ cons0 (TmNat 0)
      \/ cons0 TmSucc
      \/ cons0 TmPred
      \/ cons0 TmIsZero
      \/ cons1 TmChar
      \/ cons0 TmPutChar
      \/ cons0 TmGetChar
      \/ cons0 TmCompareChar

data Type = TyVar VarName
          | TyArrow Type Type
          | TyRecord TypeRow
          | TyVariant TypeRow
          | TyMu VarName Type
          | TyRef Type
          -- Primitives
          | TyNat
          | TyChar
          deriving (Eq, Show, Ord)

infixr 7 `TyArrow`

pattern TyUnit :: Type
pattern TyUnit = TyRecord RowEmpty

typeOrdering :: VarName -> Type
typeOrdering r = TyVariant
  ( RowPresence "LT" (Present TyUnit)
  $ RowPresence "EQ" (Present TyUnit)
  $ RowPresence "GT" (Present TyUnit)
  $ RowVar r
  )

typeBool :: VarName -> Type
typeBool r = TyVariant
  ( RowPresence "true"  (Present TyUnit)
  $ RowPresence "false" (Present TyUnit)
  $ RowVar r
  )

instance Monad m => Serial m Type where
  series =
    cons0 (TyVar "X")
      \/ cons2 TyArrow
      \/ cons1 TyRecord
      \/ cons1 TyVariant
      \/ cons1 (TyMu "X")
      \/ cons1 TyRef
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
isValue t@(TmApp (TmApp (TmRcdExtend _) _) _) = isRcdValue t
isValue t@TmEmptyRcd                          = isRcdValue t
isValue (TmApp (TmRcdExtend _) t)             = isValue t
isValue (TmRcdExtend _)                       = True
isValue (TmApp (TmRcdUpdate _) t)             = isValue t
isValue (TmRcdUpdate _)                       = True
isValue (TmRcdAccess _)                       = True
isValue t@(TmApp (TmApp (TmMatchExtend _) _) _) = isMatchValue t
isValue t@TmEmptyMatch                        = isMatchValue t
isValue (TmApp (TmMatchExtend _) t)           = isValue t
isValue (TmMatchExtend _          )           = True
isValue (TmApp (TmMatchUpdate _) t)           = isValue t
isValue (TmMatchUpdate _          )           = True
isValue (TmApp (TmVariant _) t    )           = isValue t
isValue (TmVariant _              )           = True
isValue TmRef                                 = True
isValue TmDeref                               = True
isValue (TmApp TmAssign t)                    = isValue t
isValue TmAssign                              = True
isValue (TmLoc _)                             = True
isValue TmNat{}                               = True
isValue TmSucc                                = True
isValue TmPred                                = True
isValue TmIsZero                              = True
isValue (TmChar _)                            = True
isValue TmPutChar                             = True
isValue TmGetChar                             = True
isValue (TmApp TmCompareChar (TmChar _))      = True
isValue TmCompareChar                         = True
-- Basic
isValue TmApp{}                               = False
isValue TmVar{}                               = False
isValue TmAbs{}                               = True
isValue TmLet{}                               = False

isRcdValue :: Term -> Bool
isRcdValue TmEmptyRcd                      = True
isRcdValue (TmApp (TmRcdExtend _label) v1) = isValue v1
isRcdValue (TmApp (TmApp (TmRcdExtend _label) v1) v2) =
  isValue v1 && isRcdValue v2
isRcdValue _ = False

isMatchValue :: Term -> Bool
isMatchValue TmEmptyMatch = True
isMatchValue (TmApp (TmApp (TmMatchExtend _label) v1) v2) =
  isValue v1 && isMatchValue v2
isMatchValue _ = False

fvTerm :: Term -> Set.Set VarName
fvTerm (TmVar x      )        = Set.singleton x
fvTerm (TmAbs x  t   )        = Set.delete x (fvTerm t)
fvTerm (TmApp t1 t2  )        = fvTerm t1 `Set.union` fvTerm t2
fvTerm (TmLet x t1 t2)        = fvTerm t1 `Set.union` Set.delete x (fvTerm t2)
fvTerm TmEmptyRcd             = Set.empty
fvTerm (TmRcdExtend _label)   = Set.empty
fvTerm (TmRcdUpdate _label)   = Set.empty
fvTerm (TmRcdAccess _label)   = Set.empty
fvTerm TmEmptyMatch           = Set.empty
fvTerm (TmMatchExtend _label) = Set.empty
fvTerm (TmMatchUpdate _label) = Set.empty
fvTerm (TmVariant     _label) = Set.empty
fvTerm TmRef                  = Set.empty
fvTerm TmDeref                = Set.empty
fvTerm TmAssign               = Set.empty
fvTerm (TmLoc _loc)           = Set.empty
fvTerm (TmNat _n  )           = Set.empty
fvTerm TmSucc                 = Set.empty
fvTerm TmPred                 = Set.empty
fvTerm TmIsZero               = Set.empty
fvTerm (TmChar _)             = Set.empty
fvTerm TmPutChar              = Set.empty
fvTerm TmGetChar              = Set.empty
fvTerm TmCompareChar          = Set.empty

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
fvType TyNat           = return Map.empty
fvType TyChar          = return Map.empty

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
  prettyPrec _ TmEmptyRcd      = pretty "{}"
  prettyPrec _ (TmRcdExtend l) = pretty "extend" <> parens (pretty l)
  prettyPrec _ (TmRcdUpdate l) = pretty "update" <> parens (pretty l)
  prettyPrec _ (TmRcdAccess l) = pretty "access" <> parens (pretty l)
  prettyPrec _ TmEmptyMatch    = pretty "[]"
  prettyPrec _ (TmMatchExtend l) =
    pretty "extend" <> parens (prettyVariantLabel l)
  prettyPrec _ (TmMatchUpdate l) =
    pretty "update" <> parens (prettyVariantLabel l)
  prettyPrec _ (TmVariant l) = prettyVariantLabel l
  prettyPrec _ TmRef         = pretty "ref"
  prettyPrec _ TmDeref       = pretty "!"
  prettyPrec _ TmAssign      = pretty "_:=_"
  prettyPrec _ (TmLoc l)     = pretty "loc(" <> pretty l <> pretty ")"
  prettyPrec _ (TmNat n)     = pretty n
  prettyPrec _ TmSucc        = pretty "succ"
  prettyPrec _ TmPred        = pretty "pred"
  prettyPrec _ TmIsZero      = pretty "isZero"
  prettyPrec _ (TmChar c)    = pretty (show c)
  prettyPrec _ TmPutChar     = pretty "putChar#"
  prettyPrec _ TmGetChar     = pretty "getChar#"
  prettyPrec _ TmCompareChar = pretty "compareChar#"

prettyVariantLabel :: LabelName -> Doc ann
prettyVariantLabel name = pretty '`' <> pretty name

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
  prettyPrec _ TyNat  = pretty "Nat"
  prettyPrec _ TyChar = pretty "Char"

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
