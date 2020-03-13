{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Myml.Syntax
  ( Term(..)
  , TermClass(..)
  , deriveTermClass
  , TermCase(..)
  , Type(..)
  , TypeRow(..)
  , TypePresence(..)
  , PresenceWithType(..)
  , TypeRowCofinite(..)
  , TypeScheme(..)
  , Kind(..)
  , VarName
  , LabelName
  , isValue
  , FreeVariable(..)
  , prettyTypeRow
  )
where

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
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
          | TmVariant LabelName Term
          -- Reference
          | TmRef Term
          | TmDeref Term
          | TmAssign Term Term
          | TmLoc Integer
          -- Sequence
          | TmUnit
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

data TermClass = TmClass {
    classInherit :: Maybe Term,
    classRep :: VarName,
    classMethods :: Map.Map LabelName Term
  }
  deriving (Eq, Show)

deriveTermClass :: TermClass -> Term
deriveTermClass (TmClass (Just inherit) rep methods) =
  TmAbs rep (
    TmAbs "self" (
      TmAbs ""
        (TmLet "super"
          (TmApp (TmApp (TmApp inherit (TmVar rep)) (TmVar "self")) TmUnit)
          (TmRcd methods))))
deriveTermClass (TmClass Nothing rep methods) =
  TmAbs rep (
    TmAbs "self" (
      TmAbs ""
        (TmRcd methods)))

instance Monad m => Serial m Term where
  series =
    cons1 (TmAbs "x")
      \/ cons2 TmApp
      \/ cons0 (TmVar "x")
      \/ cons2 (TmLet "x")
      \/ cons0 (TmRcd Map.empty)
      \/ cons1 (TmRcd . Map.singleton "l")
      \/ cons2 (\t1 t2 -> TmRcd (Map.fromList [("l1", t1), ("l2", t2)]))
      \/ cons2 (\t1 t2 -> TmRcdExtend t1 "l" t2)
      \/ cons1 (flip TmRcdAccess "l")
      \/ cons0 (TmMatch Map.empty)
      \/ cons1 (TmMatch . Map.singleton "l")
      \/ cons2 (\c1 c2 -> TmMatch (Map.fromList [("l1", c1), ("l2", c2)]))
      \/ cons2 (\t c -> TmMatchExtend t "l" c)
      \/ cons1 (TmVariant "l")
      \/ cons1 TmRef
      \/ cons1 TmDeref
      \/ cons2 TmAssign
      -- do not generate TmLoc
      \/ pure TmUnit -- no decDepth for TmUnit
      \/ cons2 TmSeq
      \/ cons0 TmTrue
      \/ cons0 TmFalse
      \/ cons3 TmIf
      \/ cons0 (TmNat 0)
      \/ cons0 TmSucc
      \/ cons0 TmPred
      \/ cons0 TmIsZero

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
          | TyUnit
          | TyBool
          | TyNat
          deriving (Eq, Show, Ord)

instance Monad m => Serial m Type where
  series =
    cons0 (TyVar "X")
      \/ cons2 TyArrow
      \/ cons1 TyRecord
      \/ cons1 TyVariant
      \/ cons1 (TyMu "X")
      \/ cons1 TyRef
      \/ pure TyUnit -- no decDepth for TyUnit
      \/ cons0 TyBool
      \/ cons0 TyNat

data TypeRow = TyRow { rowFinite :: Map.Map LabelName TypePresence
                     , rowCofinite :: TypeRowCofinite
                     }
                     deriving (Show, Eq, Ord)

-- preserve depth
instance Monad m => Serial m TypeRow where
  series = TyRow <$> mapSeries <~> series
   where
    mapSeries =
      cons0 Map.empty
        \/ (Map.singleton "l" <$> series)
        \/ (   (\p1 p2 -> Map.fromList [("l1", p1), ("l2", p2)])
           <$> series
           <~> series
           )

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

data TypeRowCofinite = CofAllAbsent
                     | CofRowVar VarName
                     | CofMu VarName TypeRow
                     deriving (Show, Eq, Ord)

instance Monad m => Serial m TypeRowCofinite where
  series = pure CofAllAbsent \/ pure (CofRowVar "R") \/ cons1 (CofMu "R")

data TypeScheme = ScmMono Type
            | ScmForall VarName Kind TypeScheme
            deriving (Eq, Show)

instance Monad m => Serial m TypeScheme where
  series = cons1 ScmMono \/ cons2 (ScmForall "X")

data Kind = KProper
          | KPresence
          | KRow (Set.Set LabelName)
          | KArrow Kind Kind
          deriving (Eq, Show)

instance Monad m => Serial m Kind where
  series =
    pure KProper
      \/ cons0 KPresence
      \/ cons0 (KRow Set.empty)
      \/ cons0 (KRow (Set.singleton "l"))
      \/ cons0 (KRow (Set.fromList ["l1", "l2"]))
      \/ cons2 KArrow

isValue :: Term -> Bool
isValue TmVar{}         = False
isValue TmAbs{}         = True
isValue TmApp{}         = False
isValue TmLet{}         = False
isValue (TmRcd m)       = Map.foldl (\a b -> a && isValue b) True m
isValue TmRcdExtend{}   = False
isValue TmRcdAccess{}   = False
isValue TmMatch{}       = True
isValue TmMatchExtend{} = False
isValue (TmVariant _ t) = isValue t
isValue TmRef{}         = False
isValue TmDeref{}       = False
isValue TmAssign{}      = False
isValue TmLoc{}         = True
isValue TmSeq{}         = False
isValue TmUnit          = True
isValue TmTrue          = True
isValue TmFalse         = True
isValue TmIf{}          = False
isValue TmNat{}         = True
isValue TmSucc          = True
isValue TmPred          = True
isValue TmIsZero        = True

class FreeVariable a where
  freeVariable :: a -> Set.Set VarName

instance FreeVariable Term where
  freeVariable (TmVar x    ) = Set.singleton x
  freeVariable (TmAbs x  t ) = Set.delete x (freeVariable t)
  freeVariable (TmApp t1 t2) = freeVariable t1 `Set.union` freeVariable t2
  freeVariable (TmLet x t1 t2) =
    freeVariable t1 `Set.union` Set.delete x (freeVariable t2)
  freeVariable (TmRcd m) =
    Map.foldl (\a b -> a `Set.union` freeVariable b) Set.empty m
  freeVariable (TmRcdExtend t1 _ t2) =
    freeVariable t1 `Set.union` freeVariable t2
  freeVariable (TmRcdAccess t _) = freeVariable t
  freeVariable (TmMatch m) =
    Map.foldl (\a b -> a `Set.union` freeVariable b) Set.empty m
  freeVariable (TmMatchExtend t1 _ t2) =
    freeVariable t1 `Set.union` freeVariable t2
  freeVariable (TmVariant _ t ) = freeVariable t
  freeVariable (TmRef   t     ) = freeVariable t
  freeVariable (TmDeref t     ) = freeVariable t
  freeVariable (TmAssign t1 t2) = freeVariable t1 `Set.union` freeVariable t2
  freeVariable (TmLoc _       ) = Set.empty
  freeVariable TmUnit           = Set.empty
  freeVariable (TmSeq t1 t2)    = freeVariable t1 `Set.union` freeVariable t2
  freeVariable TmTrue           = Set.empty
  freeVariable TmFalse          = Set.empty
  freeVariable (TmIf t1 t2 t3)  = Set.unions (map freeVariable [t1, t2, t3])
  freeVariable (TmNat _      )  = Set.empty
  freeVariable TmSucc           = Set.empty
  freeVariable TmPred           = Set.empty
  freeVariable TmIsZero         = Set.empty

instance FreeVariable TermCase where
  freeVariable (TmCase x t) = Set.delete x (freeVariable t)

instance FreeVariable Type where
  freeVariable (TyVar x) = Set.singleton x
  freeVariable (TyArrow ty1 ty2) =
    freeVariable ty1 `Set.union` freeVariable ty2
  freeVariable (TyRecord  row) = freeVariable row
  freeVariable (TyVariant row) = freeVariable row
  freeVariable (TyMu x t     ) = Set.delete x (freeVariable t)
  freeVariable (TyRef t      ) = freeVariable t
  freeVariable TyUnit          = Set.empty
  freeVariable TyBool          = Set.empty
  freeVariable TyNat           = Set.empty

instance FreeVariable TypeRow where
  freeVariable (TyRow f cof) =
    Map.foldl (\a b -> a `Set.union` freeVariable b) Set.empty f
      `Set.union` freeVariable cof

instance FreeVariable TypePresence where
  freeVariable Absent          = Set.empty
  freeVariable (Present     t) = freeVariable t
  freeVariable (PresenceVar x) = Set.singleton x
  freeVariable (PresenceVarWithType x t) =
    Set.singleton x `Set.union` freeVariable t

instance FreeVariable PresenceWithType where
  freeVariable PresenceWithTypeAbsent  = Set.empty
  freeVariable PresenceWithTypePresent = Set.empty
  freeVariable (PresenceWithTypeVar x) = Set.singleton x

instance FreeVariable TypeRowCofinite where
  freeVariable CofAllAbsent  = Set.empty
  freeVariable (CofRowVar r) = Set.singleton r
  freeVariable (CofMu x r  ) = Set.delete x (freeVariable r)

instance FreeVariable TypeScheme where
  freeVariable (ScmMono t      ) = freeVariable t
  freeVariable (ScmForall x _ t) = Set.delete x (freeVariable t)

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
    open      = pretty "{ "
    close     = pretty " }"
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
    open      = pretty "[ "
    close     = pretty " ]"
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
  prettyPrec n (TmVariant l t) = parensPrec
    (n > prec)
    (align (prettyVariantLabel l <> softline <> prettyPrec prec t))
    where prec = 4
  prettyPrec n (TmRef t) = parensPrec
    (n > prec)
    (align (pretty "ref" <> softline <> prettyPrec prec t))
    where prec = 4
  prettyPrec n (TmDeref t) = parensPrec
    (n > prec)
    (align (pretty "!" <> softline <> prettyPrec prec t))
    where prec = 4
  prettyPrec n (TmAssign t1 t2) = parensPrec
    (n > prec)
    (hang
      indentSpace
      (   prettyPrec (prec + 1) t1
      <+> pretty ":="
      <>  softline
      <>  prettyPrec (prec + 1) t2
      )
    )
    where prec = 2
  prettyPrec _ (TmLoc l)     = pretty "loc(" <> pretty l <> pretty ")"
  prettyPrec _ TmUnit        = pretty "unit"
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
    prettyTypeRow (pretty "{") (pretty "}") (\l -> pretty l <+> pretty ":") rows
  prettyPrec _ (TyVariant rows) = prettyTypeRow
    (pretty "[")
    (pretty "]")
    (\l -> prettyVariantLabel l <+> pretty ":")
    rows
  prettyPrec n (TyMu name t) = parensPrec
    (n > prec)
    (pretty "\x3bc" <+> pretty name <+> pretty '.' <+> prettyPrec prec t)
    where prec = 0
  prettyPrec n (TyRef t) = parensPrec (n > prec)
                                      (pretty "Ref" <+> prettyPrec prec t)
    where prec = 2
  prettyPrec _ TyUnit = pretty "Unit"
  prettyPrec _ TyBool = pretty "Bool"
  prettyPrec _ TyNat  = pretty "Nat"

prettyTypeRow
  :: Doc ann -> Doc ann -> (LabelName -> Doc ann) -> TypeRow -> Doc ann
prettyTypeRow open close conv (TyRow f cof) = align
  (group (open <+> prettyRow <+> close))
 where
  prettyRow = case cof of
    CofAllAbsent     -> finitePart
    (CofRowVar name) -> finitePart <> line <> pretty "|" <+> pretty name
    (CofMu x r) ->
      finitePart
        <>  line
        <>  pretty "|"
        <+> pretty "\x3bc"
        <+> pretty x
        <+> pretty '.'
        <+> prettyTypeRow (pretty "( ") (pretty " )") conv r
  prettyPair (l, t) = conv l <+> pretty t
  concator left right = left <> line <> pretty "," <+> right
  finitePart = concatWith concator (map prettyPair (Map.toList f))

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

instance Pretty Kind where
  pretty = prettyPrec 0

instance PrettyPrec Kind where
  prettyPrec _ KProper   = pretty '*'
  prettyPrec _ KPresence = pretty "Presence"
  prettyPrec _ (KRow labels) =
    pretty "Row" <> tupled (map pretty (Set.toList labels))
  prettyPrec n (KArrow k1 k2) = parensPrec
    (n > prec)
    (prettyPrec (prec + 1) k1 <+> pretty "=>" <+> prettyPrec prec k2)
    where prec = 0

parensPrec :: Bool -> Doc ann -> Doc ann
parensPrec cond = if cond then parens else id

indentSpace :: Int
indentSpace = 2
