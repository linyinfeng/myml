{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Myml.Syntax
  ( Term(..)
  , TermCase(..)
  , Type(..)
  , TypeRow(..)
  , TypePresence(..)
  , TypeRowCofinite(..)
  , TypeScheme(..)
  , Kind(..)
  , VarName
  , LabelName
  , isValue
  , isNatValue
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
          -- Primitives
          | TmUnit
          | TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          deriving (Eq, Show)

instance Monad m => Serial m Term where
  series =
    cons1 (TmAbs "x")
      \/ cons2 TmApp
      \/ cons0 (TmVar "x")
      \/ cons2 (TmLet "x")
      \/ cons0 (TmRcd Map.empty)
      \/ cons1 (\t -> TmRcd (Map.singleton "l" t))
      \/ cons2 (\t1 t2 -> TmRcd (Map.fromList [("l1", t1), ("l2", t2)]))
      \/ cons2 (\t1 t2 -> TmRcdExtend t1 "l" t2)
      \/ cons1 (flip TmRcdAccess "l")
      \/ cons0 (TmMatch Map.empty)
      \/ cons1 (\c -> TmMatch (Map.singleton "l" c))
      \/ cons2 (\c1 c2 -> TmMatch (Map.fromList [("l1", c1), ("l2", c2)]))
      \/ cons2 (\t c -> TmMatchExtend t "l" c)
      \/ cons1 (TmVariant "l")
      \/ cons1 TmRef
      \/ cons1 TmDeref
      \/ cons2 TmAssign
      -- do not generate TmLoc
      \/ pure TmUnit -- no decDepth for TmUnit
      \/ cons0 TmTrue
      \/ cons0 TmFalse
      \/ cons3 TmIf
      \/ cons0 TmZero
      \/ cons1 TmSucc

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
          deriving (Eq, Show)

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

data TypeRow = TyRow { finite :: Map.Map LabelName TypePresence
                     , cofinite :: TypeRowCofinite
                     }
                     deriving (Eq, Show)

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
                  deriving (Eq, Show)

-- preserve depth
instance Monad m => Serial m TypePresence where
  series = cons0 Absent \/ (Present <$> series)

data TypeRowCofinite = AllAbsent
                     | RowVar VarName
                     deriving (Eq, Show)

instance Monad m => Serial m TypeRowCofinite where
  series = cons0 AllAbsent \/ cons0 (RowVar "r")

data TypeScheme = ScmMono Type
            | ScmForall VarName Kind TypeScheme
            deriving (Eq, Show)

instance Monad m => Serial m TypeScheme where
  series = cons1 ScmMono \/ cons2 (ScmForall "x")

data Kind = KProper
          | KPresence
          | KRow (Set.Set LabelName)
          | KArrow Kind Kind
          deriving (Eq, Show)

instance Monad m => Serial m Kind where
  series =
    cons0 KProper
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
isValue TmRcd{}         = True
isValue TmRcdExtend{}   = False
isValue TmRcdAccess{}   = False
isValue TmMatch{}       = True
isValue TmMatchExtend{} = False
isValue TmVariant{}     = True
isValue TmRef{}         = False
isValue TmDeref{}       = False
isValue TmAssign{}      = False
isValue TmLoc{}         = True
isValue TmUnit          = True
isValue TmTrue          = True
isValue TmFalse         = True
isValue TmIf{}          = False
isValue t@TmZero        = isNatValue t
isValue t@TmSucc{}      = isNatValue t

isNatValue :: Term -> Bool
isNatValue TmZero      = True
isNatValue (TmSucc t') = isNatValue t'
isNatValue _           = False

class PrettyPrec a where
  prettyPrec :: Int -> a -> Doc ann

instance Pretty Term where
  pretty = prettyPrec 0

instance PrettyPrec Term where
  prettyPrec n (TmAbs x t) = parensPrec
    (n > prec)
    (pretty '\x3bb' <> pretty x <> pretty '.' <+> prettyPrec prec t)
    where prec = 0
  prettyPrec n (TmApp t1 t2) = parensPrec
    (n > prec)
    (prettyPrec prec t1 <+> prettyPrec (prec + 1) t2)
    where prec = 2
  prettyPrec _ (TmVar name   ) = pretty name
  prettyPrec n (TmLet x t1 t2) = parensPrec
    (n > prec)
    (align
      (   pretty "let"
      <+> pretty x
      <+> pretty '='
      <+> prettyPrec 0 t1
      <+> pretty "in"
      <>  line
      <>  prettyPrec prec t2
      )
    )
    where prec = 0
  prettyPrec _ (TmRcd fields) =
    pretty '{'
      <+> concatWith concator (map prettyPair (Map.toList fields))
      <+> pretty '}'
   where
    concator l r = l <> pretty ',' <+> r
    prettyPair (l, t) = pretty l <+> pretty '=' <+> prettyPrec 0 t
  prettyPrec n (TmRcdExtend t1 l t2) = parensPrec
    (n > prec)
    (   prettyPrec prec t1
    <+> pretty "with"
    <+> pretty '{'
    <+> pretty l
    <+> pretty '='
    <+> prettyPrec 0 t2
    <+> pretty '}'
    )
    where prec = 4
  prettyPrec n (TmRcdAccess t1 l) = parensPrec
    (n > prec)
    (prettyPrec prec t1 <> pretty '.' <> pretty l)
    where prec = 4
  prettyPrec _ (TmMatch cases) =
    pretty '['
      <+> concatWith concator (map prettyPair (Map.toList cases))
      <+> pretty ']'
   where
    concator l r = l <> pretty ',' <+> r
    prettyPair (l, c) = prettyVariantLabel l <+> pretty c
  prettyPrec n (TmMatchExtend t l c) = parensPrec
    (n > prec)
    (   prettyPrec prec t
    <+> pretty "with"
    <+> pretty '['
    <+> prettyVariantLabel l
    <+> pretty c
    <+> pretty ']'
    )
    where prec = 4
  prettyPrec n (TmVariant l t) = parensPrec
    (n > prec)
    (prettyVariantLabel l <+> prettyPrec prec t)
    where prec = 3
  prettyPrec n (TmRef t) = parensPrec (n > prec)
                                      (pretty "ref" <+> prettyPrec prec t)
    where prec = 3
  prettyPrec n (TmDeref t) = parensPrec
    (n > prec)
    (pretty "!" <> prettyPrec (prec + 1) t)
    where prec = 3
  prettyPrec n (TmAssign t1 t2) = parensPrec
    (n > prec)
    (prettyPrec (prec + 1) t1 <+> pretty ":=" <+> prettyPrec (prec + 1) t2)
    where prec = 1
  prettyPrec _ (TmLoc l)       = pretty "loc(" <> pretty l <> pretty ")"
  prettyPrec _ TmUnit          = pretty "unit"
  prettyPrec _ TmTrue          = pretty "true"
  prettyPrec _ TmFalse         = pretty "false"
  prettyPrec n (TmIf t1 t2 t3) = parensPrec
    (n > prec)
    (align
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
    where prec = 0
  prettyPrec _ TmZero     = pretty "zero"
  prettyPrec n (TmSucc t) = parensPrec (n > prec)
                                       (pretty "succ" <+> prettyPrec prec t)
    where prec = 3

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
    (prettyPrec (prec + 1) ty1 <+> pretty "->" <+> prettyPrec prec ty2)
    where prec = 0
  prettyPrec _ (TyRecord rows) =
    pretty '{' <+> prettyTypeRow (\l -> pretty l <+> pretty ":") rows <+> pretty
      '}'
  prettyPrec _ (TyVariant rows) =
    pretty '['
      <+> prettyTypeRow (\l -> prettyVariantLabel l <+> pretty ":") rows
      <+> pretty ']'
  prettyPrec n (TyMu name t) = parensPrec
    (n > prec)
    (pretty "\x3bc" <> pretty name <> pretty '.' <+> prettyPrec prec t)
    where prec = 1
  prettyPrec n (TyRef t) = parensPrec (n > prec)
                                      (pretty "Ref" <+> prettyPrec prec t)
    where prec = 1
  prettyPrec _ TyUnit = pretty "Unit"
  prettyPrec _ TyBool = pretty "Bool"
  prettyPrec _ TyNat  = pretty "Nat"

prettyTypeRow :: (LabelName -> Doc ann) -> TypeRow -> Doc ann
prettyTypeRow conv (TyRow f cof) = case cof of
  AllAbsent     -> finitePart
  (RowVar name) -> finitePart <> space <> pretty "|" <+> pretty name
 where
  prettyPair (l, t) = conv l <+> pretty t
  concator left right = left <> pretty "," <+> right
  finitePart = concatWith concator (map prettyPair (Map.toList f))

instance Pretty TypePresence where
  pretty Absent      = pretty "Absent"
  pretty (Present t) = pretty "Present" <+> prettyPrec 1 t

instance Pretty TypeScheme where
  pretty (ScmMono t) = pretty t
  pretty (ScmForall name k s) =
    pretty '\x2200'
      <>  pretty name
      <+> pretty "::"
      <+> pretty k
      <>  pretty "."
      <+> pretty s

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
