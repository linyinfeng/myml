module Myml.Syntax
  ( Term(..)
  , Type(..)
  , reservedIdent
  , isValue
  , isLambdaValue
  , isNatValue
  , isBoolValue
  , isUnitValue
  )
where

import           Data.HashSet

data Term = TmVar String
          | TmApp Term Term
          | TmAbs String Type Term
          | TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
          | TmUnit
          deriving (Eq, Ord, Show)

data Type = TyBool
          | TyNat
          | TyUnit
          | TyFunc Type Type
          | TyVar String
          deriving (Eq, Ord, Show)

reservedIdent :: HashSet String
reservedIdent = fromList
  [ "if"
  , "then"
  , "else"
  , "true"
  , "false"
  , "0"
  , "succ"
  , "pred"
  , "iszero"
  , "unit"
  , "Bool"
  , "Nat"
  , "Unit"
  ]

isValue :: Term -> Bool
isValue t =
  any (\p -> p t) [isLambdaValue, isBoolValue, isNatValue, isUnitValue]

isLambdaValue :: Term -> Bool
isLambdaValue TmAbs{} = True
isLambdaValue _       = False

isBoolValue :: Term -> Bool
isBoolValue TmTrue  = True
isBoolValue TmFalse = True
isBoolValue _       = False

isNatValue :: Term -> Bool
isNatValue TmZero     = True
isNatValue (TmSucc t) = isNatValue t
isNatValue _          = False

isUnitValue :: Term -> Bool
isUnitValue TmUnit = True
isUnitValue _      = False
