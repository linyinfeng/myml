module Myml.Lang.Syntax
  ( TopLevel (..),
  )
where

import Myml.Syntax

data TopLevel
  = TopBind VarName Term
  | TopTerm Term
  | TopImport String
