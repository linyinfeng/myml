{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Myml.Parser.Common
  ( Parser (..),
  )
where

import Control.Applicative
import Control.Monad
import Myml.Parser.Style
import Text.Parser.LookAhead
import Text.Parser.Token.Style
import Text.Trifecta hiding (Parser)
import qualified Text.Trifecta
import Text.Trifecta.Delta

newtype Parser a = Parser {unParser :: Text.Trifecta.Parser a}
  deriving
    ( Monad,
      Functor,
      MonadFail,
      Applicative,
      Alternative,
      MonadPlus,
      LookAheadParsing,
      CharParsing,
      Parsing,
      Errable,
      DeltaParsing,
      MarkParsing Delta,
      Semigroup,
      Monoid
    )

instance TokenParsing Parser where
  someSpace = Parser (buildSomeSpaceParser someSpace commentStyle)
