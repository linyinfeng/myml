{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

module Myml.Parser.Common
  ( Parser(..)
  )
where

import           Myml.Parser.Style
import           Data.Char
import qualified Text.Trifecta
import           Text.Trifecta           hiding ( Parser )
import           Text.Trifecta.Delta
import           Text.Parser.LookAhead
import           Text.Parser.Token.Style
import           Control.Monad
import           Control.Applicative
import           Control.Monad.Fail

newtype Parser a = Parser { unParser :: Text.Trifecta.Parser a }
  deriving ( Monad
           , Functor
           , MonadFail
           , Applicative
           , Alternative
           , MonadPlus
           , LookAheadParsing
           , CharParsing
           , Parsing
           , Errable
           , DeltaParsing
           , MarkParsing Delta
           , Semigroup
           , Monoid
           )

instance TokenParsing Parser where
  someSpace = buildSomeSpaceParser (skipSome (satisfy isSpace)) commentStyle
