module Myml.Parser.Style
  ( punctureStyle
  , identStyle
  )
where

import           Data.Char
import qualified Data.HashSet                  as H
import           Text.Trifecta
import           Text.Parser.Token.Highlight

reservedPunctures :: H.HashSet String
reservedPunctures = H.fromList (coreReserved ++ langReserved)
 where
  coreReserved = ["(", ")", "()", "[", "]", "{", "}", ".", ",", ";"]
  langReserved = [";;"]

punctureChars :: H.HashSet Char
punctureChars = H.fromList ['(', ')', '[', ']', '{', '}', '.', ',', ';']

punctureLetter :: Parser Char
punctureLetter = satisfy (flip H.member punctureChars)

punctureStyle :: IdentifierStyle Parser
punctureStyle = IdentifierStyle { _styleName              = "puncture"
                                , _styleStart             = punctureLetter
                                , _styleLetter            = punctureLetter
                                , _styleReserved          = reservedPunctures
                                , _styleHighlight         = Symbol
                                , _styleReservedHighlight = Symbol
                                }

-- no need to include ".", ",", ";"
reservedTokens :: H.HashSet String
reservedTokens = H.fromList (coreReserved ++ langReserved)
 where
  coreReserved =
    [ "let"
    , "in"
    , "with"
    , "ref"
    , "unit"
    , "true"
    , "false"
    , "if"
    , "then"
    , "else"
    , "zero"
    , "succ"
    , "pred"
    , "isZero"
    , "class"
    , "inherit"
    , "new"
    , "self"
    , "as"
    , "Unit"
    , "Bool"
    , "Nat"
    , "Rec"
    , "Ref"
    , "Absent"
    , "Present"
    , "Row"
    , "Presence"
    , ":"
    , "->"
    , "=>"
    , "::"
    , ":"
    , "|"
    , "="
    , "*"
    , "`"
    , ":="
    , "!"
    , "\x3bb"
    , "\x3bc"
    ]
  langReserved = ["import", "="]

identLetter :: Parser Char
identLetter = satisfy isTokenChar
  where isTokenChar c = not (isSpace c) && not (H.member c punctureChars)

identStyle :: IdentifierStyle Parser
identStyle = IdentifierStyle { _styleName              = "identifer"
                             , _styleStart             = identLetter
                             , _styleLetter            = identLetter
                             , _styleReserved          = reservedTokens
                             , _styleHighlight         = Identifier
                             , _styleReservedHighlight = ReservedIdentifier
                             }
