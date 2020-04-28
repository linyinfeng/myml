module Myml.Parser.Style
  ( identStyle
  , commentStyle
  )
where

import           Data.Char
import qualified Data.HashSet                  as H
import           Text.Trifecta
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style

punctureChars :: H.HashSet Char
punctureChars = H.fromList ['(', ')', '[', ']', '{', '}', '.', ',', ';']

reservedTokens :: H.HashSet String
reservedTokens = H.fromList (coreReserved ++ langReserved)
 where
  coreReserved =
    [ "let"
    , "rec"
    , "="
    , "in"
    , "\x3bb" -- lambda
    , "\xb7"  -- underline
    -- records and variants
    , "extend"
    , "update"
    , "access"
    -- ref
    , "ref"
    , "!"
    , ":="
    , "assign"
    , "unit"
    -- bool
    , "true"
    , "false"
    , "if"
    , "then"
    , "else"
    -- class
    , "class"
    , "inherit"
    , "as"
    , "self"
    , "new"
    -- integer
    , "+"
    , "-"
    , "*"
    , "/"
    , "integerPlus"
    , "integerMul"
    , "integerAbs"
    , "integerSignum"
    , "integerNegate"
    , "integerQuotRem"
    , "integerCompare"
    -- char
    , "charCompare"
    -- io
    , "ioGetChar"
    , "ioPutChar"
    -- type
    , "Unit"
    , "Integer"
    , "Char"
    , "Rec"
    , "\x3bc"
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
    , "*"
    ]
  langReserved = ["import", "="]

isIdentLetter :: Char -> Bool
isIdentLetter c = not (isSpace c) && not (H.member c punctureChars)

isIdentStart :: Char -> Bool
isIdentStart c = isIdentLetter c && c /= '`'

identStart :: CharParsing m => m Char
identStart = satisfy isIdentStart

identLetter :: CharParsing m => m Char
identLetter = satisfy isIdentLetter

identStyle :: CharParsing m => IdentifierStyle m
identStyle = IdentifierStyle { _styleName              = "identifer"
                             , _styleStart             = identStart
                             , _styleLetter            = identLetter
                             , _styleReserved          = reservedTokens
                             , _styleHighlight         = Identifier
                             , _styleReservedHighlight = ReservedIdentifier
                             }

commentStyle :: CommentStyle
commentStyle = haskellCommentStyle
