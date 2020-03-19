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
    , "in"
    , "with"
    , "extend"
    , "access"
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
    , "from"
    , "inherit"
    , "new"
    , "self"
    , "as"
    , "getChar#"
    , "putChar#"
    , "compareChar#"
    -- type
    , "Unit"
    , "Nat"
    , "Char"
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
    , "="
    , "*"
    , "`"
    , ":="
    , "_:=_"
    , "!"
    , "\x3bb"
    , "\x3bc"
    , "\xb7"
    ]
  langReserved = ["import", "="]

isIdentLetter :: Char -> Bool
isIdentLetter c = not (isSpace c) && not (H.member c punctureChars)

identLetter :: CharParsing m => m Char
identLetter = satisfy isIdentLetter

identStyle :: CharParsing m => IdentifierStyle m
identStyle = IdentifierStyle { _styleName              = "identifer"
                             , _styleStart             = identLetter
                             , _styleLetter            = identLetter
                             , _styleReserved          = reservedTokens
                             , _styleHighlight         = Identifier
                             , _styleReservedHighlight = ReservedIdentifier
                             }

commentStyle :: CommentStyle
commentStyle = haskellCommentStyle
