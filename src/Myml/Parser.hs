module Myml.Parser
  ( parseTerm
  )
where

import           Myml.Syntax
import           Text.Trifecta
import           Control.Applicative
import qualified Text.Parser.Token.Highlight   as H

identStyle :: IdentifierStyle Parser
identStyle = IdentifierStyle "identifier"
                             letter
                             alphaNum
                             reservedIdent
                             H.Identifier
                             H.ReservedIdentifier

parseTerm :: Parser Term
parseTerm = parsePrefixTerm

parsePrefixTerm :: Parser Term
parsePrefixTerm = parseAbs <|> parseIf <|> parseHigher
  where parseHigher = parseAppLike

parseAbs :: Parser Term
parseAbs = liftA3 TmAbs
                  (parseLambda *> parseIdent)
                  (colon *> parseType)
                  (dot *> parsePrefixTerm)

parseIf :: Parser Term
parseIf = liftA3 TmIf
                 (symbol "if" *> parseTerm)
                 (symbol "then" *> parseTerm)
                 (symbol "else" *> parsePrefixTerm)

parseAppLike :: Parser Term
parseAppLike =
  parseApp parseHigher
    <|> parseSucc
    <|> parsePred
    <|> parseIsZero
    <|> parseHigher
  where parseHigher = parseAtom

parseApp :: Parser Term -> Parser Term
parseApp higher = chainl1 higher (return TmApp)

parseSucc :: Parser Term
parseSucc = TmSucc <$ symbol "succ" <*> parseAppLike

parsePred :: Parser Term
parsePred = TmPred <$ symbol "pred" <*> parseAppLike

parseIsZero :: Parser Term
parseIsZero = TmIsZero <$ symbol "iszero" <*> parseAppLike

parseAtom :: Parser Term
parseAtom = Prelude.foldl (<|>) parseVar atoms <|> parens parseTerm
 where
  atoms = map
    (\(t, s) -> t <$ symbol s)
    [(TmTrue, "true"), (TmFalse, "false"), (TmZero, "0"), (TmUnit, "unit")]

parseVar :: Parser Term
parseVar = TmVar <$> parseIdent

parseLambda :: Parser String
parseLambda = symbol "λ" <|> symbol "\\"

parseType :: Parser Type
parseType = parseTypeFunc

parseTypeFunc :: Parser Type
parseTypeFunc = chainl1 parseTypeAtom (TyFunc <$ symbol "->")

parseTypeAtom :: Parser Type
parseTypeAtom = Prelude.foldl (<|>) parseTypeVar atoms <|> parens parseType
 where
  atoms = map (\(t, s) -> t <$ symbol s)
              [(TyUnit, "Unit"), (TyNat, "Nat"), (TyBool, "Bool")]

parseTypeVar :: Parser Type
parseTypeVar = TyVar <$> parseIdent

parseIdent :: Parser String
parseIdent = ident identStyle
