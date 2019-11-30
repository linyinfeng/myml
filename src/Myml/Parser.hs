module Myml.Parser
  ( parseTerm
  )
where

import           Myml.Syntax
import           Text.Trifecta
import           Control.Applicative
import           Data.Functor
import qualified Text.Parser.Token.Highlight   as H

identStyle :: IdentifierStyle Parser
identStyle = IdentifierStyle "identifier"
                             letter
                             alphaNum
                             reservedIdent
                             H.Identifier
                             H.ReservedIdentifier

parseTerm :: Parser Term
parseTerm = parseHighest where parseHighest = parseSeqLike

parseSeqLike :: Parser Term
parseSeqLike = parseSeq parseHigher <|> parseHigher
  where parseHigher = parsePrefixTerm

parseSeq :: Parser Term -> Parser Term
parseSeq higher =
  chainr1 higher $ symbol ";" $> \t1 t2 -> TmApp (TmAbs "_" TyUnit t2) t1

parsePrefixTerm :: Parser Term
parsePrefixTerm = parseAbs <|> parseIf <|> parseLetIn <|> parseHigher
  where parseHigher = parseAppLike

parseAbs :: Parser Term
parseAbs = liftA3 TmAbs
                  (parseLambda *> identOrUnderscore)
                  (colon *> parseType <|> pure (TyVar "_"))
                  (dot *> parsePrefixTerm)
  where identOrUnderscore = parseIdent <|> symbol "_"

parseIf :: Parser Term
parseIf = liftA3 TmIf
                 (symbol "if" *> parseTerm)
                 (symbol "then" *> parseTerm)
                 (symbol "else" *> parsePrefixTerm)

parseLetIn :: Parser Term
parseLetIn = liftA3 TmLetIn
                    (symbol "let" *> parseIdent)
                    (symbol "=" *> parseTerm)
                    (symbol "in" *> parsePrefixTerm)

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
  atoms = map
    (\(t, s) -> t <$ symbol s)
    [(TyVar "_", "_"), (TyUnit, "Unit"), (TyNat, "Nat"), (TyBool, "Bool")]

parseTypeVar :: Parser Type
parseTypeVar = TyVar <$> parseIdent

parseIdent :: Parser String
parseIdent = ident identStyle
