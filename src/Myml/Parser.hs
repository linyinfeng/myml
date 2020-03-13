module Myml.Parser
  ( parseTerm
  , parseTermAtom
  , parseType
  , parseTypeAtom
  , parseTypeRow
  , parseScheme
  , parseSchemeAtom
  , parseKind
  , parseKindAtom
  , identStyle
  )
where

import           Myml.Syntax
import           Text.Trifecta
import           Text.Parser.Token.Highlight
import           Text.Parser.Expression
import qualified Data.HashSet                  as H
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Control.Applicative
import           Data.Functor
import           Data.Char

parseTerm :: Parser Term
parseTerm = buildExpressionParser termOperatorTable parseTermAtom <?> "term"

termOperatorTable :: OperatorTable Parser Term
termOperatorTable =
  [ [Postfix (chainedPostfix (opRcdAccess <|> opRcdExtend <|> opMatchExtend))]
  , [Prefix (chainedPrefix (opVariant <|> opRef <|> opDeref))]
  , [Infix opApp AssocLeft]
  , [Infix opAssign AssocNone]
  , [Infix opSeq AssocRight]
  , [Prefix (chainedPrefix (opIf <|> opAbs <|> opLet))]
  ]
 where
  opIf = do
    reserve identStyle "if"
    t1 <- parseTerm
    reserve identStyle "then"
    t2 <- parseTerm
    reserve identStyle "else"
    return (TmIf t1 t2)
  opAbs = do
    reserve identStyle "\x3bb" <|> reserve identStyle "\\"
    x <- ident identStyle
    _ <- symbol "."
    return (TmAbs x)
  opLet = do
    reserve identStyle "let"
    x <- ident identStyle
    reserve identStyle "="
    t <- parseTerm
    reserve identStyle "in"
    return (TmLet x t)
  opApp       = return TmApp
  opVariant   = TmVariant <$> variantLabel
  opRcdAccess = flip TmRcdAccess <$> (symbol "." *> ident identStyle)
  opRcdExtend = do
    try $ reserve identStyle "with" <* symbol "{"
    (l, t) <- recordPair
    _      <- symbol "}"
    return (\e -> TmRcdExtend e l t)
  opMatchExtend = do
    try $ reserve identStyle "with" <* symbol "["
    (l, c) <- matchPair
    _      <- symbol "]"
    return (\t -> TmMatchExtend t l c)
  opRef    = TmRef <$ reserve identStyle "ref"
  opDeref  = TmDeref <$ reserve identStyle "!"
  opAssign = TmAssign <$ reserve identStyle ":="
  opSeq    = TmSeq <$ symbol ";"

parseTermAtom :: Parser Term
parseTermAtom =
  (   rcd
    <|> match
    <|> unit
    <|> true
    <|> false
    <|> zero
    <|> nat
    <|> suc
    <|> prd
    <|> isZero
    <|> klass
    <|> var
    <|> parens parseTerm
    )
    <?> "termAtom"
 where
  var   = TmVar <$> ident identStyle
  rcd   = TmRcd . Map.fromList <$> braces (recordPair `sepBy` symbol ",")
  match = TmMatch . Map.fromList <$> brackets (matchPair `sepBy` symbol ",")
  unit =
    TmUnit
      <$ (reserve identStyle "unit" <|> (try (symbol "(" *> symbol ")") $> ()))
  true   = TmTrue <$ reserve identStyle "true"
  false  = TmFalse <$ reserve identStyle "false"
  zero   = TmNat 0 <$ reserve identStyle "zero"
  nat    = TmNat <$> natural
  suc    = TmSucc <$ reserve identStyle "succ"
  prd    = TmPred <$ reserve identStyle "pred"
  isZero = TmIsZero <$ reserve identStyle "isZero"
  klass = do
    reserve identStyle "class"
    inherit <- optional (reserve identStyle "inherit" *> parseTermAtom)
    reserve identStyle "with"
    rep <- ident identStyle
    methods <- Map.fromList <$> braces (recordPair `sepBy` symbol ",")
    let k = TmClass inherit rep methods
    return (deriveTermClass k)

recordPair :: Parser (LabelName, Term)
recordPair =
  (\x y -> (x, y)) <$> ident identStyle <* reserve identStyle "=" <*> parseTerm

matchPair :: Parser (LabelName, TermCase)
matchPair = (\x y -> (x, y)) <$> variantLabel <*> matchCase

matchCase :: Parser TermCase
matchCase =
  TmCase <$> ident identStyle <* reserve identStyle "->" <*> parseTerm

variantLabel :: Parser LabelName
variantLabel = try (char '`' *> ident identStyle)

parseType :: Parser Type
parseType = buildExpressionParser typeOperatorTable parseTypeAtom <?> "type"

typeOperatorTable :: OperatorTable Parser Type
typeOperatorTable =
  [ [Prefix (chainedPrefix opRef)]
  , [Infix opArrow AssocRight]
  , [Prefix (chainedPrefix opMu)]
  ]
 where
  opRef = TyRef <$ reserve identStyle "Ref"
  opMu =
    TyMu
      <$> (  (reserve identStyle "\x3bc" <|> reserve identStyle "Rec")
          *> ident identStyle
          <* symbol "."
          )
  opArrow = TyArrow <$ reserve identStyle "->"

parseTypeAtom :: Parser Type
parseTypeAtom =
  (var <|> rcd <|> variant <|> unit <|> bool <|> nat <|> parens parseType)
    <?> "typeAtom"
 where
  var = TyVar <$> ident identStyle
  rcd =
    TyRecord <$> (symbol "{" *> parseTypeRow (ident identStyle) <* symbol "}")
  variant =
    TyVariant <$> (symbol "[" *> parseTypeRow variantLabel <* symbol "]")
  unit = TyUnit <$ reserve identStyle "Unit"
  bool = TyBool <$ reserve identStyle "Bool"
  nat  = TyNat <$ reserve identStyle "Nat"

parseScheme :: Parser TypeScheme
parseScheme =
  buildExpressionParser schemeOperatorTable parseSchemeAtom <?> "scheme"

schemeOperatorTable :: OperatorTable Parser TypeScheme
schemeOperatorTable = [[Prefix (chainedPrefix opForall)]]
 where
  opForall = do
    _ <- reserve identStyle "\x2200"
    x <- ident identStyle
    reserve identStyle "::"
    kind <- parseKind
    _    <- symbol "."
    return (ScmForall x kind)

parseSchemeAtom :: Parser TypeScheme
parseSchemeAtom = (ScmMono <$> parseType) <|> parens parseScheme

parseKind :: Parser Kind
parseKind = buildExpressionParser kindOperatorTable parseKindAtom <?> "kind"

kindOperatorTable :: OperatorTable Parser Kind
kindOperatorTable = [[Infix opArrow AssocRight]]
  where opArrow = KArrow <$ reserve identStyle "=>"

parseKindAtom :: Parser Kind
parseKindAtom = proper <|> presence <|> row <|> parens parseKind
 where
  proper   = KProper <$ reserve identStyle "*"
  presence = KPresence <$ reserve identStyle "Presence"
  row      = do
    reserve identStyle "Row"
    _  <- symbol "("
    ls <- commaSep (ident identStyle)
    _  <- symbol ")"
    return (KRow (Set.fromList ls))

parseTypeRow :: Parser LabelName -> Parser TypeRow
parseTypeRow parseLabel = do
  finitePart <- Map.fromList <$> (typeRowPair parseLabel `sepBy` symbol ",")
  TyRow finitePart <$> typeRowCofinite parseLabel

typeRowPair :: Parser LabelName -> Parser (LabelName, TypePresence)
typeRowPair parseLabel = do
  label <- parseLabel
  reserve identStyle ":"
  presence <- typePresence
  return (label, presence)

typePresence :: Parser TypePresence
typePresence =
  (Absent <$ reserve identStyle "Absent")
    <|> (Present <$> (reserve identStyle "Present" *> parseType))
    <|> try (PresenceVarWithType <$> ident identStyle <*> parseType)
    <|> (PresenceVar <$> ident identStyle)

typeRowCofinite :: Parser LabelName -> Parser TypeRowCofinite
typeRowCofinite parseLabel = rowVar <|> mu <|> allAbsent
 where
  rowVar    = CofRowVar <$> try (reserve identStyle "|" *> ident identStyle)
  allAbsent = return CofAllAbsent
  mu        = do
    reserve identStyle "|"
    reserve identStyle "\x3bc"
    x <- ident identStyle
    _ <- symbol "."
    r <- parens (parseTypeRow parseLabel)
    return (CofMu x r)

-- no need to include ".", ",", ";"
reservedTokens :: H.HashSet String
reservedTokens = H.fromList
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

identStyle :: IdentifierStyle Parser
identStyle = IdentifierStyle { _styleName              = "identifer"
                             , _styleStart             = identLetter
                             , _styleLetter            = identLetter
                             , _styleReserved          = reservedTokens
                             , _styleHighlight         = Identifier
                             , _styleReservedHighlight = ReservedIdentifier
                             }

identLetter :: Parser Char
identLetter = satisfy isTokenChar
 where
  isTokenChar c =
    not (isSpace c)
      && c
      /= '('
      && c
      /= ')'
      && c
      /= '['
      && c
      /= ']'
      && c
      /= '{'
      && c
      /= '}'
      && c
      /= '.'
      && c
      /= ','
      && c
      /= ';'

chainedPrefix :: Parser (a -> a) -> Parser (a -> a)
chainedPrefix p = chainl1 p (return (.))

chainedPostfix :: Parser (a -> a) -> Parser (a -> a)
chainedPostfix p = chainr1 p (return $ flip (.))
