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
  )
where

import           Myml.Syntax
import           Myml.Parser.Common
import           Myml.Parser.Style
import           Myml.Parser.Helper
import           Text.Trifecta           hiding ( Parser )
import           Text.Parser.Expression
import qualified Data.Map                      as Map
import           Control.Applicative

parseTerm :: Parser Term
parseTerm = buildExpressionParser termOperatorTable parseTermAtom <?> "term"

termOperatorTable :: OperatorTable Parser Term
termOperatorTable =
  [ [Postfix (chainedPostfix (opRcdAccess <|> opRcdExtend <|> opMatchExtend))]
  , [Infix opApp AssocLeft]
  , [Infix opAssign AssocNone]
  , [Infix opSeq AssocRight]
  , [Prefix (chainedPrefix (opIf <|> opAbs <|> opLet <|> opClass))]
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
    xs <- some (ident identStyle)
    _  <- symbol "."
    return (\t -> foldr TmAbs t xs)
  opLet = do
    reserve identStyle "let"
    x      <- ident identStyle
    params <- many (ident identStyle)
    reserve identStyle "="
    t <- parseTerm
    reserve identStyle "in"
    return (TmLet x (foldr TmAbs t params))
  opApp       = return TmApp
  opRcdAccess = flip TmRcdAccess <$> (symbol "." *> ident identStyle)
  opRcdExtend = do
    try $ reserve identStyle "with" <* symbol "{"
    pairs <- recordPair `sepBy` symbol ","
    _     <- symbol "}"
    return (\e -> foldl (\inner (l, t) -> TmRcdExtend inner l t) e pairs)
  opMatchExtend = do
    try $ reserve identStyle "with" <* symbol "["
    pairs <- matchPair `sepBy` symbol ","
    _     <- symbol "]"
    return (\t -> foldl (\inner (l, c) -> TmMatchExtend inner l c) t pairs)
  opAssign = TmAssign <$ reserve identStyle ":="
  opSeq    = TmSeq <$ try (symbol ";" <* notFollowedBy (char ';'))
  opClass  = do
    reserve identStyle "class"
    rep      <- reserve identStyle "from" *> ident identStyle
    inherits <- many
      (do
        reserve identStyle "inherit"
        t <- parseTermAtom
        reserve identStyle "as"
        x <- ident identStyle
        return (t, x)
      )
    return (deriveTermClass . TermClass inherits rep)

parseTermAtom :: Parser Term
parseTermAtom =
  (   rcd
    <|> match
    <|> variant
    <|> ref
    <|> deref
    <|> unit
    <|> true
    <|> false
    <|> zero
    <|> nat
    <|> suc
    <|> prd
    <|> isZero
    <|> new
    <|> self
    <|> var
    <|> parens parseTerm
    )
    <?> "termAtom"
 where
  var     = TmVar <$> ident identStyle
  rcd     = TmRcd . Map.fromList <$> braces (recordPair `sepBy` symbol ",")
  match   = TmMatch . Map.fromList <$> brackets (matchPair `sepBy` symbol ",")
  variant = TmVariant <$> variantLabel
  ref     = TmRef <$ reserve identStyle "ref"
  deref   = TmDeref <$ reserve identStyle "!"
  unit =
    TmUnit
      <$ (reserve identStyle "unit" <|> (() <$ try (symbol "(" *> symbol ")")))
  true   = TmTrue <$ reserve identStyle "true"
  false  = TmFalse <$ reserve identStyle "false"
  zero   = TmNat 0 <$ reserve identStyle "zero"
  nat    = TmNat <$> natural
  suc    = TmSucc <$ reserve identStyle "succ"
  prd    = TmPred <$ reserve identStyle "pred"
  isZero = TmIsZero <$ reserve identStyle "isZero"
  new    = termNew <$ reserve identStyle "new"
  self   = termSelf <$ reserve identStyle "self"

recordPair :: Parser (LabelName, Term)
recordPair = do
  l      <- ident identStyle
  params <- many (ident identStyle)
  reserve identStyle "="
  t <- parseTerm
  return (l, foldr TmAbs t params)

matchPair :: Parser (LabelName, TermCase)
matchPair = (,) <$> variantLabel <*> matchCase

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
  row      = KRow <$ reserve identStyle "Row"

parseTypeRow :: Parser LabelName -> Parser TypeRow
parseTypeRow parseLabel = rowEmpty <|> try pair <|> var <|> mu
 where
  rowEmpty = RowEmpty <$ reserve identStyle "\xb7"
  var      = RowVar <$> ident identStyle
  pair     = do
    (l, p) <- typeRowPair parseLabel
    _      <- symbol ","
    r      <- parseTypeRow parseLabel
    return (RowPresence l p r)
  mu = do
    reserve identStyle "\x3bc" <|> reserve identStyle "Rec"
    x <- ident identStyle
    r <- parens (parseTypeRow parseLabel)
    return (RowMu x r)

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
