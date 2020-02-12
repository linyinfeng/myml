module Myml.Parser
  ( parseTerm
  , parseTermAtom
  , parseType
  , parseTypeAtom
  )
where

import           Myml.Syntax
import           Text.Trifecta
import           Text.Parser.Token.Highlight
import           Text.Parser.Expression
import qualified Data.HashSet                  as H
import qualified Data.Map                      as Map
import           Control.Applicative

parseTerm :: Parser Term
parseTerm = buildExpressionParser termOperatorTable parseTermAtom <?> "term"

termOperatorTable :: OperatorTable Parser Term
termOperatorTable =
  [ [Postfix (chainedPostfix (opRcdAccess <|> opRcdExtend <|> opMatchExtend))]
  , [Prefix (chainedPrefix (opSucc <|> opVariant <|> opRef <|> opDeref))]
  , [Infix opApp AssocLeft]
  , [Infix opAssign AssocNone]
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
    _ <- symbol "\x3bb" <|> symbol "\\"
    x <- ident identStyle
    reserve opsStyle "."
    return (TmAbs x)
  opLet = do
    reserve identStyle "let"
    x <- ident identStyle
    reserve opsStyle "="
    t <- parseTerm
    reserve identStyle "in"
    return (TmLet x t)
  opApp       = return TmApp
  opSucc      = TmSucc <$ reserve identStyle "succ"
  opVariant   = TmVariant <$> variantLabel
  opRcdAccess = flip TmRcdAccess <$> (reserve opsStyle "." *> ident identStyle)
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
  opDeref  = TmDeref <$ reserve opsStyle "!"
  opAssign = TmAssign <$ reserve opsStyle ":="

parseTermAtom :: Parser Term
parseTermAtom =
  (   var
    <|> rcd
    <|> match
    <|> unit
    <|> true
    <|> false
    <|> zero
    <|> parens parseTerm
    )
    <?> "termAtom"
 where
  var = TmVar <$> ident identStyle
  rcd =
    TmRcd . Map.fromList <$> braces (recordPair `sepBy` reserve opsStyle ",")
  match =
    TmMatch . Map.fromList <$> brackets (matchPair `sepBy` reserve opsStyle ",")
  unit  = TmUnit <$ reserve identStyle "unit"
  true  = TmTrue <$ reserve identStyle "true"
  false = TmFalse <$ reserve identStyle "false"
  zero  = TmZero <$ reserve identStyle "zero"

recordPair :: Parser (LabelName, Term)
recordPair =
  (\x y -> (x, y)) <$> ident identStyle <* reserve opsStyle "=" <*> parseTerm

matchPair :: Parser (LabelName, TermCase)
matchPair = (\x y -> (x, y)) <$> variantLabel <*> matchCase

matchCase :: Parser TermCase
matchCase = TmCase <$> ident identStyle <* reserve opsStyle "->" <*> parseTerm

variantLabel :: Parser LabelName
variantLabel = try (char '`' *> ident identStyle)

parseType :: Parser Type
parseType = buildExpressionParser typeOperatorTable parseTypeAtom <?> "type"

typeOperatorTable :: OperatorTable Parser Type
typeOperatorTable =
  [[Prefix (chainedPrefix (opRef <|> opMu))], [Infix opArrow AssocRight]]
 where
  opRef = TyRef <$ reserve identStyle "Ref"
  opMu = TyMu <$> (symbol "\x3bc" *> ident identStyle <* reserve opsStyle ".")
  opArrow = TyArrow <$ reserve opsStyle "->"

parseTypeAtom :: Parser Type
parseTypeAtom =
  (var <|> rcd <|> variant <|> unit <|> bool <|> nat <|> parens parseType)
    <?> "typeAtom"
 where
  var     = TyVar <$> ident identStyle
  rcd = TyRecord <$> (symbol "{" *> typeRow (ident identStyle) <* symbol "}")
  variant = TyVariant <$> (symbol "[" *> typeRow variantLabel <* symbol "]")
  unit    = TyUnit <$ reserve identStyle "Unit"
  bool    = TyBool <$ reserve identStyle "Bool"
  nat     = TyNat <$ reserve identStyle "Nat"

typeRow :: Parser LabelName -> Parser TypeRow
typeRow parseLabel = do
  finitePart <-
    Map.fromList <$> (typeRowPair parseLabel `sepBy` reserve opsStyle ",")
  TyRow finitePart <$> typeRowCofinite

typeRowPair :: Parser LabelName -> Parser (LabelName, TypePresence)
typeRowPair parseLabel = do
  label <- parseLabel
  reserve opsStyle ":"
  presence <- typePresence
  return (label, presence)

typePresence :: Parser TypePresence
typePresence =
  (Absent <$ reserve identStyle "Absent")
    <|> (Present <$> (reserve identStyle "Present" *> parseType))

typeRowCofinite :: Parser TypeRowCofinite
typeRowCofinite =
  (CofRowVar <$> (symbol "|" *> ident identStyle)) <|> return CofAllAbsent

reservedIdents :: H.HashSet String
reservedIdents = H.fromList
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
  , "Unit"
  , "Bool"
  , "Nat"
  , "Rec"
  , "Ref"
  , "Absent"
  , "Present"
  , "Row"
  , "Presence"
  ]

reservedOps :: H.HashSet String
reservedOps = H.fromList
  [".", ",", ":", "->", "=>", "::", ":", "|", "=", "*", "`", ":=", "!"]

identStyle :: IdentifierStyle Parser
identStyle = IdentifierStyle { _styleName              = "identifer"
                             , _styleStart             = letter
                             , _styleLetter            = alphaNum
                             , _styleReserved          = reservedIdents
                             , _styleHighlight         = Identifier
                             , _styleReservedHighlight = ReservedIdentifier
                             }

opsStyle :: IdentifierStyle Parser
opsStyle = IdentifierStyle { _styleName              = "operator"
                           , _styleStart             = _styleLetter opsStyle
                           , _styleLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
                           , _styleReserved          = reservedOps
                           , _styleHighlight         = Operator
                           , _styleReservedHighlight = ReservedOperator
                           }

chainedPrefix :: Parser (a -> a) -> Parser (a -> a)
chainedPrefix p = chainl1 p (return (.))

chainedPostfix :: Parser (a -> a) -> Parser (a -> a)
chainedPostfix p = chainr1 p (return $ flip (.))
