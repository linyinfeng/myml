{-# LANGUAGE LambdaCase #-}

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
    return (termIf t1 t2)
  opAbs = do
    reserve identStyle "\x3bb" <|> reserve identStyle "\\"
    xs <- some (ident identStyle) <* symbol "."
    return (paramsToAbs xs)
  opLet = do
    reserve identStyle "let"
    x  <- ident identStyle
    t1 <- equalPair
    reserve identStyle "in"
    return (TmLet x t1)
  opApp       = return TmApp
  opRcdAccess = TmApp . TmRcdAccess <$> (symbol "." *> ident identStyle)
  opRcdExtend = do
    ext <-
      try
      $  (TmRcdExtend <$ reserve identStyle "extend" <|> TmRcdUpdate <$ reserve
           identStyle
           "update"
         )
      <* symbol "{"
    pairs <- recordPair `sepBy` symbol ","
    _     <- symbol "}"
    return (flip (labaledApps ext) pairs)
  opMatchExtend = do
    ext <-
      try
      $  (   TmMatchExtend
         <$  reserve identStyle "extend"
         <|> TmMatchUpdate
         <$  reserve identStyle "update"
         )
      <* symbol "["
    pairs <- matchPair `sepBy` symbol ","
    _     <- symbol "]"
    return (flip (labaledApps ext) pairs)
  opAssign = (\a b -> TmApp (TmApp TmAssign a) b) <$ reserve identStyle ":="
  opSeq    = termSeq <$ try (symbol ";" <* notFollowedBy (char ';'))
  opClass  = do
    reserve identStyle "class"
    inherits <- many
      (do
        reserve identStyle "inherit"
        t <- parseTerm
        reserve identStyle "as"
        x <- ident identStyle
        return (t, x)
      )
    return (deriveTermClass . TermClass inherits)

parseTermAtom :: Parser Term
parseTermAtom =
  (   rcd
    <|> match
    <|> variant
    <|> rcdAndMatchOps
    <|> access
    <|> simplePrimitives
    <|> int
    <|> charLit
    <|> compareCharSharp
    <|> stringLit
    <|> var
    <|> tupleOrParen
    )
    <?> "termAtom"
 where
  var            = TmVar <$> ident identStyle
  rcd            = recordLiteral <$> braces (recordPair `sepBy` symbol ",")
  match          = matchLiteral <$> brackets (matchPair `sepBy` symbol ",")
  variant        = TmVariant <$> variantLabel
  rcdAndMatchOps = try matchOps <|> try rcdOps
  matchOps       = do
    op <-
      TmMatchExtend <$ reserve identStyle "extend" <|> TmMatchUpdate <$ reserve
        identStyle
        "update"
    op <$> parens variantLabel
  rcdOps = do
    op <- TmRcdExtend <$ reserve identStyle "extend" <|> TmRcdUpdate <$ reserve
      identStyle
      "update"
    op <$> parens recordLabel
  access = TmRcdAccess <$> (reserve identStyle "access" *> parens recordLabel)
  int              = TmInteger <$> try integer
  charLit          = TmChar <$> charLiteral
  compareCharSharp = TmCharCompare <$ reserve identStyle "charCompare#"
  stringLit        = deriveString <$> stringLiteral
  tupleOrParen =
    (\case
        []  -> TmUnit
        [t] -> t
        l   -> recordLiteral (zip (map show [1 :: Integer ..]) l)
      )
      <$> parens (parseTerm `sepBy` symbol ",")
  simplePrimitives = choice
    (map
      (\(t, s) -> t <$ reserve identStyle s)
      [ (TmRef           , "ref")
      , (TmDeref         , "!")
      , (TmAssign        , ":=#")
      , (TmUnit          , "unit")
      , (termTrue        , "true")
      , (termFalse       , "false")
      , (TmIntegerPlus   , "integerPlus#")
      , (TmIntegerMul    , "integerMul#")
      , (TmIntegerAbs    , "integerAbs#")
      , (TmIntegerSignum , "integerSignum#")
      , (TmIntegerNegate , "integerNegate#")
      , (TmIntegerQuotRem, "integerQuotRem#")
      , (TmIntegerCompare, "integerCompare#")
      , (TmNew           , "new")
      , (TmIOGetChar     , "ioGetChar#")
      , (TmIOPutChar     , "ioPutChar#")
      , (termSelf        , "self")
      ]
    )

paramsToAbs :: [VarName] -> Term -> Term
paramsToAbs xs t =
  foldr (\x -> if x == "_" then termWildcardAbs else TmAbs x) t xs

recordPair :: Parser (LabelName, Term)
recordPair = (,) <$> recordLabel <*> equalPair

matchPair :: Parser (LabelName, Term)
matchPair = (,) <$> variantLabel <*> equalPair

equalPair :: Parser Term
equalPair = do
  params <- many (ident identStyle)
  reserve identStyle "="
  paramsToAbs params <$> parseTerm

recordLabel :: Parser LabelName
recordLabel = ident identStyle

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
  (var <|> rcd <|> variant <|> unit <|> int <|> tyChar <|> parens parseType)
    <?> "typeAtom"
 where
  var = TyVar <$> ident identStyle
  rcd =
    TyRecord <$> (symbol "{" *> parseTypeRow (ident identStyle) <* symbol "}")
  variant =
    TyVariant <$> (symbol "[" *> parseTypeRow variantLabel <* symbol "]")
  unit   = TyUnit <$ reserve identStyle "Unit"
  int    = TyInteger <$ reserve identStyle "Integer"
  tyChar = TyChar <$ reserve identStyle "Char"

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
    _ <- symbol "."
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
