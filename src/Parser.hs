{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parser (parseProgram, Expr (..), NumLit (..), Type) where

import Control.Monad.Combinators.Expr
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Expr
  = Var String
  | NumLit NumLit
  | CharLit Char
  | StringLit String
  | BoolLit Bool
  | UnaryOp String Expr
  | BinaryOp String Expr Expr
  | IfExpr Expr Expr (Maybe Expr)
  | FuncCall Expr [Expr]
  | Block (NonEmpty Expr)
  | Bind String Expr
  | FuncDeclare String [(String, Type)] Type Expr -- Function Name, [(ArgName, Type)], ReturnType, Body
  deriving (Show, Eq)

data NumLit
  = IntLit Integer
  | LongLit Integer
  | FloatLit Float
  | DoubleLit Double
  deriving (Show, Eq)

type Type = String

type Parser = Parsec Void String

type Externs = [String]

type Program = (Externs, [Expr])

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = lexeme $ (:) <$> letterChar <*> many alphaNumChar

typeAnnotation :: Parser String
typeAnnotation = symbol ":" *> identifier

pNumLit :: Parser Expr
pNumLit =
  NumLit
    <$> choice
      [ try $ FloatLit <$> lexeme ((try L.float <|> L.decimal) <* (char 'f' <|> char 'F')),
        try $
          DoubleLit
            <$> lexeme
              ( try L.float <* optional (char 'd' <|> char 'D')
                  <|> L.decimal <* (char 'd' <|> char 'D')
              ),
        try $ LongLit <$> lexeme (L.decimal <* (char 'l' <|> char 'L')),
        IntLit <$> lexeme L.decimal
      ]

pCharLit :: Parser Expr
pCharLit = CharLit <$> lexeme (char '\'' *> L.charLiteral <* char '\'')

pStringLit :: Parser Expr
pStringLit = StringLit <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

pBoolLit :: Parser Expr
pBoolLit = BoolLit <$> lexeme (True <$ symbol "true" <|> False <$ symbol "false")

pVar :: Parser Expr
pVar = Var <$> identifier

pBind :: Parser Expr
pBind = Bind <$> identifier <* symbol "=" <*> pExpr

pIfExpr :: Parser Expr
pIfExpr = do
  cond <- symbol "if" *> pExpr <* symbol "then"
  thenExpr <- pExpr
  elseExpr <- optional $ symbol "else" *> pExpr
  pure $ IfExpr cond thenExpr elseExpr

pFuncDeclare :: Parser Expr
pFuncDeclare = do
  funcName <- identifier
  argAndTypes <- parens $ pArgs `sepBy` symbol ","
  returnType <- typeAnnotation
  _ <- symbol "="
  FuncDeclare funcName argAndTypes returnType <$> pExpr
  where
    pArgs = (,) <$> identifier <*> typeAnnotation

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable
  where
    pFuncCallArgs :: Parser (Expr -> Expr)
    pFuncCallArgs = flip FuncCall <$> parens (pExpr `sepBy` symbol ",")

    mkOpParser :: (String -> a) -> String -> Parser a
    mkOpParser f op = f op <$ symbol op

    operatorTable :: [[Operator Parser Expr]]
    operatorTable =
      [ [Postfix pFuncCallArgs],
        [Prefix $ mkOpParser UnaryOp "!"],
        [Prefix $ UnaryOp "deref" <$ symbol "*"],
        [InfixL $ mkOpParser BinaryOp "*", InfixL $ mkOpParser BinaryOp "/"],
        [InfixL $ mkOpParser BinaryOp "+", InfixL $ mkOpParser BinaryOp "-"],
        [ InfixL $ mkOpParser BinaryOp "==",
          InfixL $ mkOpParser BinaryOp "!=",
          InfixL $ mkOpParser BinaryOp "<=",
          InfixL $ mkOpParser BinaryOp ">=",
          InfixL $ mkOpParser BinaryOp "<",
          InfixL $ mkOpParser BinaryOp ">"
        ],
        [InfixL $ mkOpParser BinaryOp "&&", InfixL $ mkOpParser BinaryOp "||"]
      ]

pTerm :: Parser Expr
pTerm =
  choice
    [ parens pExpr, -- 括弧で囲まれた式を最優先で扱う
      pBlock,
      pNumLit,
      pCharLit,
      pStringLit,
      pBoolLit,
      pIfExpr,
      try pBind,
      try pFuncDeclare,
      pVar
    ]

exprSeparator :: Parser String
exprSeparator = symbol ";"

pBlock :: Parser Expr
pBlock = Block . fromList <$> (symbol "{" *> pExpr `sepEndBy1` exprSeparator <* symbol "}")

pProgram :: Parser Program
pProgram = do
  sc
  externs <- fromMaybe [] <$> optional pExterns
  (externs,) <$> pExpr `sepEndBy` exprSeparator
  where
    pExterns = symbol "extern" *> parens (identifier `sepBy` symbol ",")

parseProgram :: String -> Either (ParseErrorBundle String Void) Program
parseProgram = parse pProgram ""