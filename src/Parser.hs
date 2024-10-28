{-# LANGUAGE OverloadedStrings #-}

module Parser (parseProgram, Expr (..), Type) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Expr
  = Var String
  | IntLit Integer
  | StringLit String
  | BoolLit Bool
  | ArrayLit [Expr]
  | UnaryOp String Expr
  | BinaryOp String Expr Expr
  | IfExpr Expr Expr (Maybe Expr)
  | FuncCall Expr [Expr]
  | Block (NonEmpty Expr)
  | Bind String Expr
  | VarDeclare String Expr
  | Assign String Expr
  | FuncDeclare String [(String, Type)] Type Expr -- Function Name, [(ArgName, Type)], ReturnType, Body
  deriving (Show, Eq)

type Type = NonEmpty String -- ensure nonEmpty

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser ()
symbol = void . L.symbol sc

symbol1 :: Char -> Parser ()
symbol1 = (*> sc) . char

parens :: Parser a -> Parser a
parens = between (symbol1 '(') (symbol1 ')')

identifier :: Parser String
identifier = lexeme $ (:) <$> letterChar <*> many alphaNumChar

pIntLit :: Parser Expr
pIntLit = IntLit <$> lexeme L.decimal

pStringLit :: Parser Expr
pStringLit = StringLit <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

pBoolLit :: Parser Expr
pBoolLit = BoolLit <$> lexeme (True <$ symbol "true" <|> False <$ symbol "false")

pArrayLit :: Parser Expr
pArrayLit = ArrayLit <$> (symbol1 '[' *> pExpr `sepBy` symbol1 ',' <* symbol1 ']')

pVar :: Parser Expr
pVar = Var <$> identifier

pBind :: Parser Expr
pBind = Bind <$> identifier <* symbol1 '=' <*> pExpr

pVarDeclare :: Parser Expr
pVarDeclare = VarDeclare <$> identifier <* symbol ":=" <*> pExpr

pAssign :: Parser Expr
pAssign = Assign <$> identifier <* symbol "<-" <*> pExpr

pIfExpr :: Parser Expr
pIfExpr = do
  cond <- symbol "if" *> pExpr <* symbol "then"
  thenExpr <- pExpr
  elseExpr <- optional $ symbol "else" *> pExpr
  pure $ IfExpr cond thenExpr elseExpr

pFuncDeclare :: Parser Expr
pFuncDeclare = do
  funcName <- identifier
  argAndTypes <- parens $ pArgs `sepBy` symbol1 ','
  returnType <- typeAnnotation
  symbol1 '='
  FuncDeclare funcName argAndTypes returnType <$> pExpr
  where
    typeAnnotation = fromList <$> (symbol1 ':' *> some identifier)
    pArgs = (,) <$> identifier <*> typeAnnotation

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable
  where
    pFuncCallArgs :: Parser (Expr -> Expr)
    pFuncCallArgs = flip FuncCall <$> parens (pExpr `sepBy` symbol1 ',')

    operatorTable :: [[Operator Parser Expr]]
    operatorTable =
      [ [Postfix pFuncCallArgs],
        [Prefix $ UnaryOp "!" <$ symbol1 '!'],
        [InfixL $ BinaryOp "*" <$ symbol1 '*', InfixL $ BinaryOp "/" <$ symbol1 '/'],
        [InfixL $ BinaryOp "+" <$ symbol1 '+', InfixL $ BinaryOp "-" <$ symbol1 '-'],
        [ InfixL $ BinaryOp "==" <$ symbol "==",
          InfixL $ BinaryOp "!=" <$ symbol "!=",
          InfixL $ BinaryOp "<=" <$ symbol "<=",
          InfixL $ BinaryOp ">=" <$ symbol ">=",
          InfixL $ BinaryOp "<" <$ symbol1 '<',
          InfixL $ BinaryOp ">" <$ symbol1 '>'
        ],
        [InfixL $ BinaryOp "&&" <$ symbol "&&", InfixL $ BinaryOp "||" <$ symbol "||"],
        [InfixL $ BinaryOp "|>" <$ symbol "|>"]
      ]

pTerm :: Parser Expr
pTerm =
  choice
    [ parens pExpr, -- 括弧で囲まれた式を最優先で扱う
      pBlock,
      pArrayLit,
      pIntLit,
      pStringLit,
      pBoolLit,
      pIfExpr,
      try pBind,
      try pVarDeclare,
      try pAssign,
      try pFuncDeclare,
      pVar
    ]

exprSeparator :: Parser ()
exprSeparator = symbol1 ';'

pBlock :: Parser Expr
pBlock = Block . fromList <$> (symbol1 '{' *> pExpr `sepEndBy1` exprSeparator <* symbol1 '}')

pProgram :: Parser [Expr]
pProgram = pExpr `sepEndBy` exprSeparator

parseProgram :: String -> Either (ParseErrorBundle String Void) [Expr]
parseProgram = parse pProgram ""
