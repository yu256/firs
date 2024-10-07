{-# LANGUAGE OverloadedStrings #-}

module Parser (parseProgram) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Expr
  = IntLit Integer
  | StringLit String
  | Var String
  | ValDeclare String String Expr -- Value Name, Type Name, Value
  | VarDeclare String String Expr -- Variable Name, Type Name, Value
  | Assign String Expr
  | BinaryOp String Expr Expr
  | ArrayLit [Expr]
  | IfExpr Expr Expr (Maybe Expr)
  | FuncDeclare String [(String, String)] String Expr -- Function Name, [(ArgName, Type)], ReturnType, Body
  | FuncCall Expr [Expr]
  | Block [Expr]
  deriving (Show, Eq)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

symbol1 :: Char -> Parser ()
symbol1 c = char c *> sc

parens :: Parser a -> Parser a
parens pExpr_ = symbol1 '(' *> pExpr_ <* symbol1 ')'

identifier :: Parser String
identifier = lexeme $ (:) <$> letterChar <*> many alphaNumChar

typeAnnotation :: Parser String
typeAnnotation = symbol1 ':' *> identifier

pIntLit :: Parser Expr
pIntLit = IntLit <$> lexeme L.decimal

pStringLit :: Parser Expr
pStringLit = StringLit <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

pArrayLit :: Parser Expr
pArrayLit = ArrayLit <$> (symbol1 '[' *> pExpr `sepBy` symbol1 ',' <* symbol1 ']')

pVar :: Parser Expr
pVar = Var <$> identifier

pValDeclare :: Parser Expr
pValDeclare = ValDeclare <$> identifier <*> typeAnnotation <* symbol1 '=' <*> pExpr

pVarDeclare :: Parser Expr
pVarDeclare = VarDeclare <$> identifier <*> typeAnnotation <* symbol ":=" <*> pExpr

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
    pArgs = (,) <$> identifier <*> typeAnnotation

pFuncCall :: Parser Expr
pFuncCall = do
  fn <- parens pExpr <|> pVar
  args <- parens $ pExpr `sepBy` symbol1 ','
  pure $ FuncCall fn args

pBinaryOp :: Parser Expr
pBinaryOp = makeExprParser pTerm operatorTable
  where
    operatorTable =
      [ [InfixL $ BinaryOp "*" <$ symbol1 '*', InfixL $ BinaryOp "/" <$ symbol1 '/'],
        [InfixL $ BinaryOp "+" <$ symbol1 '+', InfixL $ BinaryOp "-" <$ symbol1 '-']
      ]

pExpr :: Parser Expr
pExpr =
  choice
    [ pBlock,
      pIfExpr,
      try pValDeclare,
      try pVarDeclare,
      try pAssign,
      try pFuncDeclare,
      try pFuncCall,
      try pBinaryOp,
      pArrayLit,
      pIntLit,
      pStringLit,
      pVar
    ]

pTerm :: Parser Expr
pTerm =
  choice
    [ pIntLit,
      pStringLit,
      pVar,
      pArrayLit,
      parens pExpr
    ]

exprSeparator :: Parser ()
exprSeparator = void $ lexeme $ char ';' <|> char '\n'

pBlock :: Parser Expr
pBlock = Block <$> (symbol1 '{' *> pExpr `sepEndBy` exprSeparator <* symbol1 '}')

pProgram :: Parser [Expr]
pProgram = pExpr `sepEndBy` exprSeparator

parseProgram :: String -> Either (ParseErrorBundle String Void) [Expr]
parseProgram = parse pProgram ""
