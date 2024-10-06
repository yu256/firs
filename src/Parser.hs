{-# LANGUAGE OverloadedStrings #-}

module Parser (parseProgram) where

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
  deriving (Show, Eq)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens pExpr_ = symbol "(" *> pExpr_ <* symbol ")"

identifier :: Parser String
identifier = lexeme $ (:) <$> letterChar <*> many alphaNumChar

typeAnnotation :: Parser String
typeAnnotation = symbol ":" *> identifier

pIntLit :: Parser Expr
pIntLit = IntLit <$> lexeme L.decimal

pStringLit :: Parser Expr
pStringLit = StringLit <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

pArrayLit :: Parser Expr
pArrayLit = ArrayLit <$> (symbol "[" *> pExpr `sepBy` symbol "," <* symbol "]")

pVar :: Parser Expr
pVar = Var <$> identifier

pValDeclare :: Parser Expr
pValDeclare = ValDeclare <$> identifier <*> typeAnnotation <* symbol "=" <*> pExpr

pVarDeclare :: Parser Expr
pVarDeclare = VarDeclare <$> identifier <*> typeAnnotation <* symbol ":=" <*> pExpr

pAssign :: Parser Expr
pAssign = Assign <$> identifier <* symbol "<-" <*> pExpr

pIfExpr :: Parser Expr
pIfExpr =
  let cond = symbol "if" *> pExpr <* symbol "then"
      elseExpr = optional $ symbol "else" *> pExpr
   in IfExpr <$> cond <*> pExpr <*> elseExpr

pFuncDeclare :: Parser Expr
pFuncDeclare =
  let arg = (,) <$> identifier <*> typeAnnotation
      argAndTypes = parens $ arg `sepBy` symbol ","
      body = symbol "=" *> pExpr
   in FuncDeclare <$> identifier <*> argAndTypes <*> typeAnnotation <*> body

pFuncCall :: Parser Expr
pFuncCall =
  let fn = parens pExpr <|> pVar
      args = parens $ pExpr `sepBy` symbol ","
   in FuncCall <$> fn <*> args

pBinaryOp :: Parser Expr
pBinaryOp = makeExprParser pTerm operatorTable
  where
    operatorTable =
      [ [InfixL $ BinaryOp "*" <$ symbol "*", InfixL $ BinaryOp "/" <$ symbol "/"],
        [InfixL $ BinaryOp "+" <$ symbol "+", InfixL $ BinaryOp "-" <$ symbol "-"]
      ]

pExpr :: Parser Expr
pExpr =
  choice
    [ pIfExpr,
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

pProgram :: Parser [Expr]
pProgram = many pExpr

parseProgram :: String -> Either (ParseErrorBundle String Void) [Expr]
parseProgram = parse pProgram ""
