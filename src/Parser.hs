{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parser (parseProgram, Expr (..), NumLit (..), Type) where

import Control.Monad (void)
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
  | ArrayLit [Expr]
  | UnaryOp String Expr
  | BinaryOp String Expr Expr
  | IfExpr Expr Expr (Maybe Expr)
  | FuncCall Expr [Expr]
  | Block (NonEmpty Expr)
  | Bind String Expr
  | VarDeclare String Type Expr
  | Assign String Expr
  | FuncDeclare String [(String, Type)] Type Expr -- Function Name, [(ArgName, Type)], ReturnType, Body
  deriving (Show, Eq)

data NumLit
  = IntLit Integer
  | LongLit Integer
  | FloatLit Float
  | DoubleLit Double
  deriving (Show, Eq)

type Type = NonEmpty String

type Parser = Parsec Void String

type Externs = [String]

type Program = (Externs, [Expr])

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

typeAnnotation :: Parser (NonEmpty String)
typeAnnotation = fromList <$> (symbol1 ':' *> some identifier)

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

pArrayLit :: Parser Expr
pArrayLit = ArrayLit <$> (symbol1 '[' *> pExpr `sepBy` symbol1 ',' <* symbol1 ']')

pVar :: Parser Expr
pVar = Var <$> identifier

pBind :: Parser Expr
pBind = Bind <$> identifier <* symbol1 '=' <*> pExpr

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

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable
  where
    pFuncCallArgs :: Parser (Expr -> Expr)
    pFuncCallArgs = flip FuncCall <$> parens (pExpr `sepBy` symbol1 ',')

    operatorTable :: [[Operator Parser Expr]]
    operatorTable =
      [ [Postfix pFuncCallArgs],
        [Prefix $ UnaryOp "!" <$ symbol1 '!'],
        [Prefix $ UnaryOp "deref" <$ symbol1 '*'],
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
        [InfixL $ BinaryOp "|>" <$ symbol "|>"],
        [InfixL $ BinaryOp "$" <$ symbol1 '$']
      ]

pTerm :: Parser Expr
pTerm =
  choice
    [ parens pExpr, -- 括弧で囲まれた式を最優先で扱う
      pBlock,
      pArrayLit,
      pNumLit,
      pCharLit,
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

pProgram :: Parser Program
pProgram = do
  sc
  externs <- fromMaybe [] <$> optional pExterns
  (externs,) <$> pExpr `sepEndBy` exprSeparator
  where
    pExterns = symbol "extern" *> parens (identifier `sepBy` symbol1 ',')

parseProgram :: String -> Either (ParseErrorBundle String Void) Program
parseProgram = parse pProgram ""
