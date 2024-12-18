{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Codegen (printLLVM) where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.State
import qualified Data.Bifunctor as Bifunctor
import Data.Foldable (for_, traverse_)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.String (IsString (fromString))
import Data.Text.Lazy (Text)
import qualified LLVM.AST as AST
import LLVM.AST.AddrSpace (AddrSpace (AddrSpace))
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Instruction as I
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Operand as O
import qualified LLVM.AST.Type as T
import LLVM.AST.Typed (typeOf)
import LLVM.IRBuilder
import LLVM.Pretty (ppllvm)
import Parser (Expr (..), NumLit (..), Type)
import StdLib (externCFunc)

type Codegen = State CodegenState

type CodegenIRBuilder = IRBuilderT (ModuleBuilderT Codegen)

data CodegenState = CodegenState
  { symTable :: [[(String, O.Operand)]],
    strCache :: Map.Map String O.Operand
  }

initialCodegenState :: CodegenState
initialCodegenState =
  CodegenState [] Map.empty

toLLVMType :: Type -> T.Type
toLLVMType = \case
  ["Int"] -> T.i32
  ["Long"] -> T.i64
  ["Float"] -> T.float
  ["Double"] -> T.double
  ["Char"] -> T.i8
  ["Bool"] -> T.i1
  ["String"] -> T.ptr T.i8
  ('A' : 'r' : 'r' : 'a' : 'y' : _) :| [] -> error "Kind of Array type is (* -> *)."
  ('A' : 'r' : 'r' : 'a' : 'y' : len) :| t : rest ->
    case reads len :: [(Int, String)] of
      [(n, "")] -> T.ArrayType (fromIntegral n) (toLLVMType $ t :| rest)
      _ -> error "Invalid Array length in type."
  "Ptr" :| [] -> error "Kind of Pointer type is (* -> *)."
  "Ptr" :| t : rest -> T.ptr . toLLVMType $ t :| rest
  invalid -> error $ "Invalid Type: " ++ show (NE.intersperse " " invalid)

codegenExpr :: Expr -> CodegenIRBuilder O.Operand
codegenExpr (NumLit lit) =
  pure . O.ConstantOperand $ case lit of
    IntLit n -> C.Int 32 n
    LongLit n -> C.Int 64 n
    FloatLit n -> C.Float $ F.Single n
    DoubleLit n -> C.Float $ F.Double n
codegenExpr (CharLit c) = pure $ O.ConstantOperand $ C.Int 8 $ fromIntegral $ fromEnum c
codegenExpr (StringLit str) = do
  cache <- gets strCache -- 再定義しないようキャッシュする
  case Map.lookup str cache of
    Just op -> pure op
    Nothing -> do
      op <- O.ConstantOperand <$> globalStringPtr str (fromString $ "str." ++ str)
      modify $ \s -> s {strCache = Map.insert str op (strCache s)}
      pure op
codegenExpr (BoolLit bool) =
  pure $ O.ConstantOperand $ C.Int 1 $ if bool then 1 else 0
codegenExpr (ArrayLit exprs) = do
  ops <- traverse codegenExpr exprs
  let elemType = case ops of
        [] -> error "Empty array literals are not supported."
        (op1 : _) -> typeOf op1
      arrType = T.ArrayType (fromIntegral $ length ops) elemType

  unless (all ((== elemType) . typeOf) ops) $
    error "All elements in an array literal must be of the same type."

  arrPtr <- alloca arrType Nothing 0

  for_ (zip [0 ..] ops) $ \(i, op) -> do
    elemPtr <- gep arrPtr [int32 0, int32 i]
    store elemPtr 0 op

  pure arrPtr
codegenExpr (Var name) = lift $ lift $ lookupVar name
codegenExpr (Bind name expr) = do
  val <- codegenExpr expr
  lift $ lift $ addVar name val
  pure val
codegenExpr (VarDeclare name typeName expr) = do
  let llvmType = toLLVMType typeName
  ptr <- alloca llvmType Nothing 0
  val <- codegenExpr expr
  store ptr 0 val
  lift $ lift $ addVar name ptr
  pure ptr
codegenExpr (Assign name expr) = do
  ptr <- lift $ lift $ lookupVar name
  val <- codegenExpr expr
  store ptr 0 val
  pure val
codegenExpr (UnaryOp op hs) = do
  hs' <- codegenExpr hs
  case op of
    "!" -> emitInstr T.i1 $ I.Xor hs' (O.ConstantOperand $ C.Int 1 1) []
    "deref" -> case typeOf hs' of
      T.PointerType t _ -> emitInstr t $ I.Load False hs' Nothing 0 []
      t -> error $ show t ++ "cannot be dereferenced."
    _ -> error $ "Unknown unary operator: " ++ op
codegenExpr (BinaryOp "|>" lhs rhs) =
  case rhs of
    FuncCall fn args -> codegenExpr $ FuncCall fn (lhs : args)
    Var _ -> codegenExpr $ FuncCall rhs [lhs]
    _ -> error $ "Invalid expression for |> operator: " ++ show rhs
codegenExpr (BinaryOp "$" lhs rhs) =
  case lhs of
    FuncCall fn args -> codegenExpr $ FuncCall fn (args ++ [rhs])
    Var _ -> codegenExpr $ FuncCall lhs [rhs]
    _ -> error $ "Invalid expression for $ operator: " ++ show rhs
codegenExpr (BinaryOp op lhs rhs) = do
  lhs' <- codegenExpr lhs
  rhs' <- codegenExpr rhs
  case op of
    "+" -> emitInstr T.i32 $ I.Add False False lhs' rhs' []
    "-" -> emitInstr T.i32 $ I.Sub False False lhs' rhs' []
    "*" -> emitInstr T.i32 $ I.Mul False False lhs' rhs' []
    "/" -> emitInstr T.i32 $ I.SDiv False lhs' rhs' []
    "==" -> emitInstr T.i1 $ I.ICmp IP.EQ lhs' rhs' []
    "!=" -> emitInstr T.i1 $ I.ICmp IP.NE lhs' rhs' []
    "<" -> emitInstr T.i1 $ I.ICmp IP.SLT lhs' rhs' []
    ">" -> emitInstr T.i1 $ I.ICmp IP.SGT lhs' rhs' []
    "<=" -> emitInstr T.i1 $ I.ICmp IP.SLE lhs' rhs' []
    ">=" -> emitInstr T.i1 $ I.ICmp IP.SGE lhs' rhs' []
    "&&" -> emitInstr T.i1 $ I.And lhs' rhs' []
    "||" -> emitInstr T.i1 $ I.Or lhs' rhs' []
    _ -> error $ "Unknown binary operator: " ++ op
codegenExpr (FuncCall (Var varName) args) = do
  funcOp <- lift $ lift $ lookupVar varName
  args' <- traverse codegenExpr args
  case funcOp of
    O.ConstantOperand (C.GlobalReference (T.PointerType (T.FunctionType retType _ _) _) _) ->
      emitInstr retType $ I.Call Nothing CC.C [] (Right funcOp) [(arg, []) | arg <- args'] [] []
    _ -> error $ "Calling non-function type: " ++ varName
codegenExpr expr@FuncDeclare {} = lift $ generateDef expr
codegenExpr (Block exprs) = do
  lift $ lift pushScope
  result <- NE.last <$> traverse codegenExpr exprs
  lift $ lift popScope
  pure result
codegenExpr (IfExpr condExpr thenExpr mayBeElseExpr) = do
  cond <- codegenExpr condExpr

  thenBlock <- freshName "then"
  mergeBlock <- freshName "merge"

  case mayBeElseExpr of
    Just elseExpr -> do
      elseBlock <- freshName "else"

      condBr cond thenBlock elseBlock

      emitBlockStart thenBlock
      thenVal <- codegenExpr thenExpr
      br mergeBlock

      emitBlockStart elseBlock
      elseVal <- codegenExpr elseExpr
      br mergeBlock

      emitBlockStart mergeBlock

      phi [(thenVal, thenBlock), (elseVal, elseBlock)]
    Nothing -> do
      condBr cond thenBlock mergeBlock

      emitBlockStart thenBlock
      _ <- codegenExpr thenExpr
      br mergeBlock

      emitBlockStart mergeBlock
      pure $ O.ConstantOperand $ C.Undef T.void
codegenExpr e = error $ "Unsupported expression: " ++ show e

generateBasicBlock :: Expr -> ModuleBuilderT Codegen [AST.BasicBlock]
generateBasicBlock body =
  snd
    <$> runIRBuilderT
      emptyIRBuilder
      (block `named` "entry" *> codegenExpr body >>= ret)

lookupVar :: String -> Codegen O.Operand
lookupVar name =
  gets (lookupInScopes . symTable) >>= \case
    Just op -> pure op
    Nothing -> error $ "Unknown variable: " ++ name
  where
    lookupInScopes [] = Nothing
    lookupInScopes (scope : rest) =
      lookup name scope <|> lookupInScopes rest

pushScope :: Codegen ()
pushScope = modify $ \s -> s {symTable = [] : symTable s}

popScope :: Codegen ()
popScope = modify $ \s -> s {symTable = tail $ symTable s}

-- 現在のスコープにOperandを追加する
addVar :: String -> O.Operand -> Codegen ()
addVar name op = modify $ \s ->
  case symTable s of
    currentScope : rest -> s {symTable = ((name, op) : currentScope) : rest}
    [] -> s

generateDef :: Expr -> ModuleBuilderT Codegen O.Operand
generateDef (FuncDeclare name params retType body) = do
  let args = map (Bifunctor.second toLLVMType) params
  let returnType = toLLVMType retType
  let funcType = T.FunctionType returnType (map snd args) False
  let fn = O.ConstantOperand $ C.GlobalReference (T.PointerType funcType (AddrSpace 0)) (fromString name)

  lift $ addVar name fn

  -- lift pushScope 下の処理で同時にScopeをpushしている

  let paramOperands = map (\(paramName, paramType) -> (paramName, O.LocalReference paramType (fromString paramName))) args
  modify $ \s -> s {symTable = paramOperands : symTable s}

  blocks <- generateBasicBlock body

  emitDefn $
    AST.GlobalDefinition
      G.functionDefaults
        { G.name = fromString name,
          G.parameters = (map (\(n, ty) -> G.Parameter ty (fromString n) []) args, False),
          G.returnType = returnType,
          G.basicBlocks = blocks
        }

  lift popScope
  pure fn
generateDef _ = error "Only functions can be declared at the top level."

codegenProgram :: [String] -> [Expr] -> AST.Module
codegenProgram externs exprs =
  evalState
    ( buildModuleT "program" $ do
        externCFunc externs >>= \case
          Right ops -> put $ CodegenState [ops] Map.empty
          Left l -> error l
        traverse_ generateDef exprs
    )
    initialCodegenState

printLLVM :: [String] -> [Expr] -> Text
printLLVM externs = ppllvm . codegenProgram externs
