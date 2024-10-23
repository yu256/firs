{-# LANGUAGE OverloadedStrings #-}

module Codegen (printLLVM) where

import Control.Monad.State
import qualified Data.Bifunctor as Bifunctor
import Data.Foldable (traverse_)
import Data.String
import Data.Text.Lazy (Text)
import LLVM.AST hiding (Type, args, function)
import LLVM.AST.AddrSpace (AddrSpace (AddrSpace))
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Instruction as I
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Operand as O
import qualified LLVM.AST.Type as T
import LLVM.IRBuilder
import LLVM.Pretty
import Parser (Expr (..), Type)

type Codegen = State CodegenState

type CodegenIRBuilder = IRBuilderT (ModuleBuilderT Codegen)

data CodegenState = CodegenState
  { symTable :: [(String, Operand)],
    _dummy :: ()
  }

initialCodegenState :: CodegenState
initialCodegenState =
  CodegenState
    [ ("printf", O.ConstantOperand $ C.GlobalReference (T.ptr (T.FunctionType T.i32 [T.ptr T.i8] True)) (Name "printf"))
    ]
    ()

toLLVMType :: Type -> T.Type
toLLVMType ["Int"] = T.i32
toLLVMType ["String"] = T.ptr T.i8
toLLVMType _ = T.void

codegenExpr :: Expr -> CodegenIRBuilder Operand
codegenExpr (IntLit n) = pure $ O.ConstantOperand $ C.Int 32 n
codegenExpr (Var name) = lift $ lift $ lookupVar name
codegenExpr (ValDeclare name _type expr) = do
  val <- codegenExpr expr
  modify $ \s -> s {symTable = (name, val) : symTable s}
  pure val
codegenExpr (VarDeclare name _type expr) = do
  val <- codegenExpr expr
  modify $ \s -> s {symTable = (name, val) : symTable s}
  pure val
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
    _ -> error $ "Unknown binary operator: " ++ op
codegenExpr (FuncCall (Var fn) args) = do
  args' <- traverse codegenExpr args
  funcOp <- lift $ lift $ lookupVar fn
  case funcOp of
    O.ConstantOperand (C.GlobalReference (T.PointerType (T.FunctionType retType _ _) _) _) ->
      emitInstr retType $ I.Call Nothing CC.C [] (Right funcOp) [(arg, []) | arg <- args'] [] []
    _ -> error $ "Calling non-function type: " ++ fn
codegenExpr expr@FuncDeclare {} = lift $ generateDef expr
codegenExpr (StringLit str) =
  ConstantOperand <$> globalStringPtr str (fromString $ "str." ++ str)
codegenExpr (Block exprs) =
  last <$> traverse codegenExpr exprs
codegenExpr _ = error "Unsupported expression"

generateBasicBlock :: Expr -> ModuleBuilderT Codegen [BasicBlock]
generateBasicBlock body =
  snd
    <$> runIRBuilderT
      emptyIRBuilder
      (block `named` "entry" *> codegenExpr body >>= ret)

lookupVar :: String -> Codegen Operand
lookupVar name = do
  symTbl <- gets symTable
  case lookup name symTbl of
    Just op -> pure op
    Nothing -> error $ "Unknown variable: " ++ name

generateDef :: Expr -> ModuleBuilderT Codegen Operand
generateDef (FuncDeclare name params retType body) = do
  blocks <- generateBasicBlock body
  let args = map (Bifunctor.second toLLVMType) params
  let returnType = toLLVMType retType
  let funcType = T.FunctionType returnType (map snd args) False
  let fn = O.ConstantOperand $ C.GlobalReference (T.PointerType funcType (AddrSpace 0)) (Name (fromString name))
  emitDefn $ genFunction name args returnType blocks
  modify $ \s -> s {symTable = (name, fn) : symTable s}
  pure fn
  where
    genFunction :: String -> [(String, T.Type)] -> T.Type -> [BasicBlock] -> Definition
    genFunction name_ args retType_ body_ =
      let parameters = map (\(n, ty) -> G.Parameter ty (Name (fromString n)) []) args
       in GlobalDefinition
            G.functionDefaults
              { G.name = fromString name_,
                G.parameters = (parameters, False),
                G.returnType = retType_,
                G.basicBlocks = body_
              }
generateDef _ = error "Only functions can be declared at the top level."

codegenProgram :: [Expr] -> Module
codegenProgram exprs =
  evalState
    ( buildModuleT "program" $ do
        _ <- externVarArgs "printf" [T.ptr T.i8] T.i32
        traverse_ generateDef exprs
    )
    initialCodegenState

printLLVM :: [Expr] -> Text
printLLVM = ppllvm . codegenProgram