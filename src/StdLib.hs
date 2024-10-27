{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module StdLib (genPrelude) where

import Control.Monad.State
import Data.String
import LLVM.AST hiding (function)
import qualified LLVM.AST.Type as T
import LLVM.IRBuilder
import Prelude hiding (getLine, putStr, putStrLn, readLn)

genPrelude :: Monad a => ModuleBuilderT a [(String, Operand)]
genPrelude = do
  printf <- externVarArgs "printf" [T.ptr T.i8] T.i32
  scanf <- externVarArgs "scanf" [T.ptr T.i8] T.i32
  sprintf <- externVarArgs "sprintf" [T.ptr T.i8, T.ptr T.i8, T.i32] T.i32
  int <- extern "atoi" [T.ptr T.i8] T.i32

  putStr <- function "putStr" [(T.ptr T.i8, "str")] T.i32 $ \[str] -> do
    _ <- call printf [(str, [])]
    ret $ int32 0

  putStrLn <- function "putStrLn" [(T.ptr T.i8, "str")] T.i32 $ \[str] -> do
    newlineStr <- globalStringPtr "%s\n" "formatStrLn"
    _ <- call printf [(ConstantOperand newlineStr, []), (str, [])]
    ret $ int32 0

  readLn <- function "readLn" [] T.i32 $
    const $ do
      formatStr <- globalStringPtr "%d" "formatStrRead"
      inputPtr <- alloca T.i32 Nothing 0
      _ <- call scanf [(ConstantOperand formatStr, []), (inputPtr, [])]
      result <- load inputPtr 0
      ret result

  getLine <- function "getLine" [] (T.ptr T.i8) $
    const $ do
      buffer <- alloca (T.ptr T.i8) Nothing 0
      formatStr <- globalStringPtr "%255[^\n]" "formatStrGetLine"
      _ <- call scanf [(ConstantOperand formatStr, []), (buffer, [])]
      ret buffer

  string <- function "string" [(T.i32, "val")] (T.ptr T.i8) $ \[val] -> do
    buffer <- alloca (T.ptr T.i8) Nothing 0
    formatStr <- globalStringPtr "%d" "formatStrToString"
    _ <- call sprintf [(buffer, []), (ConstantOperand formatStr, []), (val, [])]
    ret buffer

  pure
    [ ("printf", printf),
      ("scanf", scanf),
      ("sprintf", sprintf),
      ("putStr", putStr),
      ("putStrLn", putStrLn),
      ("readLn", readLn),
      ("getLine", getLine),
      ("int", int),
      ("string", string)
    ]
