{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module StdLib (externCFunc) where

import Data.List (nub, (\\))
import qualified Data.Map as Map
import Data.Traversable (for)
import LLVM.AST hiding (function)
import qualified LLVM.AST.Type as T
import LLVM.IRBuilder

cFunctions :: MonadModuleBuilder m => Map.Map String (m Operand)
cFunctions =
  Map.fromList
    [ ("printf", externVarArgs "printf" [T.ptr T.i8] T.i32),
      ("sprintf", externVarArgs "sprintf" [T.ptr T.i8, T.ptr T.i8, T.i32] T.i32),
      ("atoi", extern "atoi" [T.ptr T.i8] T.i32),
      ("strlen", extern "strlen" [T.ptr T.i8] T.i32)
    ]

externCFunc :: Monad m => [String] -> ModuleBuilderT m (Either String [(String, Operand)])
externCFunc functions =
  let duplicates = functions \\ nub functions
   in if null duplicates
        then fmap sequence . for functions $ \functionName -> do
          case Map.lookup functionName cFunctions of
            Just op -> Right . (functionName,) <$> op
            Nothing -> pure . Left $ functionName ++ " is not found."
        else pure . Left $ "Duplicate function names found: " ++ show duplicates
