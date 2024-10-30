{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Codegen (printLLVM)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text.Lazy.IO as TLIO
import Parser
import System.Environment (getArgs)
import System.IO
import Text.Megaparsec.Error

main :: IO ()
main = do
  args <- getArgs
  case args of
    fileName : rest -> do
      let outputPath = rest `headOr` "a.ll"
      input <- readFile fileName
      case parseProgram input of
        Left err -> ePutStrLn $ errorBundlePretty err
        Right (externs, ast) -> TLIO.writeFile outputPath $ printLLVM externs ast
    _ -> ePutStrLn "Usage: <program> <source-file> <output>"
  where
    headOr = flip fromMaybe . listToMaybe
    ePutStrLn = hPutStrLn stderr