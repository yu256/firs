{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Codegen (printLLVM)
import Control.Monad (when)
import Data.Maybe (listToMaybe)
import qualified Data.Text.Lazy.IO as TLIO
import Parser
import System.Environment (getArgs)
import System.IO
import Text.Megaparsec.Error

main :: IO ()
main = do
  args <- getArgs
  let (fileName, debugFlag, outputPath) = parseArgs args
  case fileName of
    Just fName -> do
      input <- readFile fName
      case parseProgram input of
        Left err -> ePutStrLn $ errorBundlePretty err
        Right (externs, ast) -> do
          when debugFlag $ print ast
          TLIO.writeFile outputPath $ printLLVM externs ast
    Nothing -> ePutStrLn "Usage: <program> <source-file> [-o <output>] [--debug]"
  where
    ePutStrLn = hPutStrLn stderr

parseArgs :: [String] -> (Maybe String, Bool, String)
parseArgs args =
  let fileName = listToMaybe [arg | arg <- args, head arg /= '-']
      debugFlag = "--debug" `elem` args
      outputPath = case dropWhile (/= "-o") args of
        ("-o" : out : _) -> out
        _ -> "a.ll"
   in (fileName, debugFlag, outputPath)
