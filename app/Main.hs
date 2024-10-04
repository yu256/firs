module Main (main) where

import Parser
import Text.Megaparsec.Error

main :: IO ()
main = do
  input <- getLine
  case parseProgram input of
    Left err -> putStr $ errorBundlePretty err
    Right exprs -> print exprs