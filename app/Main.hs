module Main where

import Path.Posix

import qualified Lib

main :: IO ()
main = do
  i <- parseRelFile "example/result.json"
  o <- parseRelDir "target"
  Lib.convert i o
