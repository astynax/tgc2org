{-# LANGUAGE TypeApplications #-}

module Lib where

import System.Exit

import Data.Aeson
import Path.Posix

import Lib.Telegram

convert :: Path a File -> Path a Dir -> IO ()
convert ip _ = do
  ei <- eitherDecodeFileStrict' @Channel (toFilePath ip)
  i <- case ei of
    Right v -> pure v
    Left e  -> do
      putStrLn e
      exitWith (ExitFailure 1)
  let
    messages =
      [ m
      | Channel{cMessages = ms} <- pure i
      , m@Message{mType = MTMessage} <- ms
      ]
  print $ length messages
