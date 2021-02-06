{-# LANGUAGE TypeApplications #-}

module Lib where

import Data.Aeson
import Path.Posix

import Lib.Types

convert :: Path a File -> Path a Dir -> IO ()
convert ip _ = do
  i <- eitherDecodeFileStrict' @Channel (toFilePath ip)
  print i
  pure ()
