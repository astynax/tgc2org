{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Time
import Data.ByteString.Lazy.Char8 as BS
import Data.Aeson
import Path.Posix
import Test.HUnit

import Lib.Types

main :: IO ()
main = do
  simple <- BS.readFile "test/fixtures/simple.json"
  photo <- parseRelFile "foo/bar.png"
  date1 <- fromTS "2000-01-01T12:45:00"
  date2 <- fromTS "2000-01-02T09:00:00"
  runTestTTAndExit $ test
    [ "parser" ~: "simple" ~: decode simple ~=? Just
      (Channel "channel"
       [ Message { mId = 1
                 , mType = MTService
                 , mDate = date1
                 , mPhoto = Nothing
                 , mText = MessageText [CPlain ""]
                 }
       , Message { mId = 2
                 , mType = MTMessage
                 , mDate = date2
                 , mPhoto = Just photo
                 , mText =
                   MessageText
                   [ CPlain "plain text"
                   , CBold "accent"
                   , CPre (Just "haskell") "preformatted"
                   , CHashtag "#todo"
                   , CItalic "emphasis"
                   , CLink (Link "https://haskell.org")
                   , CMention "@robot"
                   , CCode "putStrLn"
                   , CTextLink "title" (Link "https://hackage.haskell.org")
                   ]
                 }
       ]
      )
    ]
  where
    fromTS = parseTimeM False defaultTimeLocale "%FT%T"
