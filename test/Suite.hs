module Main (main) where

import Data.Aeson
import Data.ByteString.Lazy.Char8 as BS
import Data.List.NonEmpty (NonEmpty(..))
import Data.Org as Org
import Data.Time
import Path.Posix
import Test.HUnit

import Lib
import Lib.Telegram as Tg

main :: IO ()
main = do
  runTestTTAndExit $ test
    [ "parser"    ~: parserTest
    , "converter" ~: test
      [ blokifyTest
      ]
    ]

parserTest :: Test
parserTest = "fixtures/simple.json" ~: do
  simple <- BS.readFile "test/fixtures/simple.json"
  photo <- parseRelFile "foo/bar.png"
  date1 <- fromTS "2000-01-01T12:45:00"
  date2 <- fromTS "2000-01-02T09:00:00"
  decode simple @?= Just (
    Channel "channel"
    [ Message
      { mId    = 1
      , mType  = MTService
      , mDate  = date1
      , mPhoto = Nothing
      , mText  = MessageText [CPlain ""]
      }
    , Message
      { mId    = 2
      , mType  = MTMessage
      , mDate  = date2
      , mPhoto = Just photo
      , mText  =
        MessageText
        [ CPlain "plain text"
        , CBold "accent"
        , CPre (Just "haskell") "preformatted"
        , CHashtag "todo"
        , CItalic "emphasis"
        , CLink (Tg.Link "https://haskell.org")
        , CMention "robot"
        , CCode "putStrLn"
        , CTextLink "title" (Tg.Link "https://hackage.haskell.org")
        ]
      }
    ])

blokifyTest :: Test
blokifyTest = test
  [ "empty is empty" ~: blokify [] ~=? []

  , "no hashtags" ~:
    blokify [CHashtag "foo", CPlain "bar"]
    ~?=
    [Paragraph (Plain "foo" :| [Plain "bar"])]

  , "inline markup" ~: test
    [ "bold"      ~: blokify [CBold   "foo"] ~?= p (Bold "foo")
    , "italic"    ~: blokify [CItalic "bar"] ~?= p (Italic "bar")
    , "code"      ~: blokify [CCode   "baz"] ~?= p (Verbatim "baz")

    , "link"      ~:
      blokify [CLink $ Tg.Link "url"]
      ~?=
      p (Org.Link (URL "url") Nothing)

    , "text link" ~:
      blokify [CTextLink "title" (Tg.Link "url")]
      ~?=
      p (Org.Link (URL "url") (Just "title"))

    , "mention"   ~:
      blokify [CMention "who"]
      ~?=
      p (Org.Link (URL "https://t.me/who") (Just "@who"))
    ]

  , "punctuation" ~:
    blokify [CBold "bold", CPlain ", plain, ", CItalic "italic"]
    ~?=
    [Paragraph (Bold "bold" :| [Punct ',', Plain "plain,", Italic "italic"])]

  , "whitespace trimming" ~:
    blokify
      [ CPlain "  p "
      , CBold "   b "
      , CItalic " i  "
      , CCode "   c   "
      , CPlain "  end.\n "
      ]
      ~?=
      [ Paragraph
        (Plain "p" :|
         [ Bold "b"
         , Italic "i"
         , Verbatim "c"
         , Plain "end.\n"
         ])
      ]

  , "paragraph grouping" ~:
    blokify
      [ CPlain "Hello"
      , CBold "World"
      , CPre (Just "haskell") "x :: Int\nx = 42"
      , CPlain "Yay!"
      ]
      ~?=
      [ Paragraph (Plain "Hello" :| [Bold "World"])
      , Code (Just $ Language "haskell") "x :: Int\nx = 42"
      , Paragraph (Plain "Yay!" :| [])
      ]
  ]
  where
    p w = [Paragraph (w :| [])]

-- helpers

fromTS :: MonadFail m => String -> m LocalTime
fromTS = parseTimeM False defaultTimeLocale "%FT%T"
