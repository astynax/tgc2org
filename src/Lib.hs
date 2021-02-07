module Lib where

import Data.Char
import Data.List.NonEmpty (NonEmpty(..))
import System.Exit

import Data.Aeson
import qualified Data.Map.Strict as Map
import Data.Org as Org
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import Path.Posix

import Lib.Telegram as Tg

convert :: Path a File -> Path a Dir -> IO ()
convert ip _ = do
  ei <- eitherDecodeFileStrict' $ toFilePath ip
  i <- case ei of
    Right v -> pure v
    Left e  -> do
      putStrLn e
      exitWith (ExitFailure 1)
  let o@(OrgFile _ d@(OrgDoc _ os)) = organize i
  TIO.putStrLn $ prettyOrgFile
    o{orgDoc = d
       {docSections = reverse . take 10 $ reverse os
       }
     }

organize :: Channel -> OrgFile
organize Channel{cName = title, cMessages = ms} =
  OrgFile (Map.fromList [("TITLE", title)]) $ OrgDoc []
    [ Section
      { sectionHeading = orgTS t :| []
      , sectionTags    = tags
      , sectionDoc     = OrgDoc (blokify cs <> bPhoto) []
      }
    | Message{mDate = t, mPhoto = mp, mText = MessageText cs} <- posts
    , let tags = [T.toLower h | CHashtag h <- cs]
    , let bPhoto = [photoBlock p | Just p <- pure mp]
    ]
  where
    posts = [m | m@Message{mType = MTMessage} <- ms]

blokify :: [Chunk] -> [Block]
blokify = go []
  where
    go acc []     = par acc
    go acc (c:cs) = case c of
      CPre ml t  -> concat
        [ par acc
        , [Code (Language <$> ml) t]
        , go [] cs
        ]
      CHashtag  t             -> withWord $ Plain    t
      CBold     t             -> withWord $ Bold     $ trim t
      CItalic   t             -> withWord $ Italic   $ trim t
      CCode     t             -> withWord $ Verbatim $ trim t
      CPlain    t             ->
        case unconsPunct t of
          ([], _)  -> withWord $ Plain $ trim t
          (ps, tt) -> go (acc <> map Punct ps <> [Plain $ trim tt]) cs
      CLink       (Tg.Link u) -> withWord $ Org.Link (URL u) Nothing
      CTextLink t (Tg.Link u) -> withWord $ Org.Link (URL u) (Just t)
      CMention  t             -> withWord $
        Org.Link (URL $ "https://t.me/" <> t) (Just $ "@" <> t)
      where
        withWord w = go (acc <> [w]) cs
    par acc = [Paragraph (w :| ws) | (w : ws) <- pure acc]

photoBlock :: Path Rel File -> Block
photoBlock p = Paragraph $ Image (URL . T.pack $ toFilePath p) :| []

orgTS :: LocalTime -> Words
orgTS = Plain . T.pack . formatTime defaultTimeLocale "<%F %R>"

trim :: Text -> Text
trim = T.dropWhileEnd (== ' ') . T.dropWhile (== ' ')

unconsPunct :: Text -> ([Char], Text)
unconsPunct = go []
  where
    go acc t = case T.uncons t of
      Nothing      -> (reverse acc, "")
      Just (c, tt) ->
        if isPunctuation c
        then go (c : acc) tt
        else (reverse acc, tt)
