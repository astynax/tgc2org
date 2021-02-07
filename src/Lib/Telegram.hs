{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Telegram where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Maybe
import GHC.Generics

import Data.ByteString.Lazy.Char8 as BS
import Data.Text as T
import Data.Time
import Data.Aeson
import Data.Aeson.Types (Parser)
import Deriving.Aeson
import Path.Posix

data Channel
  = Channel
    { cName     :: !Text
    , cMessages :: ![Message]
    } deriving (Generic, Show, Eq)
      deriving (FromJSON) via CustomJSON
               '[FieldLabelModifier (StripPrefix "c", CamelToSnake)
               ] Channel

data Message
  = Message
    { mId    :: !Int
    , mType  :: !MessageType
    , mDate  :: !LocalTime
    , mPhoto :: !(Maybe (Path Rel File))
    , mText  :: !MessageText
    } deriving (Generic, Show, Eq)
      deriving (FromJSON) via CustomJSON
               '[FieldLabelModifier (StripPrefix "m", CamelToSnake)
               ] Message

newtype MessageText =
  MessageText
  { getMessageText :: [Chunk]
  } deriving (Show, Eq)

instance FromJSON MessageText where
  parseJSON = fmap MessageText . \case
    String s -> pure [CPlain s]
    Array xs -> traverse chunkP $ toList xs
    v        -> failWith v
    where
      chunkP v =
        plainP v
        <|> boldP v
        <|> italicP v
        <|> hashtagP v
        <|> linkP v
        <|> mentionP v
        <|> codeP v
        <|> preP v
        <|> textLinkP v
        <|> failWith v
      plainP   = withText "plain" (pure . CPlain)
      boldP    = withType "bold"    $ "text" `as` CBold
      italicP  = withType "italic"  $ "text" `as` CItalic
      hashtagP = withType "hashtag" $ "text" `as` (CHashtag . without "#")
      linkP    = withType "link"    $ "text" `as` (CLink . Link)
      mentionP = withType "mention" $ "text" `as` (CMention . without "@")
      codeP    = withType "code"    $ "text" `as` CCode
      preP = withType "pre" $ \o -> CPre
        <$> o .:? "language"
        <*> o .:  "text"
      textLinkP = withType "text_link" $ \o -> CTextLink
        <$> o .: "text"
        <*> (Link <$> o .: "href")
      without v t = fromMaybe t $ T.stripPrefix v t

data Chunk
  = CPlain    !Text
  | CBold     !Text
  | CPre      !(Maybe String) !Text
  | CHashtag  !Text
  | CItalic   !Text
  | CLink     !Link
  | CMention  !Text
  | CCode     !Text
  | CTextLink !Text !Link
  deriving (Show, Eq)

newtype Link = Link Text deriving (Show, Eq)

data MessageType
  = MTService
  | MTMessage
  deriving (Show, Eq)

instance FromJSON MessageType where
  parseJSON = \case
    String "service" -> pure MTService
    String "message" -> pure MTMessage
    v                -> failWith v

withType :: String -> (Object -> Parser a) -> Value -> Parser a
withType t f = withObject "Chunk" $ \o -> do
  t' <- o .: "type"
  guard $ t' == t
  f o

as :: Text -> (Text -> a) -> Object -> Parser a
as n f o = f <$> o .: n

failWith :: Value -> Parser a
failWith = fail . BS.unpack . encode
