{-# LANGUAGE OverloadedStrings #-}

module Client255.Type
    where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Text
import Data.Time
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict as H
import System.Locale

data Tweet = Tweet
    { createdAt :: UTCTime
    , tweetId :: Integer
    , text :: Text
    , user :: User
    } deriving (Show)

instance FromJSON Tweet where
    parseJSON (Object tweet) = Tweet
        <$> twitterTime tweet
        <*> tweet .: "id"
        <*> tweet .: "text"
        <*> tweet .: "user"
      where
        twitterTime :: HashMap Text Value -> Parser UTCTime
        twitterTime object = case H.lookup "created_at" tweet of
            Nothing -> fail "Cannot find created_at"
            Just (String string) -> case (parseTime defaultTimeLocale "%a %b %d %T %z %Y" (unpack string)) of
                Nothing -> fail "Cannot parse"
                Just time -> return time

data User = User
    { userId :: Integer
    , name :: Text
    , screenName :: Text
    } deriving (Show)

instance FromJSON User where
    parseJSON (Object user) = User
        <$> user .: "id"
        <*> user .: "name"
        <*> user .: "screen_name"
