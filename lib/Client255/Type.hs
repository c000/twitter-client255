{-# LANGUAGE OverloadedStrings #-}

module Client255.Type
    where

import Control.Applicative
import Data.Text
import Data.Time
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict as H
import System.Locale

data Tweet = Tweet
    { createdAt :: UTCTime
    , tweetId   :: Integer
    , text      :: Text
    , user      :: User
    } deriving (Show)

instance FromJSON Tweet where
    parseJSON (Object tweet) = Tweet
        <$> twitterTime tweet
        <*> tweet .: "id"
        <*> tweet .: "text"
        <*> tweet .: "user"
      where
        twitterTime :: HashMap Text Value -> Parser UTCTime
        twitterTime obj = case H.lookup "created_at" obj of
            Nothing -> fail "Cannot find created_at"
            Just (String string) -> case (parseTime defaultTimeLocale "%a %b %d %T %z %Y" (unpack string)) of
                Nothing -> fail "Cannot parse"
                Just time -> return time
            Just _ -> fail "Invalid format created_at"
    parseJSON _ = fail "Invalid format"

data User = User
    { userId     :: Integer
    , name       :: Text
    , screenName :: Text
    } deriving (Show)

instance FromJSON User where
    parseJSON (Object o) = User
        <$> o .: "id"
        <*> o .: "name"
        <*> o .: "screen_name"
    parseJSON _ = fail "Invalid format user"
