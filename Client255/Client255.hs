{-# LANGUAGE OverloadedStrings #-}

module Client255.Client255
    where

import System.IO (stdout)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM

import Web.Authenticate.OAuth as OAuth
import Network.HTTP.Conduit

import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Attoparsec
import Data.Aeson (json, Value (..))

import Control.Monad.IO.Class (liftIO)

import qualified Secrets

oauth :: OAuth.OAuth
oauth = OAuth.newOAuth
    { OAuth.oauthServerName = "twitter"
    , OAuth.oauthRequestUri = "http://api.twitter.com/oauth/request_token"
    , OAuth.oauthAccessTokenUri = "http://api.twitter.com/oauth/access_token"
    , OAuth.oauthAuthorizeUri = "http://api.twitter.com/oauth/authorize"
    , OAuth.oauthSignatureMethod = OAuth.HMACSHA1
    , OAuth.oauthConsumerKey = Secrets.consumerKey
    , OAuth.oauthConsumerSecret = Secrets.consumerSecret
    , OAuth.oauthVersion = OAuth.OAuth10a
    }

getCred :: IO Credential
getCred = do
    tmp <- withManager $ getTemporaryCredential oauth
    putStrLn $ authorizeUrl oauth tmp
    pin <- BS.getLine
    let tmp' = injectVerifier pin tmp
    cred <- withManager $ getTokenCredential oauth tmp'
    return cred

jsonParser = sinkParser json

printJsonValue v = case v of
    String text -> T.putStrLn text
    other       -> print other

postData :: IO (Request IO)
postData = do
    initReq <- parseUrl "https://api.twitter.com/1.1/statuses/update.json"
    let text = TE.encodeUtf8 ""
    return $ urlEncodedBody [("status", text)] initReq

parseUserStream :: Credential -> IO ()
parseUserStream cred = do
    withManager $ \manager -> do
        initReq <- parseUrl "https://userstream.twitter.com/1.1/user.json"
        req <- signOAuth oauth cred initReq
        source <- http req manager
        loop (responseBody source) jsonParser
  where
    loop source sink = do
        (nextSource, result) <- source $$++ sink
        case result of
            Object obj -> liftIO $ mapM_ (\(k,v) -> T.putStr k >> putStr "\t" >> printJsonValue v) $ HM.toList obj
            other      -> liftIO $ print other
        loop nextSource sink

getFavorites :: Credential -> IO ()
getFavorites cred = do
    withManager $ \manager -> do
        initReq <- parseUrl "https://api.twitter.com/1.1/favorites/list.json?count=200"
        req <- signOAuth oauth cred initReq
        response <- http req manager
        (responseBody response) $$+- CB.sinkFile "/tmp/favorites.json"
