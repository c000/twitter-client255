{-# LANGUAGE OverloadedStrings #-}

module Client255.Client255
    where

import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Web.Authenticate.OAuth as OAuth
import Network.HTTP.Conduit

import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Attoparsec
import Data.Aeson

import qualified Secrets

oauth :: OAuth.OAuth
oauth = OAuth.newOAuth
    { OAuth.oauthServerName      = "twitter"
    , OAuth.oauthRequestUri      = "https://api.twitter.com/oauth/request_token"
    , OAuth.oauthAccessTokenUri  = "https://api.twitter.com/oauth/access_token"
    , OAuth.oauthAuthorizeUri    = "https://api.twitter.com/oauth/authorize"
    , OAuth.oauthSignatureMethod = OAuth.HMACSHA1
    , OAuth.oauthConsumerKey     = Secrets.consumerKey
    , OAuth.oauthConsumerSecret  = Secrets.consumerSecret
    , OAuth.oauthVersion         = OAuth.OAuth10a
    }

restAPI :: String -> String
restAPI endpoint = "https://api.twitter.com/1.1/" ++ endpoint

getCred :: IO Credential
getCred = do
    tmp <- withManager $ getTemporaryCredential oauth
    hPutStrLn stderr $ "URL: " ++ authorizeUrl oauth tmp
    hPutStr stderr $ "Enter PIN: "
    hFlush stderr
    pin <- BS.getLine
    let tmp' = injectVerifier pin tmp
    cred <- withManager $ getTokenCredential oauth tmp'
    return cred

jsonParser :: Conduit BS.ByteString (ResourceT IO) (PositionRange, Value)
jsonParser = conduitParser json

postData :: Credential -> T.Text -> IO (Response (ResumableSource (ResourceT IO) BS.ByteString))
postData cred postString = withManager $ \manager -> do
    let Just initReq = parseUrl $ restAPI "statuses/update.json"
        postText     = TE.encodeUtf8 postString
        request      = urlEncodedBody [("status", postText)] initReq
    signed <- signOAuth oauth cred request
    http signed manager

getUserStream :: Credential -> Manager -> ResourceT IO (Response (ResumableSource (ResourceT IO) BS.ByteString))
getUserStream cred manager = do
    let Just initReq = parseUrl "https://userstream.twitter.com/1.1/user.json"
    req <- signOAuth oauth cred initReq
    http req manager

getHomeTimeline :: Credential -> IO (BS.ByteString)
getHomeTimeline cred = do
    withManager $ \manager -> do
        let Just initReq = parseUrl $ restAPI "statuses/home_timeline.json?count=200"
        req <- signOAuth oauth cred initReq
        lbs <- httpLbs req manager
        return $ (LBS.toStrict . responseBody) lbs

getFavorites :: Credential -> IO ()
getFavorites cred = do
    withManager $ \manager -> do
        let Just initReq = parseUrl $ restAPI "favorites/list.json?count=200"
        req <- signOAuth oauth cred initReq
        response <- http req manager
        (responseBody response) $$+- CB.sinkFile "/tmp/favorites.json"
