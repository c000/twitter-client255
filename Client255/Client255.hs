{-# LANGUAGE OverloadedStrings #-}

module Client255.Client255
    where

import System.IO
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Web.Authenticate.OAuth as OAuth
import Network.HTTP.Conduit

import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Attoparsec
import Data.Aeson

import Control.Monad.IO.Class (liftIO)

import qualified Secrets
import Client255.Type

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

jsonParser = conduitParser json

postData :: Credential -> T.Text -> IO (Response (ResumableSource (ResourceT IO) BS.ByteString))
postData cred postString = withManager $ \manager -> do
    initReq <- parseUrl $ restAPI "statuses/update.json"
    let text = TE.encodeUtf8 postString
    let request = urlEncodedBody [("status", text)] initReq
    signed <- signOAuth oauth cred request
    http signed manager

parseUserStream :: Credential -> IO ()
parseUserStream cred = do
    withManager $ \manager -> do
        initReq <- parseUrl "https://userstream.twitter.com/1.1/user.json"
        req <- signOAuth oauth cred initReq
        source <- http req manager
        loop (responseBody source)
  where
    loop source = do
        (source', result) <- source $$++ jsonParser =$ await
        case result of
            Just (_, json)   -> do
                case fromJSON json of
                    Success x -> liftIO $ print (x :: Tweet)
                    Error err -> liftIO $ print json >> putStrLn err
                loop source'
            Nothing           -> do
                return ()

getFavorites :: Credential -> IO ()
getFavorites cred = do
    withManager $ \manager -> do
        initReq <- parseUrl $ restAPI "favorites/list.json?count=200"
        req <- signOAuth oauth cred initReq
        response <- http req manager
        (responseBody response) $$+- CB.sinkFile "/tmp/favorites.json"
