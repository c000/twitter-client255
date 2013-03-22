{-# LANGUAGE OverloadedStrings #-}

module Client255.Client255
    where

import System.IO (hFlush, stdout)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM

import Web.Authenticate.OAuth as OAuth
import Network.HTTP.Conduit

import Data.Conduit
import Data.Conduit.List (consume)
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
    putStrLn $ "URL: " ++ authorizeUrl oauth tmp
    putStr $ "Enter PIN: "
    hFlush stdout
    pin <- BS.getLine
    let tmp' = injectVerifier pin tmp
    cred <- withManager $ getTokenCredential oauth tmp'
    return cred

jsonParser = conduitParser json

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
        loop (responseBody source)
  where
    loop source = do
        (source', result) <- source $$++ jsonParser =$ await
        case result of
            Just (_, (Object obj)) -> do
                liftIO $ mapM_ (\(k,v) -> T.putStr k >> putStr "\t" >> printJsonValue v) $ HM.toList obj
                loop source'
            Just (_, other)   -> do
                liftIO $ print other
                loop source'
            Nothing           -> do
                return ()

getFavorites :: Credential -> IO ()
getFavorites cred = do
    withManager $ \manager -> do
        initReq <- parseUrl "https://api.twitter.com/1.1/favorites/list.json?count=200"
        req <- signOAuth oauth cred initReq
        response <- http req manager
        (responseBody response) $$+- CB.sinkFile "/tmp/favorites.json"
