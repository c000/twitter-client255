module Client255.UI
    ( runClient
    ) where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Attoparsec.ByteString (parse, IResult(..))
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Text (Text)
import qualified Data.Text.IO as T
import Network.HTTP.Conduit
import System.IO
import System.Directory
import System.FilePath
import Web.Authenticate.OAuth (Credential)

import Client255.Client255 as C
import Client255.Config
import qualified Client255.Type as T
import Client255.Printer

runClient :: Config -> IO ()
runClient config = do
    cred <- tryGetCred (credPath config)
    case command config of
        UserStream -> runUserStream cred
        Tweet content -> tweet cred content
        HomeTimeline -> homeTimeline cred

tryGetCred :: FilePath -> IO Credential
tryGetCred path = do
    exist <- doesFileExist path
    case exist of
        True  -> do
            read <$> readFile path
        False -> do
            cred <- getCred
            createDirectoryIfMissing True $ takeDirectory path
            writeFile path (show cred)
            return cred

runUserStream :: Credential -> IO ()
runUserStream cred = withManager $ \manager -> do
    userStream <- getUserStream cred manager
    userStreamPrint $ responseBody userStream
  where
    userStreamPrint stream = do
        (stream', result) <- stream $$++ (conduitParser json) =$ await
        case result of
            Nothing -> return ()
            Just (_, jsonObject) -> do
                case fromJSON jsonObject of
                    Success x -> liftIO $ printTweet (x :: T.Tweet)
                    Error _   -> liftIO $ print jsonObject
                userStreamPrint stream'

homeTimeline :: Credential -> IO ()
homeTimeline cred = do
    timeline <- C.getHomeTimeline cred
    case parse json timeline of
        Done _ ary -> print ary
        someError  -> print someError

tweet :: Credential -> Text -> IO ()
tweet cred content = do
    hPutStr stderr "Do you tweet \""
    T.hPutStr stderr content
    hPutStr stderr "\" (y/n): "
    l <- getLine
    case l of
        'y':_ -> execTweet
        'Y':_ -> execTweet
        _     -> hPutStrLn stderr "Canceled."
  where
    execTweet = do
        postData cred content
        hPutStrLn stderr "Tweet posted."
        return ()
