module Client255.UI
    ( runClient
    ) where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.IO
import System.Directory
import System.FilePath
import Web.Authenticate.OAuth (Credential)

import Client255.Client255
import Client255.Config

runClient :: Config -> IO ()
runClient config = do
    cred <- tryGetCred (credPath config)
    case command config of
        UserStream -> parseUserStream cred
        Tweet content -> tweet cred content

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
