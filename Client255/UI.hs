module Client255.UI
    ( runClient
    ) where

import Control.Applicative
import System.Directory
import System.FilePath
import Web.Authenticate.OAuth (Credential)

import Client255.Client255
import Client255.Config

runClient :: Config -> IO ()
runClient config = do
    cred <- tryGetCred (credPath config)
    parseUserStream cred

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
