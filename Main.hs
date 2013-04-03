
import System.Directory
import System.FilePath

import Client255.UI
import Client255.Config

defaultConfig :: IO Config
defaultConfig = do
    basePath <- getAppUserDataDirectory "client255"
    return $ Config (basePath </> "cred")

main = do
    config <- defaultConfig
    runClient config
