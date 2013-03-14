
import System.Directory
import System.FilePath

import Client255.Client255
import Client255.Config

main = do
    basePath <- getAppUserDataDirectory "client255"
    let config = Config (basePath </> "cred")
    mainWithConfig config

mainWithConfig (Config { credPath = credPath }) = do
    putStrLn credPath
    
