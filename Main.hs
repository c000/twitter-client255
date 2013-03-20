
import System.Directory
import System.FilePath

import Client255.UI
import Client255.Config

main = do
    basePath <- getAppUserDataDirectory "client255"
    let config = Config (basePath </> "cred")
    runClient config
