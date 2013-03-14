module Client255.Config
    where

import System.FilePath

data Config = Config
    { credPath :: FilePath
    } deriving (Show, Read)
