module Client255.Config
    where

import Data.Text (Text)

data Config = Config
    { credPath :: FilePath
    , command :: Command
    } deriving (Show, Read)

data Command = UserStream
             | Tweet Text
             | HomeTimeline
             deriving (Show, Read)
