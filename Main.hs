
import System.Console.GetOpt
import System.Environment
import System.Directory
import System.FilePath
import System.IO (hSetEcho, stdout)

import Data.Text (pack)

import Client255.UI
import Client255.Config

defaultConfig :: IO Config
defaultConfig = do
    basePath <- getAppUserDataDirectory "client255"
    return $ Config (basePath </> "cred") UserStream

parseArgs :: [String] -> ([Config -> Config], [String], [String])
parseArgs = getOpt RequireOrder options

options :: [OptDescr (Config -> Config)]
options =
    [ Option ['c'] ["credential"] (ReqArg (\path opts -> opts {credPath = path}) "PATH") "Credential path"
    , Option ['u'] ["update"] (ReqArg (\string opts -> opts {command = Tweet (pack string)}) "CONTENT") "Tweet to twitter"
    , Option ['h'] ["hometimeline"] (NoArg (\opts -> opts {command = HomeTimeline})) "Get HomeTimeline"
    ]

main :: IO ()
main = do
    hSetEcho stdout False
    config <- defaultConfig
    args <- getArgs
    case parseArgs args of
        (o,[],[]) -> runClient (foldl (flip id) config o)
        (_,_,err) -> ioError (userError (concat err ++ usageInfo "" options))
