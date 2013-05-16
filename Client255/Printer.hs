module Client255.Printer
    ( printTweet
    ) where

import qualified Data.Text.IO as T
import Client255.Type

printTweet :: Tweet -> IO ()
printTweet t = do
    T.putStr $ (screenName . user) t
    putStr $ " [" ++ (show . tweetId) t ++ "] "
    T.putStrLn $ text t
