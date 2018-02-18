module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    folder <- head <$> getArgs
    putStrLn "Deploying site to S3"
    config <- loadConfig
    uploadFolder folder config
    putStrLn "done"
