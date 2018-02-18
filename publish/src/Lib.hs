{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Lazy           as LBS
import           Data.Configurator
import           Data.Configurator.Types        (convert)
import qualified Data.HashMap.Strict            as H (lookup)
import           Data.Maybe                     (fromMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Network.AWS                    as Aws
import qualified Network.AWS.S3.PutObject       as Aws
import qualified Network.AWS.S3.Types           as Aws
import           System.FilePath

data Config = Config { s3BucketName       :: Text
                     , awsAccessKeyId     :: ByteString
                     , awsSecretAccessKey :: ByteString
                     }

loadConfig = do
    config <- getMap =<< load [Required $ "publish" </> "config" </> "aws.config"]
    let loadEntry x = fromMaybe (error $ T.unpack x) $ convert =<< H.lookup x config
    return $ Config (loadEntry "bucketName")
                    (loadEntry "accessKeyId")
                    (loadEntry "secretAccessKey")


uploadFile :: String -> FilePath -> Config -> IO ()
uploadFile folder filename config = do
    let elementName = Aws.ObjectKey . T.pack $ filename
        accessKeyId = awsAccessKeyId config
        secretAccessKey = awsSecretAccessKey config
        bucket = Aws.BucketName $ s3BucketName config
    body <- Aws.toBody <$> LBS.readFile (folder </> filename)
    env <- liftIO . Aws.newEnv $ Aws.FromKeys (Aws.AccessKey accessKeyId) (Aws.SecretKey secretAccessKey)
    void . runResourceT . Aws.runAWS env . Aws.within Aws.NorthVirginia $
         Aws.send (set Aws.poContentType (Just "text/html") $ set Aws.poACL (Just Aws.OPublicRead) $ Aws.putObject bucket elementName body)


createRedirect :: String -> Config -> IO ()
createRedirect folderName config = do
    let redirect1 = Aws.ObjectKey . T.pack $ folderName
        redirect2 = Aws.ObjectKey . T.pack $ folderName ++ "/"
        accessKeyId = awsAccessKeyId config
        secretAccessKey = awsSecretAccessKey config
        bucket = Aws.BucketName $ s3BucketName config
    env <- liftIO . Aws.newEnv $ Aws.FromKeys (Aws.AccessKey accessKeyId) (Aws.SecretKey secretAccessKey)
    void . runResourceT . Aws.runAWS env . Aws.within Aws.NorthVirginia $ do
         Aws.send (set Aws.poContentType (Just "text/html") $ set Aws.poWebsiteRedirectLocation (Just . T.pack $ "https://www.placesquiet.com/" ++ folderName </> "index.html") $ set Aws.poACL (Just Aws.OPublicRead) $ Aws.putObject bucket redirect1 (Aws.toBody redirectText))
         Aws.send (set Aws.poContentType (Just "text/html") $set Aws.poWebsiteRedirectLocation (Just . T.pack $ "https://www.placesquiet.com/" ++ folderName </> "index.html") $ set Aws.poACL (Just Aws.OPublicRead) $ Aws.putObject bucket redirect2 (Aws.toBody redirectText))


redirectText :: Text
redirectText = "redirect"


uploadFolder :: String -> Config -> IO ()
uploadFolder folderName config = do
    uploadFile folderName "index.html" config
    createRedirect "shops" config
    uploadFile folderName ("shops" </> "index.html") config
    createRedirect "contribute" config
    uploadFile folderName ("contribute" </> "index.html") config
