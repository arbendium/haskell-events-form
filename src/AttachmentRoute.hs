{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : AttachmentRoute
Description : Public endpoints for uploading attachments
-}

module AttachmentRoute (postAttachment) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import System.Directory (createDirectoryIfMissing)
import Data.UUID (UUID, toText)
import Data.UUID.V4 (nextRandom)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Aeson (encode)
import Network.HTTP.Types.Status (status200)
import qualified Network.Wai as Wai
import Network.Wai.Parse (FileInfo(..), parseRequestBody, lbsBackEnd)
import Configuration (Configuration(attachments))


postAttachment :: Configuration -> Wai.Application
postAttachment configuration request respond = do
    (_, files) <- parseRequestBody lbsBackEnd request
    uuids <- liftIO $ mapM (saveFile (attachments configuration) . snd) files
    respond $ Wai.responseLBS
        status200
        [("content-type", "application/json")]
        (encode uuids)


saveFile :: Text -> FileInfo BL.ByteString -> IO UUID
saveFile prefix fileInfo = do
    time <- getCurrentTime
    uuid <- nextRandom
    let name = fileNamePrefix time uuid <> " " <> decodeUtf8 (fileName fileInfo)
    createDirectoryIfMissing True $ unpack prefix
    BL.writeFile (unpack $ prefix <> "/" <> name) $ fileContent fileInfo
    return uuid
    where
        formattedTime = pack . formatTime defaultTimeLocale "%Y-%m-%d %H%M%S"
        fileNamePrefix time uuid = formattedTime time <> " " <> toText uuid
