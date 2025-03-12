module AttachmentUtil where

import Control.Exception (catch, throw)
import System.IO.Error (isDoesNotExistError)
import System.Directory (listDirectory)
import Data.Maybe (mapMaybe)
import Data.List (find)
import Data.Text (Text, unpack, pack)
import qualified Data.Text as T
import Data.UUID (UUID, toString)
import Configuration (Configuration(attachments))


attachmentFileName :: Configuration -> UUID -> IO (Maybe Text)
attachmentFileName configuration uuid = do
    fileNames <-
        listDirectory (unpack $ attachments configuration)
        `catch` handleNotFound
    return $ pack <$> find fileNameMatches fileNames
    where
        fileNameMatches = (" " ++ toString uuid ++ " " ==) . take 38 . drop 17
        handleNotFound e =
            if isDoesNotExistError e
            then return []
            else throw e


attachmentFileNames :: Configuration -> [UUID] -> IO [(UUID, Text)]
attachmentFileNames configuration uuids = do
    fileNames <-
        listDirectory (unpack $ attachments configuration)
        `catch` handleNotFound
    let attachmentFileNames = map (fileName fileNames) uuids
    return $ mapMaybe attachmentFilter $ zip uuids attachmentFileNames
    where
        fileNameMatches uuid =
            (" " ++ toString uuid ++ " " ==) . take 38 . drop 17
        fileName fileNames uuid = find (fileNameMatches uuid) fileNames
        attachmentFilter (a, Just b) = Just (a, pack b)
        attachmentFilter (_, Nothing) = Nothing
        handleNotFound e =
            if isDoesNotExistError e
            then return []
            else throw e


attachmentNames :: Configuration -> [UUID] -> IO [(UUID, Text)]
attachmentNames configuration uuids =
    fmap name <$> attachmentFileNames configuration uuids
    where name (a, b) = (a, T.drop 55 b)