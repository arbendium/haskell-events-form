{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : AdminRoute
Description : Admin page routes
-}
module AdminRoute (getAdminPage, getEventDetails, getAttachment, postEventResponse) where

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar (readMVar)
import Data.List (find)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.UUID (fromText)
import Data.Aeson (FromJSON(parseJSON), (.:), (.=), withObject, object, eitherDecodeStrict, encode)
import System.Directory (listDirectory)
import Network.URI.Encode (encodeTextToBS)
import Network.Mail.Mime (Address(..))
import qualified  Network.Wai as Wai
import Network.HTTP.Types.Status (status200, status204, status400, status404)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import CMark (commonmarkToHtml)
import AttachmentUtil (attachmentFileName, attachmentNames)
import qualified Configuration as C
import Event (Event(..), Field(field, value))
import EventDatabase (EventCache, getEvent, saveEvent)
import Gmail (createDraft)
import EventMail (readTemplate, renderTemplateMarkdown)
import AdminPage (adminPage, eventDetailsPage)
import Data.Time (UTCTime(utctDay), getCurrentTime)


data ResponseDraft = ResponseDraft
    { subject :: Text
    , content :: Text
    }

instance FromJSON ResponseDraft where
    parseJSON = withObject "response.draft" $ \ o ->
        ResponseDraft <$> o .: "subject" <*> o .: "content"

data Response = Response
    { summary :: Text
    , draft :: Maybe ResponseDraft
    }

instance FromJSON Response where
    parseJSON = withObject "response" $ \ o ->
        Response <$> o .: "summary" <*> o .: "draft"

getAdminPage :: C.Configuration -> EventCache -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
getAdminPage configuration eventCache respond = do
    eventMap <- liftIO $ readMVar eventCache
    today <- utctDay <$> liftIO getCurrentTime
    respond $ Wai.responseLBS
        status200
        [("cache-control", "no-store"), ("Content-Type", "text/html")]
        (renderHtml $ adminPage (C.gmapsKey configuration) today eventMap)

getEventDetails :: C.Configuration -> EventCache -> Text -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
getEventDetails configuration eventCache id respond =
    case fromText id of
        Nothing -> respond $ Wai.responseLBS status400 [] "Bad event ID"
        Just u -> do
            event <- liftIO $ getEvent eventCache u
            case event of
                Nothing ->
                    respond $ Wai.responseLBS status404 [] "Event with given ID was not found"
                Just event -> do
                    attachmentNames <- liftIO $ attachmentNames configuration
                                              $ attachments event
                    templateNames <-
                        liftIO $ responseTemplates configuration event
                    respond $ Wai.responseLBS
                        status200
                        [("cache-control", "no-store"), ("Content-Type", "text/html")]
                        (renderHtml $ eventDetailsPage event attachmentNames templateNames)

getAttachment :: C.Configuration -> Text -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
getAttachment configuration id respond =
    case fromText id of
        Nothing -> respond $ Wai.responseLBS status400 [] "Bad attachment ID"
        Just u -> do
            fileName <- liftIO $ attachmentFileName configuration u
            case fileName of
                Nothing -> respond $ Wai.responseLBS status404 [] "Attachment not found"
                Just fileName -> do
                    let path = C.attachments configuration <> "/" <> fileName
                    respond $ Wai.responseFile
                        status200
                        [("content-disposition", "attachment; filename*=utf-8''" <> encodeTextToBS (T.drop 55 fileName))]
                        (unpack path)
                        Nothing

responseTemplates :: C.Configuration -> Event -> IO [(Text, Text, Text, Text)]
responseTemplates configuration event =
    listDirectory responsesDir >>= mapM load . filterResponses . map pack
    where
        responsesDir = unpack (C.appDirectory configuration) ++ "/templates"
        filterResponses =
            filter (\ x -> T.isPrefixOf "response-" x && T.isSuffixOf ".md" x)
        load fileName = do
            template <- readTemplate configuration $ dropExtension fileName
            let (summary:markup) = T.lines template
            let (subject, content) =
                    renderTemplateMarkdown configuration event $
                        T.unlines markup
            return (responseName fileName, summary, subject, content)
        dropExtension = T.dropEnd 3
        responseName = T.drop 9 . dropExtension

postEventResponse :: C.Configuration -> EventCache -> Text -> ByteString -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
postEventResponse configuration eventCache id body respond =
    case eitherDecodeStrict body of
        Left _ -> respond $ Wai.responseLBS status400 [] ""
        Right response ->
            withEvent id $ \ event -> do
                utcTime <- liftIO getCurrentTime
                let databaseFile = unpack $ C.database configuration
                    newEvent =
                        event
                            { parentEvent = Just event
                            , timestamp = utcTime
                            , Event.summary = AdminRoute.summary response
                            }

                case draft response of
                    Just draft ->  do
                        draftUrl <-
                            liftIO $ createDraft configuration
                                (contactAddress event) (subject draft) $
                                commonmarkToHtml [] (content draft)
                        _ <- liftIO $ saveEvent databaseFile eventCache newEvent
                        respond $ Wai.responseLBS
                            status200
                            [("content-type", "application/json")]
                            (encode $ object [ "draftUrl" .= draftUrl ])
                    Nothing -> do
                        _ <- liftIO $ saveEvent databaseFile eventCache newEvent
                        respond $ Wai.responseLBS status204 [] ""
    where
        withEvent id action =
            case fromText id of
                Nothing -> notFound
                Just uuid -> do
                    event <- liftIO $ getEvent eventCache uuid
                    maybe notFound action event
        notFound = respond $ Wai.responseLBS status404 [] "Event with given ID was not found"
        contactName =
            maybe "" value . find (("contactName"==) . field) . fields
        contactEmail =
            maybe "" value . find (("contactEmail"==) . field) . fields
        maybeText "" = Nothing
        maybeText a = Just a
        contactAddress event =
            case contactEmail event of
                "" -> Address (maybeText $ contactName event) $ email event
                email -> Address (maybeText $ contactName event) email
