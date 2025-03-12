{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : EventRoute
Description : Public endpoints for creating and modifying event invitations
-}
module EventRoute (getNewEventForm, postNewEventForm, getExistingEventForm,
                   postExistingEventForm, getNewEventSubmitted,
                   postResendEmailToInviter, getEventAttachment) where

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (readMVar)
import System.Entropy (getEntropy)
import qualified Data.ByteString.Base64 as Base64
import Data.Text (Text, pack, unpack, replace)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.UUID (nil, fromText)
import Data.UUID.V4 (nextRandom)
import Data.Time (UTCTime(..), getCurrentTime, secondsToDiffTime, fromGregorian)
import Data.Aeson (eitherDecodeStrict, encode)
import Network.URI.Encode (encodeTextToBS)
import qualified Network.Wai as Wai
import Network.HTTP.Types.Status (status200, status204, status400, status404)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import AttachmentUtil (attachmentFileName, attachmentNames)
import Configuration as C (Configuration(appDirectory, database, attachments,
                           gmapsKey))
import Event as E (Event(..), EventDetails(..), EventDetailsOnsiteRecord(..))
import EventDatabase (EventCache, saveEvent, getEvent)
import EventPage (eventFormPage, newEventSubmittedPage)
import EventMail (sendEventMailToUser, sendConfirmationToInviter)

-- | Respond with event form for given event
eventForm :: Configuration -> EventCache -> Event -> IO Wai.Response
eventForm configuration eventCache event = do
    let formFile = unpack (C.appDirectory configuration) ++ "/templates/form.md"
    markup <- liftIO $ readFile formFile
    today <- utctDay <$> liftIO getCurrentTime
    eventMap <- liftIO $ readMVar eventCache
    attachmentNames <- liftIO $ attachmentNames configuration
                              $ E.attachments event
    let page = eventFormPage (gmapsKey configuration)
                             (pack markup)       -- full form markdown text
                             today               -- today's date
                             eventMap            -- event map with all events
                             event               -- currently handmed event
                             attachmentNames     -- attachment names
    return $ Wai.responseLBS
        status200
        [("cache-control", "no-store"), ("Content-Type", "text/html")]
        (renderHtml page)


newEvent :: Event
newEvent = Event
    { uuid = nil
    , parentEvent = Nothing
    , timestamp = UTCTime nilDay $ secondsToDiffTime 0
    , token = Nothing
    , summary = ""
    , email = ""
    , eventName = ""
    , eventUrl = ""
    , eventDescription = ""
    , details = EventDetailsOnsite (
        EventDetailsOnsiteRecord
            { arriveDate = nilDay
            , leaveDate = nilDay
            , location = ""
            , locationCoordinates = ""
            }
        )
    , confirmationDeadline = nilDay
    , E.attachments = []
    , fields = [] }
    where nilDay = fromGregorian 0 0 0


getNewEventForm :: Configuration -> EventCache -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
getNewEventForm configuration eventCache respond = eventForm configuration eventCache newEvent >>= respond

getExistingEventForm :: Configuration -> EventCache -> Text -> [(Text, Text)] -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
getExistingEventForm configuration eventCache id query respond =
    case fromText id of
        Nothing -> respond $ Wai.responseLBS status400 [] "Bad event ID"
        Just u -> do
            event <- liftIO $ getEvent eventCache u
            case event of
                Nothing -> noEvent
                Just event ->
                    if lookup "token" query /= token event
                    then noEvent
                    else eventForm configuration eventCache event >>= respond
    where
        noEvent = respond $ Wai.responseLBS status404 [] "Event with given ID was not found"


postNewEventForm :: Configuration -> EventCache -> ByteString -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
postNewEventForm configuration eventCache body respond =
    case eitherDecodeStrict body of
        Left e -> respond $ Wai.responseLBS status400 [] (fromString ("Error: " ++ show e))
        Right event -> do
            newEventUuid <- liftIO nextRandom
            utcTime <- liftIO getCurrentTime
            securityToken <- liftIO $
                encodeToken . decodeUtf8 . Base64.encode <$> getEntropy 24
            let newEvent = event { uuid = newEventUuid
                                 , parentEvent = Nothing
                                 , timestamp = utcTime
                                 , token = Just securityToken
                                 , summary = "" }
            savedEvent <- liftIO $
                saveEvent (unpack $ C.database configuration)
                eventCache newEvent
            _ <- liftIO $ forkIO $ sendEventMailToUser configuration savedEvent
            _ <- liftIO $ forkIO $
                sendConfirmationToInviter configuration savedEvent
            respond $ Wai.responseLBS status200 [("content-type", "application/json")] $ encode savedEvent { parentEvent = Nothing }
    where encodeToken = replace "/" "_" . replace "+" "-"


postExistingEventForm:: Configuration -> EventCache -> Text -> ByteString -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
postExistingEventForm configuration eventCache id body respond =
    case eitherDecodeStrict body of
        Left e -> respond $ Wai.responseLBS status400 [] (fromString ("Error: " ++ show e))
        Right event ->
            case fromText id of
                Nothing -> respond $ Wai.responseLBS status400 [] "Bad event ID"
                Just u -> do
                    oldEvent <- liftIO $ getEvent eventCache u
                    case oldEvent of
                        Nothing -> noEvent
                        Just oldEvent ->
                            if token event /= token oldEvent
                            then noEvent
                            else do
                                utcTime <- liftIO getCurrentTime
                                let newEvent = event
                                        { uuid = u
                                        , parentEvent = Just oldEvent
                                        , timestamp = utcTime
                                        , summary = "" }
                                let databaseFile =
                                        unpack $ C.database configuration
                                savedEvent <- liftIO $
                                    saveEvent databaseFile eventCache newEvent
                                _ <- liftIO $ forkIO $
                                    sendEventMailToUser configuration savedEvent
                                respond $ Wai.responseLBS status200 [("content-type", "application/json")] $ encode savedEvent { parentEvent = Nothing }
    where noEvent = respond $ Wai.responseLBS status404 [] "Event with given ID was not found"


getNewEventSubmitted:: EventCache -> Text -> [(Text, Text)] -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
getNewEventSubmitted eventCache id query respond =
    case fromText id of
        Nothing -> respond $ Wai.responseLBS status400 [] "Bad event ID"
        Just u -> do
            event <- liftIO $ getEvent eventCache u
            case event of
                Nothing -> noEvent
                Just event -> do
                    let page = newEventSubmittedPage event
                    if lookup "token" query /= token event
                    then noEvent
                    else respond $ Wai.responseLBS
                        status200
                        [("cache-control", "no-store"), ("Content-Type", "text/html")]
                        (renderHtml page)
    where noEvent = respond $ Wai.responseLBS status404 [] "Event with given ID was not found"


postResendEmailToInviter :: Configuration -> EventCache -> Text -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
postResendEmailToInviter configuration eventCache id respond =
    case fromText id of
        Nothing -> respond $ Wai.responseLBS status400 [] "Bad event ID"
        Just u -> do
            event <- liftIO $ getEvent eventCache u
            case event of
                Nothing -> noEvent
                Just event -> do
                    _ <- liftIO $ forkIO $
                        sendConfirmationToInviter configuration event
                    respond $ Wai.responseLBS status204 [] ""
    where noEvent = respond $ Wai.responseLBS status404 [] "Event with given ID was not found"


getEventAttachment :: Configuration -> EventCache -> Text -> Text -> [(Text, Text)] -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
getEventAttachment configuration eventCache eventUuid attachmentUuid query respond =
    case (,) <$> fromText eventUuid <*> fromText attachmentUuid of
        Nothing -> respond $ Wai.responseLBS status400 [] ""
        Just (eventUuid, attachmentUuid) -> do
            verified <- liftIO $ verifyEvent eventCache eventUuid
            if verified
            then serveAttachment configuration attachmentUuid
            else noAttachment
    where
        noAttachment = respond $ Wai.responseLBS status404 [] "Attachment not found"
        verifyEvent eventCache uuid =
            maybe False ((==lookup "token" query) . token) <$> getEvent eventCache uuid
        serveAttachment configuration uuid = do
            fileName <- liftIO $ attachmentFileName configuration uuid
            case fileName of
                Nothing -> noAttachment
                Just fileName -> do
                    let path = C.attachments configuration <> "/" <> fileName
                    respond $ Wai.responseFile
                        status200
                        [("content-disposition", "attachment; filename*=utf-8''" <> encodeTextToBS (T.drop 55 fileName))]
                        (unpack path)
                        Nothing
