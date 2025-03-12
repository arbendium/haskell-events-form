{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Application
Description : WAI application

WAI application with middlewares, routes, asset server etc.
Any generic request handlers (e.g. logging) should be here.
-}

module Application (application) where

import Control.Arrow (second)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types (queryToQueryText)
import Network.Wai (Application, queryString, pathInfo, requestMethod, strictRequestBody)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import qualified Configuration as C
import EventDatabase (EventCache)
import AdminRoute (getAdminPage, getAttachment, getEventDetails, postEventResponse)
import AttachmentRoute (postAttachment)
import EventRoute (getNewEventForm, postNewEventForm, getExistingEventForm, postExistingEventForm, getNewEventSubmitted, postResendEmailToInviter, getEventAttachment)

dispatch :: C.Configuration -> EventCache -> ByteString -> [Text] -> Application
dispatch configuration eventCache "GET" ["admin"] _ =
  getAdminPage configuration eventCache
dispatch configuration eventCache "GET" ["admin", "event", eventId] _ =
  getEventDetails configuration eventCache eventId
dispatch configuration eventCache "POST" ["admin", "event", eventId, "response"] req =
  \ respond -> do
    body <- strictRequestBody req
    postEventResponse configuration eventCache eventId (BL.toStrict body) respond
dispatch configuration _ "GET" ["admin", "attachment", attachmentId] _ =
  getAttachment configuration attachmentId

dispatch configuration _ "POST" ["attachment"] req =
  postAttachment configuration req

dispatch configuration eventCache "GET" []  _ =
  getNewEventForm configuration eventCache
dispatch configuration eventCache "POST" [] req =
  \ respond -> do
    body <- strictRequestBody req
    postNewEventForm configuration eventCache (BL.toStrict body) respond
dispatch configuration eventCache "GET" [eventId] req =
  getExistingEventForm configuration eventCache eventId (map (second (fromMaybe "")) $ queryToQueryText $ queryString req)
dispatch configuration eventCache "POST" [eventId] req =
  \ respond -> do
    body <- strictRequestBody req
    postExistingEventForm configuration eventCache eventId (BL.toStrict body) respond
dispatch _ eventCache "GET" [eventId, "submitted"] req =
  getNewEventSubmitted eventCache eventId (map (second (fromMaybe "")) $ queryToQueryText $ queryString req)
dispatch configuration eventCache "GET" [eventId, "reset-email"] _ =
  postResendEmailToInviter configuration eventCache eventId
dispatch configuration eventCache "GET" [eventId, "attachment", attachmentId] req =
  getEventAttachment configuration eventCache eventId attachmentId (map (second (fromMaybe "")) $ queryToQueryText $ queryString req)

dispatch configuration _ _ _ req = staticApp (defaultWebAppSettings staticRoot) req
  where staticRoot = T.unpack (C.appDirectory configuration) ++ "/www"

-- | Main request handler (WAI application)
application :: C.Configuration -> EventCache -> Application
-- Note: it doesn't handle "", ".", ".." path components in any special way
application configuration eventCache = logStdout $ \ req -> dispatch configuration eventCache (requestMethod req) (pathInfo req) req
