{-# LANGUAGE OverloadedStrings #-}

module Notifier (runNotifier) where

import Data.Map (elems)
import Data.Text (unpack)
import Data.UUID (toText)
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.MVar (readMVar)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Data.Time (UTCTime(utctDay), Day, getCurrentTime, addDays)
import Configuration (Configuration(notifications))
import Event (Event(..), EventDetails(..), EventDetailsOnsiteRecord(..), EventDetailsOnlineRecord(..))
import EventDatabase (EventCache)
import EventMail (sendEventNotification, sendDeadlineNotification)


-- | Runs notifier in a new thread
runNotifier :: Configuration -> EventCache -> IO ThreadId
runNotifier configuration eventCache = forkIO $ notifier configuration eventCache


notifier :: Configuration -> EventCache -> IO ()
notifier configuration eventCache = do
    threadDelay $ seconds 10
    notify configuration eventCache
    threadDelay $ seconds 300
    notifier configuration eventCache
    where seconds = (*1000000)


notify :: Configuration -> EventCache -> IO ()
notify configuration eventCache = do
    today <- utctDay <$> getCurrentTime
    (eventNotifications, deadlineNotifications) <- neededNotifications today eventCache
    mapM_ (notifyEvent configuration) eventNotifications
    mapM_ (notifyDeadline configuration) deadlineNotifications


neededNotifications :: Day -> EventCache -> IO ([Event], [Event])
neededNotifications today eventCache = do
    eventMap <- readMVar eventCache
    return (filter eventApproaching $ elems eventMap, filter deadLineApproaching $ elems eventMap)
    where
        isConfirmed = (`elem` ["yes", "no", "cancelled", "ignore"]) . summary
        isAccepted = (`notElem` ["no", "cancelled", "ignore"]) . summary
        dateApproaching date = (addDays 3 today >= date) && (date >= today)
        eventApproaching event@Event { details = EventDetailsOnsite details } =
            isAccepted event && dateApproaching (arriveDate details)
        eventApproaching event@Event { details = EventDetailsOnline details } =
            isAccepted event && dateApproaching (date details)
        deadLineApproaching event = not (isConfirmed event) &&
                                    dateApproaching (confirmationDeadline event)


notifyEvent :: Configuration -> Event -> IO ThreadId
notifyEvent configuration event = forkIO $ do
    notificationExists <- doesFileExist $ unpack notificationFlag
    if not notificationExists then do
        putStrLn ("Notifying about upcoming event " ++ show (uuid event))
        sendEventNotification configuration event
        createDirectoryIfMissing True $ unpack $ notifications configuration
        writeFile (unpack notificationFlag) ""
    else mempty
    where notificationFlag = notifications configuration <> "/event-" <> toText (uuid event)


notifyDeadline :: Configuration -> Event -> IO ThreadId
notifyDeadline configuration event = forkIO $ do
    notificationExists <- doesFileExist $ unpack notificationFlag
    if not notificationExists then do
        putStrLn ("Notifying about decision deadline of event " ++ show (uuid event))
        sendDeadlineNotification configuration event
        createDirectoryIfMissing True $ unpack $ notifications configuration
        writeFile (unpack notificationFlag) ""
    else mempty
    where notificationFlag = notifications configuration <> "/deadline-" <> toText (uuid event)
