{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : EventDatabase
Description : File-based append-only JSON database for events

Events are stored in single append-only database - one event
per line in JSON format. Invalid lines (either not JSON or
invalid structure) are ignored with error printed to standard
error.

Events are cached into `Data.Map.Map UUID Event` structure.
Events happened more than 30 days in the past are not stored
in cache. Any updates are first written to file while cache is
locked and then into cache. Database file should only be read
once and then stored into `Control.Concurrent.MVar`.

Datbase file will be opened and closed for each read/write
operation. That shouldn't be a probleb due to low expected
traffic.
-}
module EventDatabase (EventMap
                    , EventCache
                    , newEventCache
                    , readEvents
                    , saveEvent
                    , getEvent) where

import System.IO (hPutStrLn, stderr, Handle, withFile, IOMode(ReadMode), hIsEOF)
import System.IO.Error (isDoesNotExistError)
import Control.Exception (catch, throw)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, modifyMVar_, readMVar)
import Data.ByteString (hGetLine)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Data.Time (Day, utctDay, utctDayTime, getCurrentTime, addDays,
                  secondsToDiffTime, diffTimeToPicoseconds)
import Data.Aeson (encode, eitherDecodeStrict)
import Data.UUID (UUID)
import Event (Event(..), EventDetails(..), EventDetailsOnsiteRecord(..), EventDetailsOnlineRecord(..))


-- | Map from event UUID to event
type EventMap = M.Map UUID Event


-- | MVar containing EventMap.
type EventCache = MVar EventMap


-- | Creates new event cache
newEventCache :: IO EventCache
newEventCache = newMVar M.empty


-- | Reads events from database file into given cache.
readEvents :: FilePath -> EventCache -> IO ()
readEvents databaseFilePath cache = do
    today <- utctDay <$> getCurrentTime
    modifyMVar_ cache $ (`catch` handleNotFound) . loadEvents today
    where
        loadEvents today _ =
            withFile databaseFilePath ReadMode $
                \ handle -> do
                    allEvents <- readEvent today handle 0 M.empty
                    let filteredEvents = M.filter (isRelevant today) allEvents
                    putStrLn $ "Loaded " ++
                        show (M.size allEvents) ++ " events, " ++
                        show (M.size filteredEvents) ++ " relevant"
                    return filteredEvents
        isRelevant today event =
            (summary event`notElem` ["yes", "no", "cancelled", "ignore"]) ||
            addDays (-30) today <= relevanceDeadline (details event)
        relevanceDeadline (EventDetailsOnsite details) = leaveDate details
        relevanceDeadline (EventDetailsOnline details) = date details
        handleNotFound e =
            if isDoesNotExistError e
            then return M.empty
            else throw e

-- Recursively reads remaining lines (events) from database file handle
readEvent :: Day -> Handle -> Int -> EventMap -> IO EventMap
readEvent today databaseFile lineNumber eventMap = do
    isEof <- hIsEOF databaseFile
    if isEof
    then return eventMap
    else do
        line <- hGetLine databaseFile
        let event = eitherDecodeStrict line :: Either String Event
        case event of
            Right event -> do
                let eventWithParent =
                       event { parentEvent = M.lookup (uuid event) eventMap }
                readNextEvent $ M.insert (uuid event) eventWithParent eventMap
            Left error -> do
                hPutStrLn stderr $ "Failed to read event from database on line "
                    ++ show (lineNumber + 1) ++ ": " ++ error
                readNextEvent eventMap
    where
        readNextEvent = readEvent today databaseFile $ lineNumber + 1


-- | Saves event into database file and cache
saveEvent :: FilePath -> EventCache -> Event -> IO Event
saveEvent databaseFilePath cache event =
    modifyMVar cache $ \ eventMap -> do
        let roundedTime =
                secondsToDiffTime $
                diffTimeToPicoseconds (utctDayTime $ timestamp event) `quot`
                1000000000000
            newEvent =
                event
                { timestamp = (timestamp event) { utctDayTime = roundedTime }
                , parentEvent = M.lookup (uuid event) eventMap
                }
        BL.appendFile databaseFilePath $ encode newEvent <> "\n"
        return (M.insert (uuid event) newEvent eventMap, newEvent)


-- | Get single event from event cache
getEvent :: EventCache -> UUID -> IO (Maybe Event)
getEvent cache uuid = do
    eventMap <- readMVar cache
    return $ M.lookup uuid eventMap
