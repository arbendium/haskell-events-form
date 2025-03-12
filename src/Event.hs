{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields, NamedFieldPuns #-}
{-|
Module      : Event
Description : Data types and utilities representing events
-}
module Event (Event(..), EventType(..), EventDetails(..), EventDetailsOnsiteRecord(..), EventDetailsOnlineRecord(..), Field(..)) where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Time (UTCTime, Day)
import Data.UUID (UUID)
import Data.Aeson ((.=), (.:), (.:?), FromJSON(parseJSON), ToJSON(toEncoding), pairs, withObject)

data EventType = EventTypeOnsite | EventTypeOnline
    deriving (Show, Generic)

instance FromJSON EventType
instance ToJSON EventType

data EventDetailsOnsiteRecord = EventDetailsOnsiteRecord
    { arriveDate :: Day
    , leaveDate :: Day
    , location :: Text
    , locationCoordinates :: Text
    } deriving (Show, Generic)

instance FromJSON EventDetailsOnsiteRecord
instance ToJSON EventDetailsOnsiteRecord

data EventDetailsOnlineRecord = EventDetailsOnlineRecord
    { date :: Day
    , time :: Text
    , callInUrl :: Text
    } deriving (Show, Generic)

instance FromJSON EventDetailsOnlineRecord
instance ToJSON EventDetailsOnlineRecord

data EventDetails = EventDetailsOnsite EventDetailsOnsiteRecord
                  | EventDetailsOnline EventDetailsOnlineRecord
    deriving (Show, Generic)

instance FromJSON EventDetails
instance ToJSON EventDetails

data Field = Field
    { field :: Text
    , value :: Text
    , name :: Text
    , section :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromJSON Field
instance ToJSON Field where
    toEncoding f =
        case section f of
            Nothing -> pairs pairs'
            Just s -> pairs $ pairs' <> "section" .= s
        where
            pairs' =
                "field" .= field f <> "value" .= value f <> "name" .= name f

-- | Main data structure to pass events around
data Event = Event
    {
    -- Metadata
      uuid :: UUID
    , parentEvent :: Maybe Event
    , timestamp :: UTCTime
    , token :: Maybe Text
    , summary :: Text
    , email :: Text -- TODO: related to contactEmail?

    -- Event information
    , eventName :: Text
    , eventUrl :: Text -- optional/empty
    , eventDescription :: Text
    , confirmationDeadline :: Day
    , details :: EventDetails

    -- Attachments
    , attachments :: [UUID]

    -- Dynamic fields
    , fields :: [Field]
    } deriving (Show, Generic)

instance FromJSON Event where
    parseJSON  = withObject "Event" $ \v -> do
        uuid <- v .: "uuid"
        timestamp <- v .: "timestamp"
        token <- v .:? "token"
        summary <- v .: "summary"
        email <- v .: "email"
        eventName <- v .: "eventName"
        eventUrl <- v .: "eventUrl"
        eventDescription <- v .: "eventDescription"
        confirmationDeadline <- v .: "confirmationDeadline"
        location <- v .:? "eventLocation"
        details <- case location of
            Just location -> do
                arriveDate <- v .: "arriveDate"
                leaveDate <- v .: "leaveDate"
                locationCoordinates <- v .: "eventLocationCoordinates"
                return $ EventDetailsOnsite $ EventDetailsOnsiteRecord
                    { arriveDate, leaveDate, location, locationCoordinates }
            Nothing -> do
                date <- v .: "date"
                time <- v .: "time"
                callInUrl <- v .: "callInUrl"
                return $ EventDetailsOnline $ EventDetailsOnlineRecord
                    { date, time, callInUrl }
        attachments <- v .: "attachments"
        fields <- v .: "fields"
        return $ Event
            { uuid, parentEvent = Nothing, timestamp, token, summary, email
            , eventName, eventUrl, eventDescription, confirmationDeadline
            , details, attachments, fields }


instance ToJSON Event where
    toEncoding e =
        pairs $
            "timestamp" .= timestamp e <>
            "uuid" .= uuid e <>
            "summary" .= summary e <>
            "email" .= email e <>
            "eventName" .= eventName e <>
            "eventUrl" .= eventUrl e <>
            "eventDescription" .= eventDescription e <>
            detailsFields (details e) <>
            "confirmationDeadline" .= confirmationDeadline e <>
            "fields" .= fields e <>
            "attachments" .= attachments e <>
            "token" .= token e
        where
            detailsFields (EventDetailsOnsite details) =
                "arriveDate" .= arriveDate details <>
                "leaveDate" .= leaveDate details <>
                "eventLocation" .= location details <>
                "eventLocationCoordinates" .= locationCoordinates details
            detailsFields (EventDetailsOnline details) =
                "date" .= date details <>
                "time" .= time details <>
                "callInUrl" .= callInUrl details
