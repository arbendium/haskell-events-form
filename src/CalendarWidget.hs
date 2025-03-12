{-# LANGUAGE OverloadedStrings #-}

module CalendarWidget (calendar) where

import Control.Monad (when)
import Data.Text (pack)
import qualified Data.Map as M
import Data.Time (Day
                , addDays
                , fromGregorian
                , toGregorian
                , gregorianMonthLength
                , diffDays
                , addGregorianMonthsClip)
import Data.Time.Calendar.WeekDate (toWeekDate, fromWeekDate)
import Data.Time.Format (formatTime, defaultTimeLocale)
import EventDatabase (EventMap)
import Event (Event(..), EventDetails(..), EventDetailsOnsiteRecord(arriveDate, leaveDate), EventDetailsOnlineRecord(date))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5 ((!))
import Data.Maybe (fromMaybe)

-- | Index all events by date
dayMap :: EventMap -> M.Map Day [Event]
dayMap = M.foldl reducer M.empty
    where
        reducer map event =
            if isRelevant event
            then insertEvent map event
            else map
        isRelevant = (/= "ignore") . summary
        insertEvent map event@Event { details = EventDetailsOnsite details } =
            insertOnsiteEvent map event details $ arriveDate details
        insertEvent map event@Event { details = EventDetailsOnline details } =
            M.insertWith (++) (date details) [event] map
        insertOnsiteEvent map event details day =
            if day <= leaveDate details
            then
                M.insertWith (++) day [event] $
                    insertOnsiteEvent map event details $ addDays 1 day
            else map


calendarDay :: M.Map Day [Event] -> Day -> H.Html
calendarDay dayMap day =
    H.div ! A.class_ (H.textValue className) $ H.toMarkup $ show dayOfMonth
    where
        className = "cal-day cal-day-events-" <> pack (show $ min 2 eventCount)
        (_, _, dayOfMonth) = toGregorian day
        eventCount = length events
        events = fromMaybe [] $ M.lookup day dayMap


calendarMonth :: M.Map Day [Event] -> Day -> Integer -> H.Html
calendarMonth dayMap today monthIndex =
    when (hasEvents || monthIndex < 3) $
        H.div ! A.class_ "row card" $ do
            H.div ! A.class_ "col-lg cal-month" $
                H.toMarkup $ formatTime defaultTimeLocale "%B %Y" startOfMonth
            H.div ! A.class_ "col-lg" $ do
                mapM_ dummyCell [1 .. startPadding]
                mapM_ cell [0 .. monthLength - 1]
                mapM_ dummyCell [startPadding + monthLength .. 35]
    where
        startPadding = diffDays startOfMonth startOfWeek
        hasEvents = any (flip M.member dayMap . nthDay) [0 .. monthLength - 1]
        nthDay = flip addDays startOfMonth
        dummyCell _ = H.div ! A.class_ "cal-day" $ mempty
        cell = calendarDay dayMap . nthDay
        (year, month, _) = toGregorian startOfMonth
        monthLength = fromIntegral $ gregorianMonthLength year month
        startOfWeek =
            let (year, week, _) = toWeekDate startOfMonth
            in fromWeekDate year week 1
        startOfMonth = addGregorianMonthsClip monthIndex startOfCurrentMonth
        startOfCurrentMonth =
            let (year, month, _) = toGregorian today
            in fromGregorian year month 0


calendar :: Day -> EventMap -> H.Html
calendar today eventMap =
    H.div ! A.class_ "container" ! A.style "margin-bottom: 2em" $ do
        H.div ! A.class_ "row" $ do
            H.div ! A.class_ "col-md-auto" $ do
                colorSquare "1"
                " one event"
            H.div ! A.class_ "col-md-auto" $ do
                colorSquare "2"
                " two or more events"
        mapM_ (calendarMonth eventsByDay today) [0 .. 9]
    where
        eventsByDay = dayMap eventMap
        colorSquare count =
            H.div ! A.class_ ("cal-legend cal-day cal-day-events-" <> count) $
            mempty
