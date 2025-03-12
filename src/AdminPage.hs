{-# LANGUAGE OverloadedStrings,NamedFieldPuns #-}
{-|
Module      : AdminPage
Description : HTML for admin page
-}
module AdminPage (adminPage, eventDetailsPage) where

import Control.Monad (unless)
import Data.Maybe (isJust, fromJust, mapMaybe)
import Data.List (sortOn, find)
import Data.Text (Text, pack)
import qualified Data.Text as T (null, isPrefixOf)
import qualified Data.Map as M
import Data.UUID (UUID, toText)
import Data.Time (UTCTime(utctDay), Day)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Network.URI.Encode (encodeText)
import Data.Time.Calendar (showGregorian)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Event (Event(..), EventDetails(..), EventDetailsOnsiteRecord(..), EventDetailsOnlineRecord(..))
import qualified Event as E (Field(..))
import EventDatabase (EventMap)
import CalendarWidget (calendar)


locationUrl :: EventDetailsOnsiteRecord -> Text
locationUrl EventDetailsOnsiteRecord { location, locationCoordinates = "" } =
    "https://www.google.com/maps/search/?api=1&query=" <> encodeText location
locationUrl EventDetailsOnsiteRecord { locationCoordinates } =
    "https://www.google.com/maps/search/?api=1&query=" <> encodeText locationCoordinates

locationLink :: EventDetailsOnsiteRecord -> H.Html
locationLink details = H.a ! A.href (H.textValue $ locationUrl details)
                         ! A.target "_blank" $ H.toMarkup (location details)

locationDataScript :: Day -> EventMap -> H.Html
locationDataScript today eventMap =
    H.script $ H.toMarkup ("var eventLocationData = " ++ show eventList ++ ";initLocationInput(document.getElementById('map'));")
    where
        eventList = mapMaybe eventListElement $ M.elems eventMap
        eventListElement Event { eventName, summary, details = EventDetailsOnsite details } =
            if today <= leaveDate details && summary /= "yes"
            then
                Just
                    [ location details
                    , locationCoordinates details
                    , pack $ showGregorian $ arriveDate details
                    , eventName
                    ]
            else Nothing
        eventListElement _ = Nothing

eventsTableRow :: Event -> H.Html
eventsTableRow event@Event { details = EventDetailsOnsite details } =
    H.div ! A.class_ "flexi-table" $ do
        H.div ! A.class_ "flexi-item-4 flexi-item-title" $ H.toMarkup $ eventName event
        item "Deadline: " $ H.toMarkup $ showGregorian $ confirmationDeadline event
        item "Start time: " $ H.toMarkup startTime
        item "End time: " $ H.toMarkup endTime
        item "Location: " $ locationLink details
        item "Summary: " $ H.toMarkup $ summary event
        item "" $ H.a ! A.href (H.textValue eventDetailsUrl) $ "Details"
    where
        item title content =
            H.div $ do
                H.span ! A.class_ "flexi-field-title"  $ title
                H.span content
        startTime = showGregorian $ arriveDate details
        endTime = showGregorian $ leaveDate details
        eventDetailsUrl = "admin/event/" <> toText (uuid event)
eventsTableRow event@Event { details = EventDetailsOnline details } =
    H.div ! A.class_ "flexi-table" $ do
        H.div ! A.class_ "flexi-item-4 flexi-item-title" $ H.toMarkup $ eventName event
        item "Deadline: " $ H.toMarkup $ showGregorian $ confirmationDeadline event
        item "Date: " $ H.toMarkup $ pack (showGregorian $ date details) <> " " <> time details
        item "" ""
        item "Location: " "Online"
        item "Summary: " $ H.toMarkup $ summary event
        item "" $ H.a ! A.href (H.textValue eventDetailsUrl) $ "Details"
    where
        item title content =
            H.div $ do
                H.span ! A.class_ "flexi-field-title"  $ title
                H.span content
        eventDetailsUrl = "admin/event/" <> toText (uuid event)

eventsTable :: EventMap -> H.Html
eventsTable eventMap =
    H.div $ do
        H.div ! A.class_ "flexi-table flexi-table-head" $ do
            H.div ! A.class_ "flexi-item-4" $ "Event Name"
            H.div "Deadline"
            H.div "Arrival day"
            H.div "Leave day"
            H.div "Location"
            H.div "Response"
            H.div ""
        mapM_ eventsTableRow eventList
    where
        eventList = sortOn confirmationDeadline $ M.elems eventMap

card :: Text -> H.Html -> H.Html
card title content =
    H.div ! A.class_ "card" $ do
        H.div ! A.class_ "card-header" $
            H.h3 ! A.class_ "card-title" $ H.toMarkup title
        H.div ! A.class_ "card-body" $ content


cardEdgeless :: H.Html -> H.Html
cardEdgeless content =
    H.div ! A.class_ "card" $
        H.div ! A.style "padding: 0;" ! A.class_ "card-body" $ content


-- | Whole admin page HTML in Blaze representation
adminPage :: Text -> Day -> EventMap -> H.Html
adminPage gmapsKey today eventMap =
    H.docTypeHtml ! A.lang "en" $ do
        H.head $ do
            H.meta ! A.charset "utf-8"
            H.link ! A.rel "apple-touch-icon" ! A.sizes "76x76" ! A.href "img/apple-icon.png"
            H.link ! A.rel "icon" ! A.type_ "image/png" ! A.href "img/favicon.png"
            H.meta ! A.httpEquiv "X-UA-Compatible" ! A.content "IE=edge"
            H.title "Event invitations"
            H.meta ! A.content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0, shrink-to-fit=no" ! A.name "viewport"
            -- Fonts and icons
            H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "https://fonts.googleapis.com/css?family=Roboto:300,400,500,700%7CRoboto+Slab:400,700%7CMaterial+Icons"
            H.link ! A.rel "stylesheet" ! A.href "css/font-awesome.min.css"
            -- CSS Files
            H.link ! A.href "css/material-dashboard.min.css" ! A.rel "stylesheet"
            H.link ! A.href "css/event-page.css" ! A.rel "stylesheet"
        H.body ! A.class_ "center-column" $ do
            cardEdgeless $ H.div ! A.id "map" ! A.style "height: 30rem; border-radius: 5px; margin-top: 0;" $ mempty
            calendar today eventMap
            card "Pending invitations" $ eventsTable $ filterEvents isPending
            card "Upcoming events" $ eventsTable $ filterEvents isUpcoming
            card "Past events" $ eventsTable $ filterEvents isPast
            card "Rejected invitations" $ eventsTable $ filterEvents isRejected
            -- Google Maps Plugin
            let gmapsScriptUrl = "https://maps.googleapis.com/maps/api/js?key=" <> encodeText gmapsKey
            H.script ! A.src (H.textValue gmapsScriptUrl) $ mempty
            H.script ! A.src "js/location-input.js" $ mempty
            locationDataScript today eventMap
    where
        filterEvents f = M.filter f eventMap
        isPending event =
            summary event `notElem` ["yes", "no", "cancelled", "ignore"] &&
            today <= relevanceDate event
        isUpcoming event = summary event == "yes" && today <= relevanceDate event
        isPast event = summary event == "yes" && today > relevanceDate event
        isRejected event =
            (summary event /= "yes" && today > relevanceDate event) ||
            (summary event `elem` ["no", "cancelled", "ignore"])
        relevanceDate Event { details = EventDetailsOnsite details } = leaveDate details
        relevanceDate Event { details = EventDetailsOnline details } = date details



responseTabs :: [(Text, Text, Text, Text)] -> H.Html
responseTabs responses =
    card "Response" $
        H.div ! A.class_ "card-nav-tabs card-plain" $ do
            H.div ! A.class_ "card-header" $
                H.div ! A.class_ "nav-tabs-navigation" $
                    H.div ! A.class_ "nav-tabs-wrapper" $
                        H.ul ! A.class_ "nav nav-pills nav-pills-primary" !
                            H.dataAttribute "tabs" "tabs" ! A.role "tablist" $
                                do
                                    mapM_ listItem indexedResponses
                                    ignoreButton

            H.div ! A.class_ "card-body" $
                H.div ! A.class_ "tab-content" $
                    mapM_ listContent indexedResponses
    where
        ignoreButton =
            H.li ! A.class_ "nav-item" $
                H.a ! A.class_ "nav-link" ! A.href "javascript:ignoreEvent();"
                    $ "ignore"
        indexedResponses = zip [(0 :: Int)..] responses
        listItem (index, (response, _, _, _)) =
            H.li ! A.class_ "nav-item" $
                H.a ! A.class_ "nav-link" !
                    A.href (H.toValue $ "#response-" ++ show index) !
                    H.dataAttribute "toggle" "tab" $
                        H.toMarkup response
        listContent (index, (_, summary, subject, content)) =
            H.div ! A.class_ "tab-pane" !
                A.id (H.toValue $ "response-" ++ show index) $ do
                    H.div ! A.class_ "form-group" $ do
                        H.input ! A.type_ "text" ! A.class_ "form-control" !
                            A.name "summary" ! A.value (H.toValue summary)
                        H.label ! A.class_ "bmd-label-floating" $ "Summary"
                    H.div ! A.class_ "form-group" $ do
                        H.input ! A.type_ "text" ! A.class_ "form-control" !
                            A.name "subject" ! A.value (H.toValue subject)
                        H.label ! A.class_ "bmd-label-floating" $ "Subject"
                    H.div ! A.class_ "form-group" $ do
                        H.textarea ! A.class_ "form-control" ! A.rows "5" !
                            A.name "content" $ H.toMarkup content
                        H.label ! A.class_ "bmd-label-floating" $ "Content"
                    H.div ! A.class_ "response-save-result" $ mempty
                    H.button ! A.type_ "submit" !
                        A.class_ "btn btn-primary post-draft" !
                        A.style "margin-top: 1.5rem;" $ "Post draft"
                    H.button ! A.type_ "submit" !
                        A.class_ "btn btn-light apply" !
                        A.style "margin-top: 1.5rem; margin-left: 0.8rem" $
                        "Apply"


eventDetailsPage :: Event
                 -> [(UUID, Text)]
                 -> [(Text, Text, Text, Text)]
                 -> H.Html
eventDetailsPage event attachments mailTemplateNames =
    H.docTypeHtml ! A.lang "en" $ do
        H.head $ do
            H.meta ! A.charset "utf-8"
            H.base ! A.href "../../"
            H.link ! A.rel "apple-touch-icon" ! A.sizes "76x76" ! A.href "img/apple-icon.png"
            H.link ! A.rel "icon" ! A.type_ "image/png" ! A.href "img/favicon.png"
            H.meta ! A.httpEquiv "X-UA-Compatible" ! A.content "IE=edge"
            H.title $ H.toMarkup ("Event: " <> eventName event)
            H.meta ! A.content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0, shrink-to-fit=no" ! A.name "viewport"
            -- Fonts and icons
            H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "https://fonts.googleapis.com/css?family=Roboto:300,400,500,700%7CRoboto+Slab:400,700%7CMaterial+Icons"
            H.link ! A.rel "stylesheet" ! A.href "css/font-awesome.min.css"
            -- CSS Files
            H.link ! A.href "css/material-dashboard.min.css" ! A.rel "stylesheet"
            H.link ! A.href "css/event-page.css" ! A.rel "stylesheet"
        H.body ! A.class_ "center-column" $ do
            card "Submitter" $ field "Email:" email
            card "Event information" $ do
                field "Response summary:" summary
                field "Event name:" eventName
                unless (T.null $ eventUrl event) $
                    row "Event URL:" $ linkWrap $ eventUrl event
                field "Invitation received" $ formatDay . utctDay . getTimestamp
                field "Decision deadline:" $ formatDay . confirmationDeadline
                onsiteDetailsField "Date of arrival:" $ H.text . formatDay . arriveDate
                onsiteDetailsField "Date of leave:" $ H.text . formatDay . leaveDate
                case details event of
                    EventDetailsOnsite details ->
                        rowElement $ do
                            colElement $ H.b "Location:"
                            colElement $ locationLink details
                    _ -> mempty
                onlineDetailsField "Date:" $ H.text . formatDay . date
                onlineDetailsField "Time:" $ H.text . time
                onlineDetailsField "Call-in URL:" callInUrlLink
                field "Event Description:" eventDescription
            sections $ fields event
            unless (null attachments) $ card "Attachments" $ do
                let attachmentLink (id, name) = H.p $ H.a ! A.href (H.textValue $ "admin/attachment/" <> toText id) $ H.toMarkup name
                mapM_ attachmentLink attachments
            responseTabs mailTemplateNames
            H.a ! A.class_ "btn btn-secondar" ! A.href "admin"
                ! A.style "color: white; margin: 0 0 1.8em 0; padding-left: 24px; float: right" $ do
                    H.i ! A.class_ "material-icons" $ "arrow_back"
                    "Back"
            H.script ! A.src "js/core/jquery.min.js" $ mempty
            H.script ! A.src "js/core/popper.min.js" $ mempty
            H.script ! A.src "js/plugins/bootstrap-notify.js" $ mempty
            H.script ! A.src "js/core/bootstrap-material-design.min.js" $ mempty
            H.script ! A.src "js/plugins/perfect-scrollbar.min.js" $ mempty
            H.script ! A.src "js/material-dashboard.min.js" $ mempty
            H.script $ H.toMarkup ("window.eventUuid = \"" <> toText (uuid event) <> "\";")
            H.script ! A.src "js/events-form-event-details.js" $ mempty
    where
        getTimestamp Event{ parentEvent = Just p } = getTimestamp p
        getTimestamp event = timestamp event
        formatDay = pack . formatTime defaultTimeLocale "%F (%a)"
        sections fields = case find (isJust . E.section) fields of
                              Just field -> do
                                  let section = fromJust $ E.section field
                                  card section $ fieldsOfSection section fields
                                  sections $ removeSection section fields
                              Nothing -> card "Other Fields" $ mapM_ customField fields
        fieldsOfSection section =
            mapM_ customField . filter ((==Just section) . E.section)
        removeSection section = filter $ (/=Just section) . E.section
        row firstColumn what = H.div ! A.class_ "row" $ do
            colElement $ H.b firstColumn
            wideColElement what
        field firstColumn what = row firstColumn (H.toMarkup $ what event)
        onsiteDetailsField = onsiteDetailsField' event
        onsiteDetailsField' Event { details = EventDetailsOnsite details } name what =
            row name $ what details
        onsiteDetailsField' _ _ _ = mempty
        onlineDetailsField = onlineDetailsField' event
        onlineDetailsField' Event { details = EventDetailsOnline details } name what =
            row name $ what details
        onlineDetailsField' _ _ _ = mempty
        callInUrlLink EventDetailsOnlineRecord { callInUrl = "" } = mempty
        callInUrlLink EventDetailsOnlineRecord { callInUrl } =
            H.a ! A.href (H.toValue callInUrl) $ H.text callInUrl
        customField field = H.div ! A.class_ "row" $ do
            colElement $ H.toMarkup $ E.name field <> ":"
            wideColElement $ linkWrap $ E.value field
        colElement = H.div ! A.class_ "col-md-3"
        wideColElement = H.div ! A.class_ "col-md-9"
                               ! A.style "white-space: pre-wrap"
        rowElement = H.div ! A.class_ "row"
        linkWrap text =
            if T.isPrefixOf "http://" text || T.isPrefixOf "https://" text
            then H.a ! A.href (H.toValue text) $ H.toMarkup text
            else H.toMarkup text
