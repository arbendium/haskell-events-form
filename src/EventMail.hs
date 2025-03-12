{-# LANGUAGE OverloadedStrings, NamedFieldPuns, TupleSections #-}
{-|
Module      : EventMail
Description : Functions for creating sending event-related mails
-}
module EventMail (sendEventMailToUser
                , sendConfirmationToInviter
                , sendEventNotification
                , sendDeadlineNotification
                , readTemplate
                , renderTemplateMarkdown
                , renderTemplateHtml) where

import Control.Monad (when, unless)
import System.Directory (findFile)
import Data.List (find)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import Data.UUID (UUID, toText)
import Network.URI.Encode (encodeText)
import Data.Time (showGregorian)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import CMark (Node(..), NodeType(..), commonmarkToNode, nodeToHtml)
import Network.Mail.Mime (Address(..))
import Util (replaceVariables)
import AttachmentUtil (attachmentFileNames)
import Configuration (Configuration(url, appDirectory, mail), Mail(..))
import qualified Configuration as C (Configuration(attachments))
import Event (Event(..), EventDetails(..), EventDetailsOnsiteRecord(..), EventDetailsOnlineRecord(..), Field(field, name, value))
import Mailer (sendMail, sendNotification)


locationUrl :: EventDetailsOnsiteRecord -> Text
locationUrl EventDetailsOnsiteRecord { location, locationCoordinates = "" } =
    "https://www.google.com/maps/search/?api=1&query=" <> encodeText location
locationUrl EventDetailsOnsiteRecord { locationCoordinates } =
    "https://www.google.com/maps/search/?api=1&query=" <> encodeText locationCoordinates

locationLink :: EventDetailsOnsiteRecord -> H.Html
locationLink details = H.a ! A.href (H.textValue $ locationUrl details)
                         ! A.target "_blank" $ H.toMarkup (location details)

groupBy :: Eq b => (a -> b) -> [a] -> [(b, [a])]
groupBy group = foldl reduce []
    where
        reduce ret e = add e $ break ((==group e) . fst) ret
        add e (left,g:right) = left ++ [(fst g, snd g ++ [e])] ++ right
        add e (left, []) = left ++ [(group e, [e])]


-- | HTML for new event to be sent to application user
newEventHtmlForUser :: Text -> Event -> Text
newEventHtmlForUser baseUrl event =
    TL.toStrict $ renderHtml $
        H.docTypeHtml ! A.lang "en" $ do
            H.head $ H.meta ! A.charset "utf-8"
            H.body $ do
                H.b $ do
                    "New "
                    H.a ! A.href (H.toValue eventDetailsUrl) $
                        "event invitation"
                    " has been submitted"
                H.br
                H.br
                field "Email" email
                field "Event name" eventName
                field "Event URL" eventUrl
                field "Decision deadline" (T.pack . showGregorian . confirmationDeadline)
                onsiteDetailsField "Date of arrival" (T.pack . showGregorian . arriveDate)
                onsiteDetailsField "Date of leave" (T.pack . showGregorian . leaveDate)
                case details event of
                    EventDetailsOnsite details -> row "Location" $ locationLink details
                    _ -> mempty
                onlineDetailsField "Date" (T.pack . showGregorian . date)
                onlineDetailsField "Time" time
                mapM_ customFieldGroup groupedFields
                unless (T.null $ eventDescription event) $ do
                    H.p $ H.b  "Event description:"
                    H.p ! A.style "white-space: pre-wrap" $
                        H.toMarkup $ eventDescription event
    where
        eventDetailsUrl =
            T.concat [baseUrl, "/admin/event/", toText $ uuid event]
        row name value = H.p $ H.b (H.text $ name <> ": ") >> value
        field name what =
            unless (T.null $ what event) $ row name $ H.text $ what event
        onsiteDetailsField = onsiteDetailsField' event
        onsiteDetailsField' Event { details = EventDetailsOnsite details } name what =
            row name $ H.text $ what details
        onsiteDetailsField' _ _ _ = mempty
        onlineDetailsField = onlineDetailsField' event
        onlineDetailsField' Event { details = EventDetailsOnline details } name what =
            row name $ H.text $ what details
        onlineDetailsField' _ _ _ = mempty
        groupedFields =
            groupBy (\ e -> (Event.field e, name e)) $
                filter (not . T.null . value) $ fields event
        customFieldGroup ((_, name), fields) =
            row name $ H.text $ customFieldValue fields
        customFieldValue = T.intercalate ", " . fmap value


-- | HTML for modified event to be sent to application user
modifiedEventHtmlForUser :: Text -> Event -> Event -> Text
modifiedEventHtmlForUser baseUrl oldEvent event =
    TL.toStrict $ renderHtml $
        H.docTypeHtml ! A.lang "en" $ do
            H.head $ H.meta ! A.charset "utf-8"
            H.body $ do
                H.b $ do
                    H.a ! A.href (H.toValue eventDetailsUrl) $
                        "Event invitation"
                    " has been modified"
                H.br
                H.br
                field "Email" email
                field "Event name" eventName
                field "Event URL" eventUrl
                field "Decision deadline" (T.pack . showGregorian . confirmationDeadline)
                onsiteDetailsField "Date of arrival" (T.pack . showGregorian . arriveDate)
                onsiteDetailsField "Date of leave" (T.pack . showGregorian . leaveDate)
                when (onsiteDetailsChanged location || onsiteDetailsChanged locationCoordinates) $
                    row "Location" $ do
                        locationLink' oldEvent
                        " -> "
                        locationLink' event
                onlineDetailsField "Date" (T.pack . showGregorian . date)
                onlineDetailsField "Time" time
                onlineDetailsField "Call-in URL" callInUrl
                mapM_ customFieldGroup groupedFields
                when (changed eventDescription) $ do
                    H.p $ H.b "New event description:"
                    H.p ! A.style "white-space: pre-wrap" $
                        H.toMarkup $ eventDescription event
    where
        eventDetailsUrl =
            T.concat [baseUrl, "/admin/event/", toText $ uuid event]
        changed what = what oldEvent /= what event
        onsiteDetailsChanged = onsiteDetailsChanged' oldEvent event
        onsiteDetailsChanged' Event { details = EventDetailsOnsite oldDetails } Event { details = EventDetailsOnsite details } what =
            what oldDetails /= what details
        onsiteDetailsChanged' Event { details = EventDetailsOnline _ } Event { details = EventDetailsOnline _ } _ = False
        onsiteDetailsChanged' _ _ _ = True
        allFields =
            map (False,) (fields oldEvent) ++ map (True,) (fields event)
        row name value = H.p $ H.b (H.text $ name <> ": ") >> value
        field name what =
            when (changed what) $
                row name $ H.text $ fieldDifference (what oldEvent) (what event)
        onsiteDetailsField = onsiteDetailsField' oldEvent event
        onsiteDetailsField' Event { details = EventDetailsOnsite oldDetails } Event { details = EventDetailsOnsite details } name what =
            when (what oldDetails /= what details) $
                row name $ H.text $ fieldDifference (what oldDetails) (what details)
        onsiteDetailsField' Event { details = EventDetailsOnline _ } Event { details = EventDetailsOnsite details } name what =
            row name $ H.text $ what details
        onsiteDetailsField' _ _ _ _ = mempty
        onlineDetailsField = onlineDetailsField' oldEvent event
        onlineDetailsField' Event { details = EventDetailsOnline oldDetails } Event { details = EventDetailsOnline details } name what =
            when (what oldDetails /= what details) $
                row name $ H.text $ fieldDifference (what oldDetails) (what details)
        onlineDetailsField' Event { details = EventDetailsOnsite _ } Event { details = EventDetailsOnline details } name what =
            row name $ H.text $ what details
        onlineDetailsField' _ _ _ _ = mempty
        locationLink' Event { details = EventDetailsOnsite details } =
            locationLink details
        locationLink' _ = mempty
        groupedFields =
            groupBy ((\ e -> (Event.field e, name e)) . snd) allFields
        customFieldGroup ((_, name), fields) = do
            let oldFields = map snd $ filter (not . fst) fields
            let newFields = map snd $ filter fst fields
            unless (oldFields == newFields) $
                row name $ H.text $
                    fieldDifference (customFieldValue oldFields)
                        (customFieldValue newFields)
        customFieldValue = T.intercalate ", " . fmap value
        fieldDifference "" b = b
        fieldDifference a b = a <> " -> " <> b


-- | Sends new or modified event to application user
sendEventMailToUser :: Configuration -> Event -> IO ()
sendEventMailToUser configuration event = do
    attachments <- EventMail.attachments configuration $ Event.attachments event
    sendNotification configuration (subject event) (content event) attachments
    where
        subject Event { parentEvent = Just _ } =
            "Invite to " <> eventName event <> " modified"
        subject Event { parentEvent = Nothing } =
            "Invite to " <> eventName event <> " received"
        content Event { parentEvent = Just parent } =
            modifiedEventHtmlForUser (url configuration) parent event
        content Event { parentEvent = Nothing } =
            newEventHtmlForUser (url configuration) event


-- | Sends new event confirmation mail to inviter
sendConfirmationToInviter :: Configuration -> Event -> IO ()
sendConfirmationToInviter configuration event = do
    (subject, content) <- render <$> template
    sendMail configuration address subject content []
    where
        address = Address contactName (email event)
        template = readTemplate configuration "confirmation"
        render = renderTemplateHtml configuration event
        contactName = value <$> find (("contactName"==) . field) (fields event)


-- | Sends notification about upcoming event
sendEventNotification :: Configuration -> Event -> IO ()
sendEventNotification configuration event =
    sendNotification configuration subject content []
    where
        subject = "Upcoming event " <> eventName event
        content = TL.toStrict $ renderHtml $
            H.docTypeHtml ! A.lang "en" $ do
                H.head $ H.meta ! A.charset "utf-8"
                H.body $ H.p $ do
                    "Event "
                    H.a ! A.href (H.textValue eventLink) $
                        H.toMarkup $ eventName event
                    " is starting in 3 days."
        eventLink = T.concat [baseUrl, "/admin/event/", toText $ uuid event]
        baseUrl = url configuration


-- | Sends notification about upcoming decision deadline
sendDeadlineNotification :: Configuration -> Event -> IO ()
sendDeadlineNotification configuration event =
    sendNotification configuration subject content []
    where
        subject =
            "Deadline on " <> eventName event <> " is approaching"
        content = TL.toStrict $ renderHtml $
            H.docTypeHtml ! A.lang "en" $ do
                H.head $ H.meta ! A.charset "utf-8"
                H.body $ H.p $ do
                    "A decision on invitation to event "
                    H.a ! A.href (H.textValue eventLink) $
                        H.toMarkup $ eventName event
                    " has te be made within 3 days. "
        eventLink = T.concat [baseUrl, "/admin/event/", toText $ uuid event]
        baseUrl = url configuration


attachments :: Configuration -> [UUID] -> IO [(Text, BL.ByteString)]
attachments configuration uuids = do
    fileNames <- fmap snd <$> attachmentFileNames configuration uuids
    let attachmentNames = map (T.drop 55) fileNames
    contents <- mapM readAttachment fileNames
    return $ zip attachmentNames contents
    where
        attachmentPathPrefix = T.snoc (C.attachments configuration) '/'
        readAttachment = BL.readFile . T.unpack . (attachmentPathPrefix <>)


readTemplate :: Configuration -> Text -> IO Text
readTemplate configuration template = do
    templateFileName <- findFile [responsesDir] (T.unpack template ++ ".md")
    case templateFileName of
        Nothing ->
            fail $
                "Could not find event response template " ++ T.unpack template
        Just fileName -> T.pack <$> readFile fileName
    where responsesDir = T.unpack (appDirectory configuration) ++ "/templates"


-- WARNING: does not escape variable values
renderTemplateMarkdown :: Configuration -> Event -> Text -> (Text, Text)
renderTemplateMarkdown configuration event markup =
    (replace subject, replace $ T.unlines content)
    where
        replace = replaceVariables $ replacePlaceholder configuration event
        (subject:content) = T.lines markup


renderTemplateHtml :: Configuration -> Event -> Text -> (Text, Text)
renderTemplateHtml configuration event markup =
    (replace subject, nodeToHtml [] node)
    where
        replace = replaceVariables $ replacePlaceholder configuration event
        (subject:content) = T.lines markup
        node =
            walk configuration event $ commonmarkToNode [] $ T.unlines content


walk :: Configuration -> Event -> Node -> Node
walk configuration event (Node position node next) =
    Node position (replaceNode node) $ map (walk configuration event) next
    where
        replace = replaceVariables (replacePlaceholder configuration event)
        replaceNode (HTML_BLOCK text) = HTML_BLOCK $ replace text
        replaceNode (CODE_BLOCK info text) = CODE_BLOCK info $ replace text
        replaceNode (TEXT text) = TEXT $ replace text
        replaceNode (HTML_INLINE text) = HTML_INLINE $ replace text
        replaceNode (CODE text) = CODE $ replace text
        -- TODO: URL-sensible replace
        replaceNode (LINK url title) = LINK (replace url) (replace title)
        replaceNode (IMAGE url title) = IMAGE (replace url) (replace title)
        replaceNode node = node


replacePlaceholder :: Configuration -> Event -> Text -> Text
replacePlaceholder configuration event variable =
    case lookup variable replacements of
        Just replacer -> replacer configuration event
        Nothing -> T.intercalate ", " $ fieldValues variable event
    where
        fieldValues field =
            map Event.value . filter ((==field) . Event.field) . fields


replacements :: [(Text, Configuration -> Event -> Text)]
replacements =
    [ ("uuid", eventUuidField)
    , ("editUrl", editUrlField)
    , ("email", eventTextField email)
    , ("eventName", eventTextField eventName)
    , ("eventUrl", eventTextField eventUrl)
    , ("eventLocation", onsiteDetailsField location)
    , ("eventLocationUrl", onsiteDetailsField locationUrl)
    , ("baseUrl", configurationTextField url)
    , ("mailFrom", configurationTextField (mailFrom . mail))
    , ("mailFromName", configurationTextField mailFromName')
    , ("notificationsTo", configurationTextField (mailFrom . mail))
    , ("notificationsToName", configurationTextField notificationsToName') ]
    where
        eventUuidField _ event = toText $ uuid event
        editUrlField configuration event =
            url configuration <> "/" <> toText (uuid event) <>
            "?token=" <> fromJust (token event)
        eventTextField field _ = field
        onsiteDetailsField field _ Event { details = EventDetailsOnsite details } = field details
        onsiteDetailsField _ _ _ = ""
        configurationTextField field configuration _ = field configuration
        mailFromName' = fromMaybe "" . mailFromName . mail
        notificationsToName' = fromMaybe "" . notificationsToName . mail
