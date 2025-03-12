{-# LANGUAGE OverloadedStrings,NamedFieldPuns #-}
{-|
Module      : EventPage
Description : Functions to generate HTML for event form
-}
module EventPage (eventFormPage, newEventSubmittedPage) where

import Control.Monad (unless)
import Data.Maybe (fromMaybe, isJust, fromJust, mapMaybe)
import Data.List (find)
import Data.Text (Text, pack, breakOn, toLower)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import qualified Data.Map.Strict as M (Map, elems, empty, insert, lookup)
import Data.UUID (UUID, toText, nil)
import Data.Time (Day, fromGregorian, showGregorian)
import CMark (Node(..), optUnsafe, NodeType(DOCUMENT, CODE_BLOCK, HTML_BLOCK),
              commonmarkToNode, nodeToHtml)
import Text.Blaze.Html5 ((!), (!?))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Network.URI.Encode (encodeText)
import Event (Event(..), EventDetails(..), EventDetailsOnsiteRecord(..), EventDetailsOnlineRecord(..))
import qualified Event as E (Field(..))
import EventDatabase (EventMap)
import CalendarWidget (calendar)

-- data type of key-value pairs describing single input field in Markdown
type Block = M.Map Text Text


-- input field parameter lookup functions
fieldArgField :: Block -> Text
fieldArgField = fromJust . M.lookup "field"

fieldArgType :: Block -> Text
fieldArgType block =
    if M.lookup "type" block == Just "email" then "email" else "text"

fieldArgName :: Block -> Text
fieldArgName = fromMaybe "" . M.lookup "name"

fieldArgTooltip :: Block -> Text
fieldArgTooltip = fromMaybe "" . M.lookup "tooltip"

fieldArgLabel :: Block -> Maybe Text
fieldArgLabel = M.lookup "label"

fieldArgSection :: Block -> Maybe Text
fieldArgSection = M.lookup "section"

fieldArgRequired :: Block -> Bool
fieldArgRequired = isJust . M.lookup "required"

fieldArgError :: Block -> Maybe Text
fieldArgError = M.lookup "error"

fieldArgRepeated :: Block -> Maybe Bool
fieldArgRepeated = fmap parseBool . M.lookup "repeated"
    where parseBool = (`elem` ["true", "yes", "1"]) . toLower

-- | Form input container
formGroup :: Block -> H.Html -> H.Html
formGroup block input = H.div ! A.class_ "form-group" $ do
    unless (T.null tooltip) $
        H.button ! A.type_ "button"
                 ! A.class_ "btn btn-secondary input-help"
                 ! H.dataAttribute "toggle" "tooltip"
                 ! A.title (H.textValue tooltip) $
            H.i ! A.class_ "material-icons" $ "help"
    input
    unless (T.null label) $
        H.label ! A.class_ "bmd-label-floating" $ H.toMarkup label
    where
        label = fromMaybe (fieldArgName block) (fieldArgLabel block)
        tooltip = fieldArgTooltip block

textInput :: Block -> Text -> H.Html
textInput block value =
    formGroup block $ H.input
        ! A.type_ (H.textValue $ fieldArgType block)
        ! A.class_ "form-control"
        ! H.dataAttribute "name" (H.textValue name)
        !? (isJust section, sectionAttr)
        !? (required, A.required "")
        !? (isJust errorAttr, fromJust errorAttr)
        ! A.name (H.textValue $ fieldArgField block)
        ! A.value (H.textValue value)
    where
        sectionAttr = H.dataAttribute "section" $ H.textValue $ fromJust section
        name = fieldArgName block
        section = fieldArgSection block
        required = fieldArgRequired block
        errorAttr =
            H.dataAttribute "error" . H.textValue <$> fieldArgError block

textareaInput :: Block -> Text -> H.Html
textareaInput block value =
    formGroup block $ H.textarea ! A.class_ "form-control"
                                 ! H.dataAttribute "name" (H.textValue name)
                                 !? (isJust section, sectionAttr)
                                 ! A.name (H.textValue $ fieldArgField block)
                                 $ H.toMarkup value
    where
        name = fieldArgName block
        section = fieldArgSection block
        sectionAttr = H.dataAttribute "section" $ H.textValue $ fromJust section

dateInput :: Block -> Day -> H.Html
dateInput block value =
    formGroup block $ H.input
        ! A.type_ "text"
        ! A.class_ "form-control datetimepicker"
        ! A.placeholder ""
        !? (required, A.required "")
        !? (isJust errorAttr, fromJust errorAttr)
        ! A.name (H.textValue field)
        ! A.value value'
    where
        field = fieldArgField block
        required = fieldArgRequired block
        errorAttr =
            H.dataAttribute "error" . H.textValue <$> fieldArgError block
        value' = if value == fromGregorian 0 0 0
                 then ""
                 else H.stringValue $ showGregorian value

locationInput :: Block -> Event -> H.Html
locationInput block event = do
    formGroup block $ do
        H.input ! A.type_ "text"
                ! A.class_ "form-control"
                ! A.placeholder ""
                !? (required, A.required "")
                !? (isJust errorAttr, fromJust errorAttr)
                ! A.name "eventLocation"
                ! A.value (H.textValue $ location' event)
        H.input ! A.type_ "hidden"
                ! A.name "eventLocationCoordinates"
                ! A.value (H.textValue $ locationCoordinates' event)
    H.div ! A.id "map" ! A.style "height: 30em;" $ mempty
    H.script "initLocationInput(document.getElementById('map'), document.querySelector('input[name=eventLocation]'), document.querySelector('input[name=eventLocationCoordinates]'))"
    where
        location' Event { details = EventDetailsOnsite details } = location details
        location' _ = ""
        locationCoordinates' Event { details = EventDetailsOnsite details } = locationCoordinates details
        locationCoordinates' _ = ""
        required = fieldArgRequired block
        errorAttr =
            H.dataAttribute "error" . H.textValue <$> fieldArgError block

locationDataScript :: Event -> Day -> EventMap -> H.Html
locationDataScript event today eventMap =
    H.script $
        H.toMarkup $ "var eventLocationData = " ++ show eventData ++
            ", currentEventLocationData = " ++ show currentEventData ++ ";"
    where
        eventData = mapMaybe eventListElement $ M.elems eventMap
        currentEventData = currentEventData' event
        currentEventData' Event { eventName, details = EventDetailsOnsite details } =
            [ location details
            , locationCoordinates details
            , pack $ showGregorian $ arriveDate details
            , eventName
            ]
        currentEventData' _ = ["", "", "0000-00-00", ""]
        eventListElement Event { uuid = u, summary, details = EventDetailsOnsite details } =
            if u /= uuid event && today <= leaveDate details && notElem summary ["no", "cancelled", "ignore"]
            then Just [locationCoordinates details]
            else Nothing
        eventListElement _ = Nothing

attachmentsInput :: UUID -> Maybe Text -> [(UUID, Text)] -> H.Html
attachmentsInput eventUuid token attachmentNames = do
    mapM_ (attachmentListElement eventUuid token) attachmentNames
    H.div $ do
        H.input ! A.type_ "file"
                ! A.class_ "form-control inputfile"
                ! A.multiple "multiple"
                ! A.name "attachments"
                ! A.style "height: auto;"
                ! A.id "attachments-input"
        H.label ! A.for "attachments-input" $
            H.span ! A.class_ "btn btn-info" $ "Add attachments"
        H.p ""
    H.br
    H.script "const fileAttachments = []; initFileInput(document.querySelector('#attachments-input'), fileAttachments);"

attachmentListElement :: UUID -> Maybe Text -> (UUID, Text) -> H.Html
attachmentListElement eventUuid token (attachmentUuid, attachmentName) =
    H.div $ do
        H.input ! A.type_ "hidden" ! A.name "attachments"
                ! A.value (H.textValue $ toText attachmentUuid)
        let tokenParam = maybe "" (("?token="<>) . encodeText) token
        let url = toText eventUuid <>
                "/attachment/" <>
                toText attachmentUuid <>
                tokenParam
        H.p ! A.class_ "file-item" $ do
            H.a ! A.href (H.textValue url) $ H.toMarkup attachmentName
            H.a ! A.class_ "remove" ! A.style "margin-left: 1em" ! A.href "javascript:void(0)"
                ! A.onclick "this.parentNode.parentNode.parentNode.removeChild(this.parentNode.parentNode);enableSave();"
                $ "Remove"


-- | Parses given block into key-value pairs - one pair per line.
-- If line does not contain space (' '), it is treated as key with empty value.
-- This also applies to empty lines, in which case key is also empty
parseBlock :: Text -> Block
parseBlock block = parseLine M.empty $ T.lines block
    where
        parseLine map [] = map
        parseLine map (line:lines) =
            let (key, value) = breakOn " " line
            in M.insert key (T.drop 1 value) (parseLine map lines)


forceRequired :: Block -> Block
forceRequired = M.insert "required" ""


field :: Text             -- field id
      -> Block            -- field description
      -> Event            -- event
      -> [(UUID, Text)]   -- attachment file names
      -> H.Html

field "email" block event _ =
    textInput (forceRequired $ M.insert "type" "email" block) $ email event

field "eventName" block event _ =
    textInput (forceRequired block) $ eventName event

field "eventUrl" block event _ =
    textInput block $ eventUrl event

field "eventDescription" block event _ =
    textareaInput block $ eventDescription event

field "arriveDate" block event _ =
    dateInput (forceRequired block) $ arriveDate' event
    where
        arriveDate' Event { details = EventDetailsOnsite details } =
            arriveDate details
        arriveDate' _ = fromGregorian 0 0 0

field "leaveDate" block event _ =
    dateInput (forceRequired block) $ leaveDate' event
    where
        leaveDate' Event { details = EventDetailsOnsite details } =
            leaveDate details
        leaveDate' _ = fromGregorian 0 0 0

field "eventLocation" block event _ =
    locationInput (forceRequired block) event

field "date" block event _ =
    dateInput (forceRequired block) $ date' event
    where
        date' Event { details = EventDetailsOnline details } = date details
        date' _ = fromGregorian 0 0 0

field "time" block event _ =
    textInput block $ time' event
    where
        time' Event { details = EventDetailsOnline details } = time details
        time' _ = ""

field "callInUrl" block event _ =
    textInput block $ callInUrl' event
    where
        callInUrl' Event { details = EventDetailsOnline details } =
            callInUrl details
        callInUrl' _ = ""

field "confirmationDeadline" block event _ =
    dateInput (forceRequired block) $ confirmationDeadline event

field "attachments" _ event attachmentNames =
    attachmentsInput (uuid event) (token event) attachmentNames

field _ block event _ =
    f label $ fieldArgRepeated block
    where
        f _ Nothing = textInput block $ maybe "" E.value
                                      $ find fieldMatches
                                      $ fields event
        f (Just label) (Just True) = repeatedFieldContainer True $
            repeatedFields repeatedFieldValues $
                flip (M.insert "label") block . (label <>) . pack . show . (+1)
        f Nothing (Just True) = repeatedFieldContainer True $
            repeatedFields repeatedFieldValues $ const block
        f _ (Just False) = repeatedFieldContainer False $
            repeatedFields repeatedFieldValues $ const block
        repeatedFields fields block =
            if null fields
            then textInput (block $ length fields) ""
            else textInput (block $ length fields) (head fields) >>
                 repeatedFields (tail fields) block
        repeatedFieldContainer hasSuffix = repeatedFieldContainerWithLabel label
            (H.div !
                H.dataAttribute "repeated" (H.stringValue $ show hasSuffix))
        repeatedFieldContainerWithLabel Nothing c = c
        repeatedFieldContainerWithLabel (Just label) c =
            c ! H.dataAttribute "label" (H.textValue label)
        repeatedFieldValues = map E.value $ filter fieldMatches $ fields event
        fieldMatches = (== fieldArgField block) . E.field
        label = fieldArgLabel block

-- Widget (input field, calendar, etc.) for given CMark markdown code block
widget :: Day -> EventMap -> Event -> [(UUID, Text)] -> Text -> Text
widget today eventMap event attachmentNames txt
     | isCalendar = render $ calendar today eventMap
     | otherwise = render $ field id block event attachmentNames
    where isCalendar = txt == "calendar\n"
          block = parseBlock txt
          id = fromJust $ M.lookup "field" block
          render = toStrict . renderHtml

eventTypeSelectorHeader :: Event -> Text -> Text
eventTypeSelectorHeader event block =
    firstTabHtml <> "\n" <> secondTabHtml <> "\n" <>
        "<div class=\"tab-content\"><div class=\"tab-pane " <> onsiteActive <> "\" id=\"onsite\">\n"
    where
        (firstTabHtml, secondTabHtml) =
            if M.lookup "if" parsedBlock == Just "online"
            then (onlineTabHtml, onsiteTabHtml)
            else (onsiteTabHtml, onlineTabHtml)
        onsiteTabHtml =
            "<div class=\"form-check form-check-radio form-check-inline\">\
\  <label class=\"form-check-label\">\
\    <input" <> onsiteChecked <> " class=\"form-check-input\" type=\"radio\" name=\"eventType\" value=\"onsite\" onClick=\"$('#onsite').addClass('active');$('#online').removeClass('active');\">\
\    " <> onsiteLabel <> "\
\    <span class=\"circle\">\
\        <span class=\"check\"></span>\
\    </span>\
\  </label>\
\</div>"
        onlineTabHtml =
            "<div class=\"form-check form-check-radio form-check-inline\">\
\  <label class=\"form-check-label\">\
\    <input" <> onlineChecked <> " class=\"form-check-input\" type=\"radio\" name=\"eventType\" value=\"online\" onClick=\"$('#onsite').removeClass('active');$('#online').addClass('active');\">\
\    " <> onlineLabel <> "\
\    <span class=\"circle\">\
\        <span class=\"check\"></span>\
\    </span>\
\  </label>\
\</div>"
        onsiteActive = onsiteActive' event
        onsiteChecked = onsiteChecked' event
        onlineChecked = onlineChecked' event
        onsiteActive' Event { details = EventDetailsOnsite _ } = " active"
        onsiteActive' _ = ""
        onsiteChecked' Event { details = EventDetailsOnsite _ } = " checked=\"checked\""
        onsiteChecked' _ = ""
        onlineChecked' Event { details = EventDetailsOnline _ } = " checked=\"checked\""
        onlineChecked' _ = ""
        parsedBlock :: M.Map Text Text
        parsedBlock = parseBlock block
        onsiteLabel = fromMaybe "" $ M.lookup "onsite-label" parsedBlock
        onlineLabel = fromMaybe "" $ M.lookup "online-label" parsedBlock

eventTypeSelectorBody :: Event -> Text
eventTypeSelectorBody event =
    "</div><div class=\"tab-pane" <> onlineActive <> "\" id=\"online\">"
    where
        onlineActive = onlineActive' event
        onlineActive' Event { details = EventDetailsOnline _ } = " active"
        onlineActive' _ = ""

eventTypeSelectorFooter :: Text
eventTypeSelectorFooter = "</div></div>"


-- | Walk through all root in CMark markup tree,
-- replacing special code blocks with suitable widgets
walk :: Day -> EventMap -> Event -> [(UUID, Text)] -> Node -> Node
walk today eventMap event attachmentNames (Node position DOCUMENT children) =
    Node position DOCUMENT $ map mapRootChild children
    where
        mapRootChild (Node pos (CODE_BLOCK "" block) _)
            | T.isPrefixOf "if onsite\n" block || T.isPrefixOf "if online\n" block =
                Node pos (HTML_BLOCK (eventTypeSelectorHeader event block)) []
            | block == "else\n" = Node pos (HTML_BLOCK (eventTypeSelectorBody event)) []
            | block == "end\n" = Node pos (HTML_BLOCK eventTypeSelectorFooter) []
            | otherwise = Node pos (HTML_BLOCK $ widget' block) []
        mapRootChild node = node
        widget' = widget today eventMap event attachmentNames
walk _ _ _ _ node = node

-- | Whole form page inBlaze representation
eventFormPage :: Text             -- Google Maps API key
              -> Text             -- full event form markdown text
              -> Day              -- today
              -> EventMap         -- event map with all events (for calendar)
              -> Event            -- currently handled event
              -> [(UUID, Text)]   -- attachment UUID & attachment name pairs
              -> H.Html
eventFormPage gmapsKey markup today eventMap event attachmentNames =
    H.docTypeHtml ! A.lang "en" $ do
        H.head $ do
            H.meta ! A.charset "utf-8"
            H.link ! A.rel "apple-touch-icon" ! A.sizes "76x76" ! A.href "img/apple-icon.png"
            H.link ! A.rel "icon" ! A.type_ "image/png" ! A.href "img/favicon.png"
            H.meta ! A.httpEquiv "X-UA-Compatible" ! A.content "IE=edge"
            H.title $ H.toMarkup title
            H.meta ! A.content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0, shrink-to-fit=no" ! A.name "viewport"
            -- Fonts and icons
            H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "https://fonts.googleapis.com/css?family=Roboto:300,400,500,700%7CRoboto+Slab:400,700%7CMaterial+Icons"
            H.link ! A.rel "stylesheet" ! A.href "css/font-awesome.min.css"
            -- CSS Files
            H.link ! A.href "css/material-dashboard.min.css" ! A.rel "stylesheet"
            H.link ! A.href "css/event-page.css" ! A.rel "stylesheet"
            H.link ! A.href "css/file-input.css" ! A.rel "stylesheet"
            H.link ! A.href "css/label-height-fix.css" ! A.rel "stylesheet"
            -- JS Scripts
            H.script ! A.src "js/location-input.js" $ mempty
            H.script ! A.src "js/file-input.js" $ mempty
        H.body $ do
            H.div ! A.class_ "center-column" ! A.style "margin: auto" $ H.div ! A.class_ "card" $ do
                H.div ! A.class_ "card-header card-header-primary" $ H.h4 ! A.class_ "card-title" $ "event invitation form"
                H.div ! A.class_ "card-body" $ do
                    H.preEscapedToMarkup renderMarkup
                    H.input ! A.type_ "hidden" ! A.name "uuid" ! A.value (H.textValue $ toText $ uuid event)
                    case token event of
                        Nothing -> mempty
                        Just token -> H.input ! A.type_ "hidden" ! A.name "token" ! A.value (H.textValue token)
                    let buttonLabel = if uuid event == nil
                                      then "submit invitation"
                                      else "save changes"
                        loadingLabel = if uuid event == nil
                                       then "submitting"
                                       else "saving"
                    H.button ! A.type_ "submit" ! A.id "save-event"
                        ! A.class_ "btn btn-primary"
                        ! A.style "margin-top: 1.5rem; float: right;"
                        ! H.dataAttribute "loading-label" loadingLabel
                        !? (uuid event /= nil, A.disabled "disabled")
                        $ buttonLabel
            locationDataScript event today eventMap
            -- Core JS Files
            H.script ! A.src "js/core/jquery.min.js" $ mempty
            H.script ! A.src "js/core/popper.min.js" $ mempty
            H.script ! A.src "js/core/bootstrap-material-design.min.js" $ mempty
            H.script ! A.src "js/plugins/perfect-scrollbar.min.js" $ mempty
            -- Plugin for the momentJs
            H.script ! A.src "js/plugins/moment.min.js" $ mempty
            -- Plugin for the DateTimePicker, full documentation here: https://eonasdan.github.io/bootstrap-datetimepicker/
            H.script ! A.src "js/plugins/bootstrap-datetimepicker.min.js" $ mempty
            -- Google Maps Plugin
            let gmapsScriptUrl = "https://maps.googleapis.com/maps/api/js?key=" <> encodeText gmapsKey <> "&libraries=places"
            H.script ! A.src (H.textValue gmapsScriptUrl) $ mempty
            -- Notifications Plugin
            H.script ! A.src "js/plugins/bootstrap-notify.js" $ mempty
            -- Control Center for Material Dashboard: parallax effects, scripts for the example pages etc
            H.script ! A.src "js/material-dashboard.min.js" $ mempty
            -- Main application
            H.script ! A.src "js/events-form.js" $ mempty
            H.script ! A.src "js/label-height-fix.js" $ mempty
  where
      (title, content) = breakOn "\n" markup
      renderMarkup =
          nodeToHtml [optUnsafe] $ walk today eventMap event attachmentNames markupNode
      markupNode = commonmarkToNode [] content


-- | HTML for page where used will be redirected to
-- after submitting new event.
newEventSubmittedPage :: Event -> H.Html
newEventSubmittedPage event =
    H.docTypeHtml ! A.lang "en" $ do
        H.head $ do
            H.meta ! A.charset "utf-8"
            H.base ! A.href "../"
            H.link ! A.rel "apple-touch-icon" ! A.sizes "76x76" ! A.href "img/apple-icon.png"
            H.link ! A.rel "icon" ! A.type_ "image/png" ! A.href "img/favicon.png"
            H.meta ! A.httpEquiv "X-UA-Compatible" ! A.content "IE=edge"
            H.title "Event invitation has been submitted"
            H.meta ! A.content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0, shrink-to-fit=no" ! A.name "viewport"
            -- Fonts and icons
            H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "https://fonts.googleapis.com/css?family=Roboto:300,400,500,700%7CRoboto+Slab:400,700%7CMaterial+Icons"
            H.link ! A.rel "stylesheet" ! A.href "css/font-awesome.min.css"
            -- CSS Files
            H.link ! A.href "css/material-dashboard.min.css" ! A.rel "stylesheet"
            H.link ! A.href "css/event-page.css" ! A.rel "stylesheet"
        H.body $ do
            H.div ! A.class_ "center-column" $ H.div ! A.class_ "card" $ do
                H.div ! A.class_ "card-header card-header-primary" $ H.h4 ! A.class_ "card-title" $ "Event Invitation"
                H.div ! A.class_ "card-body" $ do
                    H.h3 "Event invitation has been submitted"
                    H.p $ do
                        let link = T.concat [toText $ uuid event, "?token=", fromJust $ token event]
                        "You should receive an email with the submitted information and "
                        H.a ! A.href (H.textValue link) $ "edit link"
                        "."
                    H.p $ H.button ! A.id "resend-email" ! A.class_ "btn btn-primary" $ "Resend email"
            H.script ! A.src "js/core/jquery.min.js" $ mempty
            H.script ! A.src "js/plugins/bootstrap-notify.js" $ mempty
            H.script $ H.toMarkup $ T.concat ["window.eventUuid='",  toText $ uuid event, "';window.eventToken='", fromJust $ token event, "';"]
            H.script ! A.src "js/resend-email.js" $ mempty
