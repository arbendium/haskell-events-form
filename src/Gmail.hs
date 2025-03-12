{-# LANGUAGE OverloadedStrings #-}
module Gmail (createDraft) where

import Numeric (showHex)
import Data.Maybe (fromJust)
import Data.ByteString.Builder (toLazyByteString, byteStringHex)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as TL
import Data.List (find)
import System.Entropy (getEntropy)
import Network.Mail.Mime (Mail(..), Address(..), renderMail', htmlPart,
                          partEncoding, Encoding(None))
import Network.HaskellNet.IMAP.SSL (Attribute(OtherAttr), SearchQuery(HEADERs),
                                    connectIMAPSSL, login, list, append, select,
                                    search, fetchByString)
import Configuration (Configuration)
import qualified Configuration as C (Configuration(mail, gmail),
                                     Mail(..), Gmail(..))


sourceAddress :: Configuration -> Address
sourceAddress c = Address (C.mailFromName $ C.mail c) (C.mailFrom $ C.mail c)


-- TODO: escape authuser/username
-- TODO: find a way to link to full screen draft editor
gmailUrl :: Configuration -> Text -> Text
gmailUrl configuration threadId = "https://mail.google.com/mail/u?authuser=" <>
                                  C.mailFrom (C.mail configuration) <>
                                  "#drafts/" <> threadId

gmailImapHostname :: String
gmailImapHostname = "imap.gmail.com"


-- | Create draft email and return GMail URL to
-- that draft
createDraft :: Configuration -> Address -> Text -> Text -> IO Text
createDraft configuration destination subject content = do
    connection <- connectIMAPSSL gmailImapHostname
    login connection gmailUsername gmailPassword
    mailboxes <- list connection
    let (_, drafts) = fromJust $ find isDraftsMailbox mailboxes
    responseId <- toHex <$> getEntropy 24
    mailBytes <- renderMail' $ mail responseId
    append connection drafts $ BL.toStrict mailBytes
    select connection drafts
    mails <-
        search connection [HEADERs "X-Event-Response-Id" $ unpack responseId]
    fields <- fetchByString connection (last mails) "(X-GM-MSGID)"
    let (_, msgId) = fromJust $ find ((=="X-GM-MSGID") . fst) fields
    let messageId = pack $ showHex (read msgId :: Integer) ""
    return $ gmailUrl configuration messageId
    where
        mail responseId = Mail { mailFrom = sourceAddress configuration
                               , mailTo = [destination]
                               , mailCc = []
                               , mailBcc = []
                               , mailHeaders =
                                   [ ("Subject", subject)
                                   , ("X-Event-Response-Id", responseId) ]
                               , mailParts = [[part]] }
        -- due to lack of proper quoted-pritable encoding support in GMail web
        -- interface, the encoding is set to None. There might actually be
        -- better option (i.e. binary, base64 etc.)
        part = (htmlPart $ TL.fromStrict content) { partEncoding = None }
        isDraftsMailbox = elem (OtherAttr "Drafts") . fst
        toHex = decodeUtf8 . BL.toStrict . toLazyByteString . byteStringHex
        gmailUsername = unpack $ C.username $ C.gmail configuration
        gmailPassword = unpack $ C.password $ C.gmail configuration
