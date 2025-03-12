{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Mailer
Description : Functions for assembling and sending emails
-}
module Mailer (sendMail, sendNotification) where

import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL
import Network.Mime (defaultMimeLookup)
import Network.Mail.Mime (Address(..), Mail(..), htmlPart, filePartBS)
import Network.HaskellNet.SMTP (doSMTP, doSMTPPort, authenticate, AuthType(PLAIN))
import qualified Network.HaskellNet.SMTP as SMTP
import Network.HaskellNet.SMTP.SSL (doSMTPSSLWithSettings, doSMTPSTARTTLSWithSettings, Settings(..), defaultSettingsSMTPSSL, defaultSettingsSMTPSTARTTLS)
import Configuration (Configuration)
import qualified Configuration as C (Configuration(mail, smtp), Mail(..), Smtp(..), SmtpAuthentication(..), TlsMode(..))


sourceAddress :: Configuration -> Address
sourceAddress c = Address (C.mailFromName $ C.mail c) (C.mailFrom $ C.mail c)

notificationsAddress :: Configuration -> Address
notificationsAddress c = Address (C.notificationsToName $ C.mail c) (C.notificationsTo $ C.mail c)


-- | Sends new event to application user
sendNotification :: C.Configuration
                 -> Text
                 -> Text
                 -> [(Text, BL.ByteString)]
                 -> IO ()
sendNotification configuration = sendMail configuration $ notificationsAddress configuration


-- | Sends edit link to inviter
sendMail :: Configuration
         -> Address
         -> Text
         -> Text
         -> [(Text, BL.ByteString)]
         -> IO ()
sendMail configuration address subject content attachments =
    sendMail' configuration Mail { mailFrom = sourceAddress configuration
                                 , mailTo = [address]
                                 , mailCc = []
                                 , mailBcc = []
                                 , mailHeaders = [("Subject", subject)]
                                 , mailParts = parts }
    where
        parts = [htmlPart $ TL.fromStrict content] : map attachment attachments
        attachment (name, content) = [filePartBS (mimeType name) name content]
        mimeType name = decodeUtf8 $ defaultMimeLookup name


-- | Sends mime-mail compatible mail
sendMail' :: Configuration -> Mail -> IO ()
sendMail' configuration mail = connect $ \ connection -> do
    authSuccess <- auth connection
    if authSuccess
        then SMTP.sendMail mail connection
        else fail "Failed to authenticate against SMTP server"
    where
        hostname = unpack $ C.host $ C.smtp configuration
        sslSettings = case C.port $ C.smtp configuration of
            Nothing -> defaultSettingsSMTPSSL
            Just port -> defaultSettingsSMTPSSL { sslPort = fromIntegral port}
        startTlsSettings = case C.port $ C.smtp configuration of
            Nothing -> defaultSettingsSMTPSTARTTLS
            Just port -> defaultSettingsSMTPSTARTTLS { sslPort = fromIntegral port }
        connect = case C.tlsMode $ C.smtp configuration of
            C.None -> case C.port $ C.smtp configuration of
                Nothing -> doSMTP hostname
                Just port -> doSMTPPort hostname $ fromIntegral port
            C.Ssl -> doSMTPSSLWithSettings hostname sslSettings
            C.StartTls -> doSMTPSTARTTLSWithSettings hostname startTlsSettings
        auth connection = case C.authentication $ C.smtp configuration of
            Nothing -> return True
            Just a -> authenticate PLAIN (unpack $ C.username a)
                                   (unpack $ C.password a) connection

