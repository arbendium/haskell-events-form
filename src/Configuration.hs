{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}
{- |
Module      : Configuration
Description : Configuration-related data types and functions
-}
module Configuration where

import Control.Applicative (liftA2, (<|>))
import System.Environment (getProgName, getArgs, getExecutablePath)
import System.FilePath (replaceFileName, takeDirectory)
import System.Exit (exitFailure)
import System.IO (stderr, hPutStrLn)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Data.Ini.Config (IniParser, parseIniFile, section, field, fieldMb,
                        fieldOf, fieldMbOf, number)


-- configuration data structures

-- | Mail confiugration
data Mail = Mail
    { mailFromName :: Maybe Text
    , mailFrom :: Text
    , notificationsToName :: Maybe Text
    , notificationsTo :: Text } deriving (Show)

-- | GMail configuration
data Gmail = Gmail
    { username :: Text
    , password :: Text } deriving (Show)

-- | Ssl mode
data TlsMode = None | Ssl | StartTls deriving (Show)

-- | Representation of SMTP credentials
data SmtpAuthentication = SmtpAuthentication
    { username :: Text
    , password :: Text } deriving (Show)

-- | SMTP confiugration
data Smtp = Smtp
    { host :: Text
    , port :: Maybe Int
    , tlsMode :: TlsMode
    , authentication :: Maybe SmtpAuthentication } deriving (Show)

-- | HTTP listener configuration
data Listen = Listen
    -- `host` will always be `Just` value (defaulting to `localhost`) but
    -- left with `Maybe` type for semantical clarity
    { host :: Maybe Text
    , port :: Int } deriving (Show)

-- | Application configuration
data Configuration = Configuration
    { appDirectory :: Text
    , database :: Text
    , attachments :: Text
    , notifications :: Text
    , listen :: Listen
    , url :: Text
    , gmapsKey :: Text
    , mail :: Mail
    , smtp :: Smtp
    , gmail :: Gmail } deriving (Show)


configParser ::FilePath -> IniParser Configuration
configParser executableDirectory = do
    listen <- section "main" $ do
        h <- fieldMb "host"
        p <- fieldOf "port" number
        return $ Listen (h <|> Just "127.0.0.1") p
    url <- section "main" $ field "url"
    appDir <- section "main" $
        fromMaybe (pack executableDirectory) <$> fieldMb "appdir"
    dataDir <- section "main" $ field "datadir"
    gmapsKey <- section "gmaps" $ field "key"
    mail <- section "emails" $ do
        fn <- fieldMb "from_name"
        fe <- field "from_email"
        nn <- fieldMb "notifications_to_name"
        ne <- field "notifications_to_email"
        return $ Mail fn fe nn ne
    smtp <- section "smtp" $ do
        h <- field "host"
        p <- fieldMbOf "port" number
        let tlsMode "none" = Right None
            tlsMode "ssl" = Right Ssl
            tlsMode "starttls" = Right StartTls
            tlsMode _ = Left "Bad value for tls_mode"
        t <- fieldOf "tls_mode" tlsMode
        u <- fieldMb "username"
        w <- fieldMb "password"
        return $ Smtp h p t $ liftA2 SmtpAuthentication u w
    gmail <- section "gmail" $ do
        u <- field "username"
        p <- field "password"
        return $ Gmail u p
    return Configuration
        { appDirectory = appDir
        , database = dataDir <> "/database.json"
        , attachments = dataDir <> "/attachments"
        , notifications = dataDir <> "/notifications"
        , listen = listen
        , url = url
        , gmapsKey = gmapsKey
        , mail = mail
        , smtp = smtp
        , gmail = gmail }


-- | Loads configuration from given file
configuration' :: FilePath -> IO Configuration
configuration' fileName = do
    executableDirectory <- takeDirectory <$> getExecutablePath
    fileContent <- TIO.readFile fileName
    case parseIniFile fileContent $ configParser executableDirectory of
        Left err -> error $ "Failed to parse configuration file "
                        ++ fileName ++ ": " ++ err
        Right config ->  return config


-- | Loads confiugration from file indicated by
-- first application argument.
--
-- If there's an error (bad argument, bad configuration, etc.),
-- prints error to stderr and exits with non-zero exit code
configuration :: IO Configuration
configuration = do
    executablePath <- getExecutablePath
    progName <- getProgName
    args <- getArgs
    fileName <- case args of
        [] -> return $ replaceFileName executablePath "config.ini"
        [a] -> return a
        _  -> do hPutStrLn stderr $ progName ++ ": Too many arguments"
                 exitFailure

    configuration' fileName
