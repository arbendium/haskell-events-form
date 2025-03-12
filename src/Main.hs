{-|
Module      : Main
Description : Application entry point

This module exports `main` function which
initializes and bootstraps application.
-}
module Main where

import System.IO (stdout, BufferMode(LineBuffering), hSetBuffering)
import Data.String (fromString)
import Data.Text (unpack)
import Network.Wai.Handler.Warp (runSettings
                               , defaultSettings
                               , setPort
                               , setHost
                               , setBeforeMainLoop)

import Configuration (configuration
                    , Configuration
                    , Configuration(listen)
                    , Configuration(database)
                    , Listen(port)
                    , Listen(host))
import Application (application)
import EventDatabase (newEventCache, readEvents)
import Notifier (runNotifier)


-- | Returns an action to run just before main loop.
-- The action will print a message about server
-- being listening on configured address and port.
beforeMainLoop :: Configuration -> IO ()
beforeMainLoop c = do
    -- Unfortunately, WAI (Warp) does not provide a way to query
    -- exact listening address after socket has been set up.
    let p = show $ port $ listen c
    putStrLn $ "Listening on " ++ case host $ listen c of
        Just h -> unpack h ++ ":" ++ p
        Nothing -> p


-- | Main application entry point
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    c <- configuration

    eventCache <- newEventCache
    readEvents (unpack $ database c) eventCache

    _ <- runNotifier c eventCache

    -- Set listening port and (optionally) address
    let settings = setPort (port (listen c)) $
            case host $ listen c of
                Just h -> setHost (fromString $ unpack h) defaultSettings
                Nothing -> defaultSettings

    runSettings (setBeforeMainLoop (beforeMainLoop c) settings) $ application c eventCache
