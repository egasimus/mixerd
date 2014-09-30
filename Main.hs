{-# LANGUAGE OverloadedStrings #-}


import Data.Maybe
import DBus
import DBus.Client
import DBus.Socket


main = do
    client <- connectSession

    socketPath <- getPulseAddress client
    case socketPath of
        Nothing -> putStrLn "Can't find PulseAudio socket."
        Just s  -> putStrLn $ "Connecting to PulseAudio at: " ++ s


getPulseAddress :: Client -> IO (Maybe String)
getPulseAddress client = do
    reply <- call_ client (methodCall "/org/pulseaudio/server_lookup1"
                             "org.freedesktop.DBus.Properties"
                             "Get")
        { methodCallDestination = Just "org.PulseAudio1"
        , methodCallBody        = [toVariant ("org.PulseAudio.ServerLookup1" :: String),
                                   toVariant ("Address" :: String)]}
    return $ (fromVariant =<< fromVariant ((methodReturnBody reply) !! 0))


pulseConnect :: Maybe String -> Maybe (IO Socket)
pulseConnect Nothing  = Nothing
pulseConnect (Just s) = maybe Nothing (Just . open) $ parseAddress s
