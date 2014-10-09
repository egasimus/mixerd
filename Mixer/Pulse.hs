{-# LANGUAGE OverloadedStrings #-}


module Mixer.PulseAudio ( pulseGetAddress,
                          pulseConnect ) where


import DBus
import DBus.Client


pulseGetAddress :: Client -> IO (Maybe String)
pulseGetAddress client = do
    reply <- call_ client (methodCall "/org/pulseaudio/server_lookup1"
                             "org.freedesktop.DBus.Properties"
                             "Get")
        { methodCallDestination = Just "org.PulseAudio1"
        , methodCallBody        = [ toVariant ("org.PulseAudio.ServerLookup1" :: String)
                                  , toVariant ("Address" :: String) ] }
    return (fromVariant =<< fromVariant (head $ methodReturnBody reply))


pulseConnect :: Maybe String -> IO (Maybe Client)
pulseConnect ms = case ms >>= parseAddress of
    Nothing -> return Nothing
    Just a  -> do s <- connect a
                  return (Just s)
