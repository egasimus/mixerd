{-# LANGUAGE OverloadedStrings #-}


module Mixer.PulseAudio ( pulseMixer ) where


import           Control.Arrow
import           Control.Concurrent      (forkIO)
import           Control.Concurrent.MVar
import           Data.ByteString.Char8   (ByteString)
import qualified Data.ByteString.Char8   as BC
import qualified Data.Map                as     Map
-- import          Data.Maybe
import           Network.DBus
import           Network.DBus.Actions
import           Network.DBus.MessageType


pulseMixer :: IO DBusConnection
pulseMixer = do
    sessionBus <- establish busGetSession authenticateWithRealUID
    socketPath <- pulseGetAddress sessionBus
    pulseBus   <- pulseConnect socketPath

    call pulseBus "org.PulseAudio.Core1" $ DBusCall
        "/org/pulseaudio/core1"
        "ListenForSignal"
        (Just "org.PulseAudio.Core1")
        [ DBusString "org.PulseAudio.Core1.NewPlaybackStream"
        , DBusArray SigObjectPath [] ]

    registerSignal pulseBus "/org/pulseaudio/core1" $ calltableFromList
        [ ("NewPlaybackStream", "org.PulseAudio.Core1", onNewPlaybackStream) ]

    return pulseBus


pulseGetAddress :: DBusConnection -> IO ByteString
pulseGetAddress conn = do
    response <- call conn "org.PulseAudio1" $ DBusCall
        "/org/pulseaudio/server_lookup1" "Get"
        (Just "org.freedesktop.DBus.Properties")
        [ DBusString "org.PulseAudio.ServerLookup1"
        , DBusString "Address" ]
    return $ ustringToBS $ fromVariant $ head $ returnBody response
        where fromVariant (DBusVariant (DBusString s)) = s


pulseConnect :: ByteString -> IO DBusConnection
pulseConnect s = do
    let (domain, flagstr) = second BC.tail $ BC.breakSubstring ":" s
    let flags = map (\x -> let (k:v:[]) = BC.split '=' x in (k,v)) $ BC.split ',' flagstr
    let path = lookup "path" flags
    case path of
        Nothing -> error "No path found to PulseAudio socket :/"
        Just path -> do
            ctx <- connectUnix path >>= contextNew
            authenticateWithRealUID ctx
            pulseMainLoop ctx


pulseMainLoop context = do
    callbacks   <- newMVar Map.empty
    callPaths   <- newMVar Map.empty
    signalPaths <- newMVar Map.empty
    mainloopPid <- newEmptyMVar
    sendLockVar <- newMVar ()

    let con = DBusConnection
                { connectionContext         = context
                , connectionCallbacks       = callbacks
                , connectionPaths           = callPaths
                , connectionSignals         = signalPaths
                , connectionDefaultCallback = pulseHandler
                , connectionMainLoop        = mainloopPid
                , connectionSendLock        = sendLockVar
                }
    pid <- forkIO (dispatcher con)
    putMVar mainloopPid pid
    print pid
    return con


pulseHandler :: DBusMessage -> IO ()
pulseHandler msg = do
    print msg
    return ()


onNewPlaybackStream :: Signalback
onNewPlaybackStream bus sig body = do
    print bus
    print sig
    print body
    return ()
