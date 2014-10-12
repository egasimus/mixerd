{-# LANGUAGE OverloadedStrings #-}


module Mixer.Backend.PulseAudio ( pulseMixer ) where


import           Control.Arrow
import           Control.Concurrent      (forkIO, ThreadId)
import           Control.Concurrent.MVar
import           Data.ByteString.Char8   (ByteString)
import qualified Data.ByteString.Char8   as BC
import qualified Data.Map                as     Map
import           Data.Tuple
-- import          Data.Maybe
import           Network.DBus
import           Network.DBus.Actions
import           Network.DBus.MessageType


pulseMixer :: IO (ThreadId, DBusConnection)
pulseMixer = do
    sessionBus <- establish busGetSession authenticateWithRealUID
    socketPath <- pulseGetAddress sessionBus
    (pid, con) <- pulseConnect socketPath

    call con "org.PulseAudio.Core1" $
        DBusCall "/org/pulseaudio/core1" "ListenForSignal"
        (Just "org.PulseAudio.Core1")
        [ DBusString "org.PulseAudio.Core1.NewPlaybackStream"
        , DBusArray SigObjectPath [] ]

    registerSignal con "/org/pulseaudio/core1" $ calltableFromList
        [ ("NewPlaybackStream", "org.PulseAudio.Core1", onNewPlaybackStream con) ]

    return (pid, con)


pulseGetAddress :: DBusConnection -> IO ByteString
pulseGetAddress conn = do
    response <- call conn "org.PulseAudio1" $ DBusCall
        "/org/pulseaudio/server_lookup1" "Get"
        (Just "org.freedesktop.DBus.Properties")
        [ DBusString "org.PulseAudio.ServerLookup1"
        , DBusString "Address" ]
    return $ ustringToBS $ fromVariant $ head $ returnBody response
        where fromVariant (DBusVariant (DBusString s)) = s


pulseConnect :: ByteString -> IO (ThreadId, DBusConnection)
pulseConnect s = do
    let (domain, flagstr) = second BC.tail $ BC.breakSubstring ":" s
    let flags = map (\x -> let (k:v:[]) = BC.split '=' x in (k,v)) $ BC.split ',' flagstr
    case lookup "path" flags of
        Nothing   -> error "No path found to PulseAudio socket :/"
        Just path -> do
            ctx <- connectUnix path >>= contextNew
            authenticateWithRealUID ctx
            pulseMainLoop ctx


pulseMainLoop :: DBusContext -> IO (ThreadId, DBusConnection)
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
                , connectionDefaultCallback = \_ -> return ()
                , connectionMainLoop        = mainloopPid
                , connectionSendLock        = sendLockVar
                }
    pid <- forkIO (dispatcher con)
    putMVar mainloopPid pid
    return (pid, con)


onNewPlaybackStream :: DBusConnection -> Signalback
onNewPlaybackStream conn _ _ (body:_) = do
    let stream = (\(DBusObjectPath a) -> a) body
    print $ (\(ObjectPath a) -> a) stream

    _ <- forkIO $ do
        let getP prop = do
            p <- getStreamProperty conn stream prop
            print p
        mapM_ getP
            [ "Index"
            , "Driver"
            , "OwnerModule"
            , "Client"
            , "Device"
            , "SampleFormat"
            , "SampleRate"
            , "Channels"
            , "Volume"
--            , "VolumeWritable"
            , "Mute"
            , "BufferLatency"
            , "DeviceLatency"
            , "ResampleMethod"
            , "PropertyList"
            ]

        return ()

    return ()


getStreamProperty conn stream prop = call conn "org.PulseAudio.Core1" $
    DBusCall stream "Get"
    (Just "org.freedesktop.DBus.Properties")
    [ DBusString "org.PulseAudio.Core1.Stream"
    , DBusString prop ]
