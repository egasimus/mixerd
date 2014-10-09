{-# LANGUAGE OverloadedStrings #-}


import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import DBus
import DBus.Client hiding (throwError)
import DBus.Socket


main :: IO ()
main = runExceptT pulse >>= \r -> case r of
  Left e  -> print e
  Right s -> putStrLn "success! s is a socket here!"


pulse :: ExceptT String IO Socket
pulse = do
    sessionBus <- liftIO connectSession
    address    <- getPulseAddress sessionBus
    liftIO $ print address
    pulseConnect address


getPulseAddress :: Client -> ExceptT String IO String
getPulseAddress client = do
  reply <- liftIO $ call client pulseCall

  let
    maddr  = either (const Nothing) extract reply
    extract = join . fmap fromVariant . fromVariant . head . methodReturnBody

  maybe (throwError "Can't find Pulseaudio socket.") return maddr


pulseConnect :: String -> ExceptT String IO Socket
pulseConnect a = maybe (throwError "Can't parse address")
                       (liftIO . open)
                       (parseAddress a)


pulseCall :: MethodCall
pulseCall =
  (methodCall
  "/org/pulseaudio/server_lookup1"
  "org.freedesktop.DBus.Properties"
  "Get")
  { methodCallDestination = Just "org.PulseAudio1"
  , methodCallBody        =
    [ toVariant ("org.PulseAudio.ServerLookup1" :: String)
    , toVariant ("Address" :: String) ]}
