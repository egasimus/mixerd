{-# LANGUAGE OverloadedStrings #-}


import Control.Error
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import DBus
import DBus.Client
import Mixer.PulseAudio


main :: IO ()
main = do

    -- Init connection to PulseAudio
    sessionBus <- connectSession
    socketPath <- pulseGetAddress sessionBus
    pulseBus   <- pulseConnect socketPath

    case pulseBus of
        Nothing ->
            putStrLn "Can't connect to PulseAudio over DBus."
        Just _ -> do
            pulseHandler <- addMatch sessionBus matchAny handlePulseSignal
            putStrLn $ "Listening to PulseAudio over DBus at " ++ fromJust socketPath
            _ <- mainLoop
            removeMatch sessionBus pulseHandler
            putStrLn "Disconnected from PulseAudio."

    return ()

    -- Main loop
    {-mainLoop <- runEitherT $ forever $ do-}
        {-lift . putStr $ "mixerd :( "-}
        {-str <- lift getLine-}
        {-processCommand str-}

    {--- Exit handling-}
    {-case mainLoop of-}
        {-Left  a -> putStrLn "Goodbye."-}
        {-Right a -> return ()-}


mainLoop :: IO (Either () a)
mainLoop = runEitherT $ forever $ do
    str <- lift getLine
    when (str == "exit") $ left ()


handlePulseSignal :: Signal -> IO ()
handlePulseSignal = print


