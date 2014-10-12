{-# LANGUAGE OverloadedStrings #-}


import Control.Concurrent
import Control.Monad
import Mixer
import Mixer.Backend.PulseAudio
import Mixer.UI.Console
import System.Exit
import System.Posix.Signals


main :: IO ()
main = do
    mainThreadId <- myThreadId
    (pulseThreadId, pulseConnection) <- pulseMixer

    let mixer = MixerInfo { mainThreadId    = mainThreadId
                          , pulseThreadId   = pulseThreadId
                          , pulseConnection = pulseConnection
                          }

    installHandler keyboardSignal (Catch (do killThread mainThreadId; killThread pulseThreadId)) Nothing
    runConsole mixer
    return ()
