{-# LANGUAGE OverloadedStrings #-}


import Control.Concurrent
import Control.Monad
import Mixer.PulseAudio
import System.Exit
import System.Posix.Signals


main :: IO ()
main = do
    myPid <- myThreadId
    (pulsePid, pulseBus) <- pulseMixer
    installHandler keyboardSignal (Catch (do killThread pulsePid; killThread myPid)) Nothing
    forever $ threadDelay 10000
    return ()
