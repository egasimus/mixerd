{-# LANGUAGE OverloadedStrings #-}


import Control.Concurrent
import Control.Monad
import Mixer.PulseAudio


main :: IO ()
main = do
    pulse <- pulseMixer
    forever $ threadDelay 10000
    return ()
