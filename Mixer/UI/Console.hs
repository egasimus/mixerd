{-# LANGUAGE OverloadedStrings #-}


module Mixer.UI.Console ( runConsole ) where


import Data.Foldable
import Mixer
import System.Console.Haskeline


runConsole pulsePid = runInputT defaultSettings $ loop pulsePid 


loop :: MixerInfo -> InputT IO ()
loop mixer = do
    input <- getInputLine "mixerd :) "
    forM_ input $ respond mixer


respond :: MixerInfo -> String -> InputT IO ()
respond mixer input = case input of
    "exit"   -> return ()
    "status" -> cmdStatus mixer
    _        -> do outputStrLn input
                   loop mixer


cmdStatus :: MixerInfo -> InputT IO ()
cmdStatus mixer = do 
    outputStrLn $ show $ pulseThreadId mixer
    return ()
