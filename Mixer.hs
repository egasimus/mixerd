module Mixer ( MixerInfo(..) ) where


import Control.Concurrent
import Network.DBus


data MixerInfo = MixerInfo
    { mainThreadId    :: ThreadId
    , pulseThreadId   :: ThreadId
    , pulseConnection :: DBusConnection
    }
