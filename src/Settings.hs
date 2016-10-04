module Settings where
import Protolude

import Data.Configurator
import System.Directory
import qualified Network.Socket as Socket (PortNumber)


data Settings = Settings
    { _sessionLength :: Int
    , _defaultAccount :: Text
    , _timeclockPath :: FilePath
    , _udpPort :: Socket.PortNumber
    }


loadSettings :: IO Settings
loadSettings = do
    cfgPath <- fmap (<> "/.pomohoro.cfg") getHomeDirectory
    config <- load [Optional cfgPath]
    Settings
        <$> lookupDefault 25 config "session-length"
        <*> lookupDefault "work" config "default-account"
        <*> lookupDefault "/tmp/pomohoro.timeclock" config "timeclock-file"
        <*> fmap fromInteger (lookupDefault 8712 config "port")
