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
    home <- getHomeDirectory
    config <- load [Optional (home <> "/.pomohoro.cfg")]
    Settings
        <$> lookupDefault 25 config "session-length"
        <*> lookupDefault "work" config "default-account"
        <*> lookupDefault (home <> "/.pomohoro.timeclock") config "timeclock-file"
        <*> fmap fromInteger (lookupDefault 8712 config "port")
