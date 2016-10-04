module Notifications where
import Protolude

import qualified Data.Text as Text
import qualified DBus.Client as DBus
import qualified DBus.Notify as DBus


notifySend :: Text -> Text -> IO ()
notifySend title body = do
    client <- DBus.connectSession
    void (DBus.notify client (DBus.blankNote {
        DBus.summary = Text.unpack title,
        DBus.body = Just (DBus.Text (Text.unpack body))
    }))
