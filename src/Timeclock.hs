module Timeclock where
import Protolude

import Data.Time
import qualified Data.Text as Text


clockin :: ZonedTime -> Text -> Text -> Text
clockin time account comment =
    mconcat
        [ "i "
        , toTimestamp time
        , " "
        , account
        , if Text.null comment then "" else "  " <> comment
        , "\n"
        ]

clockout :: ZonedTime -> Text
clockout time =
    "o " <> toTimestamp time <> "\n"
        

toTimestamp :: ZonedTime -> Text
toTimestamp time =
    Text.pack (formatTime defaultTimeLocale "%F %T" time)


minuteDiff :: ZonedTime -> ZonedTime -> Int
minuteDiff a b =
    let
        seconds = diffUTCTime (zonedTimeToUTC a) (zonedTimeToUTC b)
    in
        round (seconds / 60)


usecFromMinutes :: Int -> Int
usecFromMinutes minutes =
    60 * 1000000 * minutes
