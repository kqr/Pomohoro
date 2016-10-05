module Main where
import Protolude

import Options.Applicative hiding ((<>))
import System.Posix.Daemonize
import Data.Text as Text
import Data.Time
import System.Timeout

import Settings
import Timeclock
import Notifications
import Networking


data Command
    = Start { _account :: Text, _comment :: Text }
    | Status
    | Interrupt
    | RemindMe { _minutes :: Int, _message :: Text }
    deriving Show


args :: Parser Command
args =
    subparser (mconcat [
        command "start" (info (
            Start
                <$> textArgument
                <*> textRestArguments
        ) (progDesc (mconcat [
            "Start a new work session for the specified timeclock",
            " account (or default if none specified)"
        ]))),
        command "status" (info (
            pure Status
        ) (progDesc
            "How long has the current session been going on for?"
        )),
        command "int" (info (
            pure Interrupt
        ) (progDesc
            ("Interrupt the ongoing work session")
        )),
        command "rem" (info (
            RemindMe
                <$> argument auto (value 5)
                <*> textRestArguments 
        ) (progDesc (mconcat [
            "Wait for the specified number of minutes and then send",
            " a notification. Good for keeping time on breaks"
        ])))])


textArgument :: Parser Text
textArgument =
    fmap pack (strArgument (value ""))


textRestArguments :: Parser Text
textRestArguments =
    fmap (unwords . fmap pack) (many (strArgument mempty))


main :: IO ()
main = do
    settings <- loadSettings
    parsed <- execParser (info (helper <*> args) (progDesc (mconcat
        [ "A small pomodoro timer based on CLI usage and FreeDesktop.org"
        , " notifications. Optionally writes pomodoro sessions to a"
        , " timeclock file for budgeting and reporting."])))
    case parsed of
        Start account comment -> startWork settings account comment
        Status -> queryStatus settings
        Interrupt -> interrupt settings
        RemindMe minutes message -> remindMe minutes message


startWork :: Settings -> Text -> Text -> IO ()
startWork settings account comment =
    daemonize $ do
        startTime <- getZonedTime
        appendFile (_timeclockPath settings) (clockin
            startTime
            (if Text.null account then _defaultAccount settings else account)
            comment)
        void (timeout (usecFromMinutes (_sessionLength settings))
            (handleSession settings startTime))
        stopTime <- getZonedTime
        appendFile (_timeclockPath settings) (clockout stopTime)
        notifySend "Work is over!"
            ("You have worked for " <> show (minuteDiff stopTime startTime) <> " minutes. Well done!")


handleSession :: Settings -> ZonedTime -> IO ()
handleSession settings startTime = do
    request <- recvUDP settings
    case request of
        "interrupt" -> pure ()
        "status" -> do
            nowTime <- getZonedTime
            sendUDP settings (show (minuteDiff nowTime startTime))
            handleSession settings startTime
        _ -> handleSession settings startTime


queryStatus :: Settings -> IO ()
queryStatus settings = do
    sendUDP settings "status"
    response <- recvUDP settings
    putStrLn ("Current work session: " <> response <> "/" <> show (_sessionLength settings) <> " minute(s)")
    

interrupt :: Settings -> IO ()
interrupt settings =
    sendUDP settings "interrupt"


remindMe :: Int -> Text -> IO ()
remindMe minutes message =
    daemonize $ do
        threadDelay (usecFromMinutes minutes)
        notifySend "Time's up!" message
    

