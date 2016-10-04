module Networking where
import Protolude

import Data.Text as Text
import Data.Text.Encoding
import qualified Network.Socket as Socket hiding (send, recv)
import qualified Network.Socket.ByteString as Socket

import Settings


data LocalSocket = LocalSocket
    { _socket :: Socket.Socket
    , _localhost :: Socket.SockAddr
    }

sendUDP :: Settings -> Text -> IO ()
sendUDP settings message =
    bracket (localSocket settings >>= connect) close (send message)

recvUDP :: Settings -> IO Text
recvUDP settings =
    bracket (localSocket settings >>= listen) close recv
                   
localSocket :: Settings -> IO LocalSocket
localSocket settings = do
    socket <- Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol
    Socket.setSocketOption socket Socket.ReusePort 1
    address <- Socket.inet_addr "127.0.0.1"
    return (LocalSocket socket (Socket.SockAddrInet (_udpPort settings) address))

connect :: LocalSocket -> IO LocalSocket
connect socket = do
    Socket.connect (_socket socket) (_localhost socket)
    return socket

listen :: LocalSocket -> IO LocalSocket
listen socket = do
    Socket.bind (_socket socket) (_localhost socket)
    return socket

send :: Text -> LocalSocket -> IO ()
send message socket =
    void (Socket.send (_socket socket) (encodeUtf8 message))

recv :: LocalSocket -> IO Text
recv socket = do
    fmap decodeUtf8 (Socket.recv (_socket socket) 128)

close :: LocalSocket -> IO ()
close =
    Socket.close . _socket
