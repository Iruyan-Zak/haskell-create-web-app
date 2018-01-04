module TcpServer where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.IO
import Control.Exception
import Control.Monad
import Data.Char
import Control.Concurrent
import qualified Data.ByteString.Char8 as BS

dumpHttpHeader :: Handle -> FilePath -> IO ()
istream `dumpHttpHeader` filename = withFile filename WriteMode copyHeader
    where
        copyHeader :: Handle -> IO ()
        copyHeader outHandle = do
            line <- hGetLine istream
            unless (2 > length line) $ hPutStr outHandle line >> copyHeader outHandle

echoOK :: BS.ByteString -> BS.ByteString
echoOK request = (dummyHeader `BS.append`) $  BS.pack "\r\n" `BS.append` request
    where
        dummyHeader = BS.pack "\
            \HTTP/1.1 200 OK\r\n\
            \Connection: Close\r\n"

infLoop :: BS.ByteString -> BS.ByteString
infLoop _ = BS.pack . show . or $ repeat False

runServer :: String -> String -> (BS.ByteString -> IO BS.ByteString) -> IO ()
runServer address port responding = bracket startServer stopServer acceptLoop
    where
        startServer :: IO Socket
        startServer = do
            sock <- createSocket
            listen sock 5
            putStrLn $ "Server is running on " ++ address ++ ":" ++ port ++ "."
            return sock

        stopServer :: Socket -> IO ()
        stopServer sock = do
            close sock
            putStrLn "Server stopped."

        acceptLoop :: Socket -> IO ()
        acceptLoop sock = forever $ do
            (conn, _) <- accept sock
            forkIO $ do
                recv conn 4096 >>= responding >>= sendAll conn
                close conn

        createSocket :: IO Socket
        createSocket = do
            let hints = defaultHints{ addrFlags = [AI_NUMERICHOST], addrSocketType = Stream }

            (AddrInfo _ addressFamily socketType protocolNumber socketAddress _)
                <- head <$> getAddrInfo (Just hints) (Just address) (Just port)
            sock <- socket addressFamily socketType protocolNumber
            bind sock socketAddress
            return sock

