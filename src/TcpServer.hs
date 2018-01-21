module TcpServer where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.IO
import Control.Exception
import Control.Monad
import Control.Concurrent
import qualified Data.ByteString.Char8 as BS

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

