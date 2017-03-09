import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.IO
import Control.Exception
import Control.Monad
import Data.Char

main :: IO ()
main = runServer "127.0.0.1" "5000" (`dumpHttpHeader` "output.log")

dumpHttpHeader :: Handle -> FilePath -> IO ()
istream `dumpHttpHeader` filename = withFile filename WriteMode copyHeader
    where
        copyHeader :: Handle -> IO ()
        copyHeader outHandle = do
            line <- hGetLine istream
            unless (2 > length line) $ hPutStr outHandle line >> copyHeader outHandle

runServer :: String -> String -> (Handle -> IO a) -> IO a
runServer addr port = bracket startServer stopServer where
    startServer :: IO Handle
    startServer = do
        let hints = defaultHints{ addrFlags = [AI_NUMERICHOST], addrSocketType = Stream }

        addrInfo@(AddrInfo _ fam stype pnum addr _):_
            <- getAddrInfo (Just hints) (Just addr) (Just port)
        sock <- socket fam stype pnum
        bind sock addr
        listen sock 1
        (conn, _) <- accept sock
        socketToHandle conn ReadWriteMode

    stopServer :: Handle -> IO ()
    stopServer = hClose

