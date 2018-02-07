{-# LANGUAGE OverloadedStrings #-}

module Service where

import qualified Data.ByteString.Char8 as BS
import Safe


type PathSegment = BS.ByteString
type Responding = (BS.ByteString -> IO BS.ByteString)

data Method = GET | POST deriving (Eq, Show, Read)

data Request = Request
    { requestMethod :: Method
    , requestDest :: [PathSegment]
    }

data Responce = Responce
    { responceStatusCode :: Int
    , responceBody :: BS.ByteString
    }


statusDescription :: Int -> BS.ByteString
statusDescription 200 = "OK"


serve :: (Request -> Responce) -> Responding
serve func = return . maybe "" (rawResponce . func) . parseRequest


rawResponce :: Responce -> BS.ByteString
rawResponce (Responce code body) =
            "HTTP/1.1 " `BS.append` (BS.pack . show) code `BS.append` " " `BS.append` statusDescription code `BS.append` "\r\nConnection: Close\r\n\r\n" `BS.append` body


parseRequest :: BS.ByteString -> Maybe Request
parseRequest =
    fmap (buildRequest . BS.words) . headMay . BS.lines
    where
        buildRequest :: [BS.ByteString] -> Request
        buildRequest (method:target:_) = Request (read $ BS.unpack method) (pathSegment target)
            where
                pathSegment :: BS.ByteString -> [BS.ByteString]
                pathSegment = filter (not . BS.null) . BS.split '/'


echoOK :: Responding
echoOK request = return . (dummyHeader `BS.append`) $  BS.pack "\r\n" `BS.append` request
    where
        dummyHeader = BS.pack "\
            \HTTP/1.1 200 OK\r\n\
            \Connection: Close\r\n"


infLoop :: Responding
infLoop _ = return . BS.pack . show . or $ repeat False

