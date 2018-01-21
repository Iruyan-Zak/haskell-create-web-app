{-# LANGUAGE OverloadedStrings #-}

module Service where

import qualified Data.ByteString.Char8 as BS


type Responding = (BS.ByteString -> IO BS.ByteString)

echoOK :: Responding
echoOK request = return . (dummyHeader `BS.append`) $  BS.pack "\r\n" `BS.append` request
    where
        dummyHeader = BS.pack "\
            \HTTP/1.1 200 OK\r\n\
            \Connection: Close\r\n"


infLoop :: Responding
infLoop _ = return . BS.pack . show . or $ repeat False

