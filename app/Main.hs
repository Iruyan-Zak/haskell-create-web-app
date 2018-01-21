{-# LANGUAGE OverloadedStrings #-}
module Main where

import TcpServer
import Service
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = runServer "127.0.0.1" "5000" $ serve route


route :: Request -> Responce
route (Request _ []) = (Responce 200 "root dir")
route (Request GET ["user", userName]) = (Responce 200 (BS.append "You are " userName))
route (Request _ segments) = (Responce 200 (BS.append "You accessed " $ BS.intercalate "/" segments))

