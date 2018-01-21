module Main where

import TcpServer
import Service

main :: IO ()
main = runServer "127.0.0.1" "5000" echoOK
