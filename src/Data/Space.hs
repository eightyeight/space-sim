{-# LANGUAGE OverloadedStrings #-}

module Data.Space where

import Prelude
import System.ZMQ4.Monadic
import Data.ByteString.Char8(pack, unpack)
import Data.Aeson
import Data.Space.Lobby
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

main :: IO ()
main =
    runZMQ $ do
        s <- socket Req
        connect s "tcp://10.0.0.236:5558"
        send s [] (pack lobby)
        reply <- receive s
        let zz = decode (L.fromChunks [reply]) :: Maybe LobbyResponse
        liftIO . print $ zz    
