{-# LANGUAGE DeriveGeneric #-}

module Spacerace.Lobby (
  LobbyRequest (..)
, LobbyResponse (..)
) where

import Data.Eq (Eq)
import Data.String (String)
import Data.Typeable (Typeable)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Prelude (Show)

data LobbyRequest =
  LobbyRequest {
    name :: String
  , team :: String
  } deriving (Eq, Show, Generic, Typeable)

data LobbyResponse =
  LobbyResponse {
    _name :: String -- conflicts with 'name' above :(
  , game :: String
  , map :: String
  , secret :: String
  } deriving (Eq, Show, Generic, Typeable)

instance ToJSON LobbyRequest
instance FromJSON LobbyResponse
