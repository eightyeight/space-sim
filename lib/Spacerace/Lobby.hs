{-# LANGUAGE DeriveGeneric #-}

module Spacerace.Lobby (
  teamInfo
, LobbyResponse(..)
) where

import Data.Eq (Eq)
import Data.String (String)
import Data.Typeable (Typeable)
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Prelude (Show)

teamInfo ::
  String
teamInfo =
  "{ \"name\": \"tony\", \"team\": \"=<<\" }"

data LobbyResponse =
  LobbyResponse {
    name :: String
  , game :: String
  , map :: String
  , secret :: String
  } deriving (Eq, Show, Generic, Typeable)

instance FromJSON LobbyResponse
