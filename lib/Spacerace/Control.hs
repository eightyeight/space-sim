module Spacerace.Control where

import Prelude

data Control =
  Control
    MainEngine
    Rotation
  deriving (Eq, Show)

data MainEngine = 
  MainEngineOff
  | MainEngineOn
  deriving (Eq, Show)

data Rotation =
  Clock
  | None
  | AntiClock
  deriving (Eq, Show)

class ToProtocol a where
    toProtocol :: a -> String

instance ToProtocol Rotation where
    toProtocol Clock = "-1"
    toProtocol None = "0"
    toProtocol AntiClock = "1"

instance ToProtocol MainEngine where
    toProtocol MainEngineOn = "1"
    toProtocol MainEngineOff = "0"

instance ToProtocol Control where
    toProtocol (Control e r) = toProtocol (e, r)

instance (ToProtocol a, ToProtocol b) => ToProtocol (a, b) where
    toProtocol (a, b) = toProtocol a ++ "," ++ toProtocol b

newtype Secret = Secret String

instance ToProtocol Secret where
    toProtocol (Secret t) = t
