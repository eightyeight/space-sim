{-# LANGUAGE OverloadedStrings #-}

module Spacerace.Main (
  run
, Config (..)
, module S
) where

import Spacerace.Control as S
import Spacerace.Map as S
import Spacerace.State as S
import Spacerace.Lobby as S
import Spacerace.State as S
import Prelude
import System.ZMQ4.Monadic
import Data.ByteString.Char8(pack)
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Control.Monad

data Config =
  Config
    String -- host
    Int -- request port
    Int -- sub port
    Int -- push port
    FilePath -- maps
  deriving (Eq, Show)

run :: Config -> IO ()
run (Config host lobbyPort statePort controlPort mapDir) = runZMQ $ do
    let connString p = "tcp://" ++ host ++ ":" ++ show p
    lobbyS <- makeSocket Req (connString lobbyPort)
    stateS <- makeSocket Sub (connString statePort)
    controlS <- makeSocket Push (connString controlPort)
    forever $ joinAndPlay lobbyS stateS controlS

makeSocket :: SocketType t => t -> String -> ZMQ z (Socket z t)
makeSocket socketType connString = do
    sock <- socket socketType
    connect sock connString
    return sock

joinAndPlay :: Socket z Req -> Socket z Sub -> Socket z Push -> ZMQ z ()
joinAndPlay lobbyS stateS controlS = do
    send lobbyS [] (pack S.teamInfo)
    reply <- receive lobbyS
    let lobbyResponse = decode (L.fromChunks [reply]) :: Maybe LobbyResponse
    liftIO . print $ lobbyResponse    
    case lobbyResponse of 
        Nothing -> error "no/incorrect response from lobby"
        Just (LobbyResponse _ gameName map secret) -> do
            -- load map
            mapData <- liftIO $ loadMap "/home/tmorris/Desktop/r/spacerace/maps" "swish"
            -- Wait for the game to begin
            waitForGame (pack gameName) stateS
            gameLoop secret mapData stateS controlS

waitForGame :: B.ByteString -> Socket z Sub -> ZMQ z ()
waitForGame gameName stateS = do
    [game, _state] <- receiveMulti stateS
    if gameName /= game
        then waitForGame gameName stateS
        else return ()

gameLoop :: String -> SpaceMap Double -> Socket z Sub -> Socket z Push -> ZMQ z ()
gameLoop secret mapData stateS controlS = do
    liftIO $ print "hi"
    state <- getCurrentState stateS
    case state of
        FinishedState -> return ()
        RunningState ships -> do
            let command = toProtocol $ getCommand secret mapData ships
            liftIO $ print command
            send controlS [] (pack command)
            gameLoop secret mapData stateS controlS

getCurrentState :: Socket z Sub -> ZMQ z GameState
getCurrentState stateS = do
    [_gameName, state] <- receiveMulti stateS
    case decode (L.fromChunks [state]) of
        Just s -> return s
        Nothing -> error "could not parse game state"

getCommand :: String -> SpaceMap Double -> [Ship] -> Control
getCommand secret mapData ships = Control secret MainEngineOn None

controlexample ::
  String
  -> Control
controlexample k =
  Control k MainEngineOn AntiClock
