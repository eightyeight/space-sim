{-# LANGUAGE OverloadedStrings #-}

module Spacerace.Main (
  run
, Config (..)
, module S
) where

import Control.Monad
import Data.Aeson
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Prelude
import Spacerace.Control as S
import Spacerace.Lobby as S
import Spacerace.Map as S
import Spacerace.State as S
import System.ZMQ4.Monadic

data Config =
  Config
    String -- host
    Int -- lobby port
    Int -- control port
    Int -- state port
    FilePath -- maps
  deriving (Eq, Show)

type LobbySocket z = Socket z Req
type ControlSocket z = Socket z Push
type StateSocket z = Socket z Sub

run :: Config -> IO ()
run (Config host lobbyPort controlPort statePort mapDir) = runZMQ $ do
    let connString p = "tcp://" ++ host ++ ":" ++ show p
    lobbyS <- makeSocket Req (connString lobbyPort)
    stateS <- makeSocket Sub (connString statePort)
    controlS <- makeSocket Push (connString controlPort)
    forever $ joinAndPlay lobbyS stateS controlS mapDir

makeSocket :: SocketType t => t -> String -> ZMQ z (Socket z t)
makeSocket socketType connString = do
    sock <- socket socketType
    connect sock connString
    return sock

joinAndPlay :: LobbySocket z -> StateSocket z -> ControlSocket z -> FilePath -> ZMQ z ()
joinAndPlay lobbyS stateS controlS mapDir = do
    send lobbyS [] (L.toStrict $ encode $ LobbyRequest "=<< 1" "team haskell")
    reply <- receive lobbyS
    let lobbyResponse = decodeStrict reply :: Maybe LobbyResponse
    liftIO . print $ lobbyResponse    
    case lobbyResponse of 
        Nothing -> error "no/incorrect response from lobby"
        Just (LobbyResponse _ gameName mapName secret) -> do
            mapData <- liftIO $ loadMap mapDir mapName
            waitForGame (pack gameName) stateS
            gameLoop secret mapData stateS controlS

waitForGame :: B.ByteString -> StateSocket z -> ZMQ z ()
waitForGame gameName stateS = do
    [game, _state] <- receiveMulti stateS
    if gameName /= game
        then waitForGame gameName stateS
        else return ()

gameLoop :: String -> SpaceMap Double -> StateSocket z -> ControlSocket z -> ZMQ z ()
gameLoop secret mapData stateS controlS = do
    state <- getCurrentState stateS
    case state of
        FinishedState -> return ()
        RunningState ships -> do
            let command = toProtocol $ getCommand secret mapData ships
            liftIO $ print command
            send controlS [] (pack command)
            gameLoop secret mapData stateS controlS

getCurrentState :: StateSocket z -> ZMQ z GameState
getCurrentState stateS = do
    [_gameName, state] <- receiveMulti stateS
    case decode (L.fromChunks [state]) of
        Just s -> return s
        Nothing -> error "could not parse game state"

getCommand :: String -> SpaceMap Double -> [Ship] -> Control
getCommand secret _mapData _ships = Control secret MainEngineOn None
