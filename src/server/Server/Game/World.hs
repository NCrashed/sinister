{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, TypeFamilies, Arrows, BangPatterns, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Server.Game.World(
    WorldMsg(..)
  , world
  , module W
  ) where

import Core 
import Control.Wire.Collection

import qualified Data.HashMap.Strict as M 

import Control.DeepSeq hiding (force)
import Data.Maybe 
import Data.Text (Text)
import Data.Typeable 
import FRP.Netwire 
import Game.World as W
import GHC.Generics (Generic)
import Prelude as P hiding (id, (.))
import qualified Data.Text as T
import Server.Game.Player 
import TextShow

data ServerWorld = ServerWorld {
  serverWorld :: !World 
, worldAddPlayers :: ![Player]
, worldRemovePlayers :: ![Text]
} deriving (Generic)

instance NFData ServerWorld 

data WorldMsg = 
    SpawnPlayer !Player
  | DespawnPlayer !Text
  deriving (Typeable, Generic)

instance NFData WorldMsg

instance Messagable WorldId where 
  type MessageType WorldId = WorldMsg 
  fromCounter = WorldId 
  toCounter = unWorldId

-- | World controller-actor
world :: Text -- ^ World name
  -> GameActor WorldId a World
world name = makeIndexed simWorld
  where 
    startServerWorld :: WorldId -> ServerWorld
    startServerWorld wid = ServerWorld (startWorld wid) [] []

    startWorld :: WorldId -> World
    startWorld wid = World wid name M.empty M.empty

    simWorld wid = loop $ proc (_, w) -> do 
      w1 <- processMessages processMessage wid . delay (startServerWorld wid) -< w

      -- DEBUG
      logInfoE . mapE (\ss -> T.unlines $ fmap ("Player connected: " <>) ss ) . playersConnected -< ()
      logInfoE . mapE (\ss -> T.unlines $ fmap ("Player disconnected: " <>) ss ) . playersDisconnected -< ()
      logInfoE . mapE showt . playerNetMessages -< "NCrashed"
      -- DEBUG

      w2 <- processPlayersDyn -< w1

      forceNF -< (serverWorld w2, w2)

    processMessage sw msg = case msg of 
      SpawnPlayer p -> sw { worldAddPlayers = p : worldAddPlayers sw } 
      DespawnPlayer p -> sw { worldRemovePlayers = p : worldRemovePlayers sw}

    processPlayersDyn :: GameWire ServerWorld ServerWorld
    processPlayersDyn = proc sw -> do 
      -- spawnEvent :: Event (GameMonad (GameWireI PlayerId World Player))
      spawnEvent <- 
          mapE (fmap player . worldAddPlayers)
        . became (not . null . worldAddPlayers) -< sw 

      -- despawnEvent :: Event [PlayerId]
      despawnEvent <- 
          mapE (\w -> 
            fmap playerId $ 
            catMaybes $ 
            worldFindPlayer (serverWorld w) <$> worldRemovePlayers w)
        . became (not . null . worldRemovePlayers) -< sw 

      ps <- dynCollection [] -< (serverWorld sw, spawnEvent, despawnEvent)

      forceNF -< sw { 
          serverWorld = (serverWorld sw) {
            worldPlayers = M.fromList $ fmap playerId ps `zip` ps
          , worldPlayersByName = M.fromList $! fmap playerName ps `zip` ps
          }
        , worldAddPlayers = []
        , worldRemovePlayers = []
        }