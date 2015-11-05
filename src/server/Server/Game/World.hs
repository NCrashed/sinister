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

import GHC.Generics (Generic)
import Control.DeepSeq hiding (force)
import Data.Typeable 

import Prelude as P hiding (id, (.))
import FRP.Netwire 

import Game.World as W
import Server.Game.Player 
import Data.Maybe 

import Data.Vec as V
import Server.Game.Boxed.Model
import Game.Boxed.Block
import Util.Function

import qualified Data.Text as T
import Data.Text (Text)
import TextShow

data ServerWorld = ServerWorld {
  serverWorld :: !World 
, worldAddPlayers :: ![Player]
, worldRemovePlayers :: ![Text]
, worldAddBoxedModel :: ![(Vec3 Double, Double, Block)]
} deriving (Generic)

instance NFData ServerWorld 

data WorldMsg = 
    SpawnPlayer !Player
  | DespawnPlayer !Text
  -- | Create model with single block at pos and rotation
  | CreateBoxedModel !(Vec3 Double) !Double !Block
  deriving (Typeable, Generic)

instance NFData WorldMsg

instance Messagable WorldId where 
  type MessageType WorldId = WorldMsg 
  fromCounter = WorldId 
  toCounter = unWorldId

-- | World controller-actor
world :: Text -- ^ World name
  -> Double -- ^ View distance
  -> GameActor WorldId a World
world name viewDist = makeIndexed simWorld
  where 
    startServerWorld :: WorldId -> ServerWorld
    startServerWorld wid = ServerWorld (startWorld wid) [] [] []

    startWorld :: WorldId -> World
    startWorld wid = World wid name M.empty M.empty M.empty viewDist

    simWorld wid = loop $ proc (_, w) -> do 
      w1 <- processMessages processMessage wid . delay (startServerWorld wid) -< w

      -- DEBUG
      logInfoE . mapE (\ss -> T.unlines $ fmap ("Player connected: " <>) ss ) . playersConnected -< ()
      logInfoE . mapE (\ss -> T.unlines $ fmap ("Player disconnected: " <>) ss ) . playersDisconnected -< ()
      logInfoE . mapE showt . playerNetMessages -< "NCrashed"

      let modelIds = M.keys . worldModels . serverWorld $ w1
      putMessageE . mapE (const (wid, CreateBoxedModel 0 0 floorBlock)) . now -< ()
      sendModelSetBlock . mapE (\ids -> (P.head ids, 0:.1:.0:.(), floorBlock)) . at 2 -< modelIds
      sendModelSetBlock . mapE (\ids -> (P.head ids, 1:.0:.0:.(), floorBlock)) . at 2 -< modelIds
      sendModelSetBlock . mapE (\ids -> (P.head ids, 1:.1:.0:.(), floorBlock)) . at 2 -< modelIds

      sendModelSetBlock . mapE (\ids -> (P.head ids, 0:.2:.0:.(), floorBlock)) . at 2 -< modelIds
      sendModelSetBlock . mapE (\ids -> (P.head ids, 2:.0:.0:.(), floorBlock)) . at 2 -< modelIds
      sendModelSetBlock . mapE (\ids -> (P.head ids, 2:.2:.0:.(), floorBlock)) . at 2 -< modelIds

      sendModelSetMeta . mapE (\ids -> (P.head ids, 0:.1:.0:.(), floorBlockCovered)) . at 3 -< modelIds
      sendModelSetMeta . mapE (\ids -> (P.head ids, 1:.0:.0:.(), floorBlockCovered)) . at 3 -< modelIds
      sendModelSetMeta . mapE (\ids -> (P.head ids, 1:.1:.0:.(), floorBlockCovered)) . at 3 -< modelIds
      sendModelSetMeta . mapE (\ids -> (P.head ids, 2:.0:.0:.(), floorBlockCovered)) . at 3 -< modelIds

      sendModelSetBlock . mapE (\ids -> (P.head ids, 0:.(-1):.0:.(), floorBlock)) . at 2 -< modelIds
      sendModelSetBlock . mapE (\ids -> (P.head ids, (-1):.0:.0:.(), floorBlock)) . at 2 -< modelIds
      sendModelSetBlock . mapE (\ids -> (P.head ids, (-1):.(-1):.0:.(), floorBlock)) . at 2 -< modelIds

      --sendModelSetMeta . mapE (\ids -> (P.head ids, 0:.1:.0:.(), floorBlockCovered)) . notYet . periodic 4 -< modelIds
      --sendModelSetMeta . mapE (\ids -> (P.head ids, 0:.1:.0:.(), floorBlockUncovered)) . notYet . periodic 5 -< modelIds
      -- DEBUG

      w2 <- processBoxedModelsDyn . processPlayersDyn -< w1

      forceNF -< (serverWorld w2, w2)

    processMessage sw msg = case msg of 
      SpawnPlayer p -> sw { worldAddPlayers = p : worldAddPlayers sw } 
      DespawnPlayer p -> sw { worldRemovePlayers = p : worldRemovePlayers sw} 
      CreateBoxedModel pos rot bl -> 
        sw { worldAddBoxedModel = (pos, rot, bl) : worldAddBoxedModel sw }

    processBoxedModelsDyn :: GameWire ServerWorld ServerWorld 
    processBoxedModelsDyn = proc sw -> do 
      -- spawnEvent :: Event (GameMonad (GameWireI BoxedModelId World BoxedModel))
      spawnEvent <- 
          mapE (fmap (uncurry3 boxedModel) . worldAddBoxedModel)
        . became (not . null . worldAddBoxedModel) -< sw

      -- despawnEvent :: Event [BoxedModelId]
      despawnEvent <- never -< ()

      bs <- dynCollection [] -< (serverWorld sw, spawnEvent, despawnEvent)

      forceNF -< sw { 
        serverWorld = (serverWorld sw) {
          worldModels = M.fromList $ fmap modelId bs `zip` bs
        }
      , worldAddBoxedModel = []
      }

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