{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, TypeFamilies, Arrows, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Server.Game.Player(
    PlayerMsg(..)
  , player
  , module P
  ) where 

import Core 

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Typeable

import Prelude hiding (id, (.))
import FRP.Netwire 

import Game.Player as P
import Game.World 
import Network.Protocol.Message
import Game.Boxed.Model
import Game.Boxed.Block 
import Server.Game.World.Event
import Server.Game.Boxed.Model
import Data.Vec as Vec 
import Util.Function
import Control.Wire.Unsafe.Event

data PlayerMsg = PlayerMsgStub
  deriving (Typeable, Generic)

instance NFData PlayerMsg

instance Messagable PlayerId where 
  type MessageType PlayerId = PlayerMsg 
  fromCounter = PlayerId 
  toCounter = unPlayerId

playerDebugPlaceBlock :: PlayerId -> GameWire World (Event [(BoxedModelId, Vec3 Int, Block, BlockMeta)])
playerDebugPlaceBlock pid = proc w -> do 
  filterJustE . onEventM (fmap sequence . mapM convert). worldPlayerNetMessages' isPlayerDebugPlaceBlock -< (w, pid)
  where 
    convert (PlayerDebugPlaceBlock mid (x,y,z) bname bmeta) = do 
      b <- getKnownBlockM bname
      return $ (BoxedModelId mid, x:.y:.z:.(), , bmeta) <$> b
    convert _ = error "playerDebugPlaceBlock: impossible message"


player :: Player -> GameActor PlayerId World Player
player startPlayer = makeIndexed simPlayer
  where
    startPlayer' pid = startPlayer { playerId = pid }

    simPlayer pid = loop $ proc (w, p) -> do 
      p1 <- processMessages processMessage pid . delay (startPlayer' pid) -< p

      -- Player requests world info, send respond
      worldRequest <- singlePlayerNetMessage isPlayerRequestWorld -< playerName p1
      sendNetMessage -< const (playerName p1, worldInfoMsg w) <$> worldRequest

      -- Player requests info about himself
      playerRequest <- singlePlayerNetMessage isPlayerRequestEnviroment -< playerName p1 
      sendNetMessage -< const (playerName p1, playerInfoMsg p1) <$> playerRequest
      sendNetMessage -< const (playerName p1, BoxedModelsAround . fmap unBoxedModelId $ 
        boxedModelsIdsAround w $ playerPos p1) <$> playerRequest

      -- DEBUG: Player wants to place blocks
      putMessagesE . mapE (fmap $ uncurry4 makePlaceBlockMsg) . playerDebugPlaceBlock pid -< w
      forceNF -< (p1, p1)

    processMessage p msg = case msg of 
      PlayerMsgStub -> p -- TODO

    makePlaceBlockMsg :: BoxedModelId -> Vec3 Int -> Block -> BlockMeta -> (BoxedModelId, BoxedModelMsg)
    makePlaceBlockMsg mid pos b bmeta = (mid, BoxedModelSetBlockAndMeta pos b bmeta)