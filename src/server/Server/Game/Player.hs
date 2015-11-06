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

data PlayerMsg = PlayerMsgStub
  deriving (Typeable, Generic)

instance NFData PlayerMsg

instance Messagable PlayerId where 
  type MessageType PlayerId = PlayerMsg 
  fromCounter = PlayerId 
  toCounter = unPlayerId

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

      forceNF -< (p1, p1)

    processMessage p msg = case msg of 
      PlayerMsgStub -> p -- TODO