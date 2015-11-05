{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, TypeFamilies, Arrows, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Client.Player(
    ClientPlayer(..)
  , clientPlayerId
  , PlayerMsg(..)
  , player
  , playerFromMsg
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
import Client.Camera

import Data.Text (Text)

-- | Client specific extension of player
data ClientPlayer = ClientPlayer {
  clientPlayer :: !Player 
, clientPlayerCamera :: !Camera   
} deriving (Generic, Show)

instance NFData ClientPlayer

-- | Returns id of the player on client side
clientPlayerId :: ClientPlayer -> PlayerId 
clientPlayerId = playerId . clientPlayer

data PlayerMsg = PlayerMsgStub
  deriving (Typeable, Generic)

instance NFData PlayerMsg

instance Messagable PlayerId where 
  type MessageType PlayerId = PlayerMsg 
  fromCounter = PlayerId 
  toCounter = unPlayerId

-- | Constructs player from username and PlayerData message
-- Note: fails with error, if got other message
playerFromMsg :: Text -> NetworkMessage -> Player
playerFromMsg username (PlayerData i pos face up) = Player (PlayerId i) username pos face up
playerFromMsg _ _ = error "playerFromMsg: invalid message"

-- | Actor that reactimates player data retrieved from server
player :: Player -> GameActor PlayerId World ClientPlayer
player startPlayer = makeFixedIndexed (playerId startPlayer) simPlayer
  where
    simPlayer pid = loop $ proc (_, p_) -> do 
      p <- processMessages processMessage pid . delay startPlayer -< p_

      (cam, _) <- runIndexed $ camera startPos startFace startUp -< () 

      let p1 = ClientPlayer p cam
      forceNF -< (p1, p)

    startPos = playerPos startPlayer
    startFace = playerFace startPlayer
    startUp = playerUp startPlayer
    
    processMessage p msg = case msg of 
      PlayerMsgStub -> p -- TODO