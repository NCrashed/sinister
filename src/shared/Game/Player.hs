{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
module Game.Player(
    Player(..)
  , PlayerId(..)
  , playerInfoMsg
  ) where 
  
import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Hashable
import Util.Vec()

import Network.Protocol.Message

import Data.Text (Text)

-- | Describing info attached to logged user
-- Equality is cheked by id equality.
data Player = Player {
  -- | Player identifier 
    playerId :: !PlayerId
  -- | Player displayed name
  , playerName :: !Text
} deriving (Generic, Show)

instance NFData Player

newtype PlayerId = PlayerId { unPlayerId :: Int } deriving (Eq, Show, Generic)

instance NFData PlayerId
instance Hashable PlayerId

-- | Converts player into network message
playerInfoMsg :: Player -> NetworkMessage
playerInfoMsg (Player{..}) = PlayerData (unPlayerId playerId)

-- | Equality by id
instance Eq Player where 
  p1 == p2 = playerId p1 == playerId p2 

-- | Hashing id
instance Hashable Player where 
  hashWithSalt s = hashWithSalt s . playerId