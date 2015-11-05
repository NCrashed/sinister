{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
module Game.Player(
    Player(..)
  , PlayerId(..)
  , playerInfoMsg
  ) where 

import Data.Vec as Vec

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
  , playerName :: !Text
  -- | Player current position
  , playerPos :: !(Vec3 Double)
  -- | Player current facing
  , playerFace :: !(Vec3 Double)
  -- | Player current up vector
  , playerUp :: !(Vec3 Double)
} deriving (Generic, Show)

instance NFData Player

newtype PlayerId = PlayerId { unPlayerId :: Int } deriving (Eq, Show, Generic)

instance NFData PlayerId
instance Hashable PlayerId

-- | Converts player into network message
playerInfoMsg :: Player -> NetworkMessage
playerInfoMsg (Player{..}) = PlayerData (unPlayerId playerId) playerPos playerFace playerUp

-- | Equality by id
instance Eq Player where 
  p1 == p2 = playerId p1 == playerId p2 

-- | Hashing id
instance Hashable Player where 
  hashWithSalt s = hashWithSalt s . playerId