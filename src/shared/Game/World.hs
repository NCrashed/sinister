{-# LANGUAGE DeriveGeneric, RecordWildCards, TypeSynonymInstances, FlexibleInstances #-}
module Game.World(
    World(..)
  , WorldId(..)
  , worldInfoMsg
  , WorldFindPlayer(..)
  ) where 

import Control.DeepSeq
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Game.Player
import GHC.Generics (Generic)
import Network.Protocol.Message
import qualified Data.HashMap.Strict as M

-- | World are completly isolated environments
data World = World {
    worldId :: !WorldId
  , worldName :: !Text
  , worldPlayers :: !(HashMap PlayerId Player)
  , worldPlayersByName :: !(HashMap Text Player)
} deriving (Generic)

instance NFData World

newtype WorldId = WorldId { unWorldId :: Int } deriving (Eq, Show, Generic)

instance NFData WorldId
instance Hashable WorldId

worldInfoMsg :: World -> NetworkMessage
worldInfoMsg (World{..}) = PlayerWorld (unWorldId worldId) worldName

class WorldFindPlayer k where 
  -- | Finds player by specific key
  worldFindPlayer :: World -> k -> Maybe Player

-- | You can search players by name
instance WorldFindPlayer Text where 
  worldFindPlayer w = (`M.lookup` worldPlayersByName w)

-- | And you can search players by theirs ids
instance WorldFindPlayer PlayerId where 
  worldFindPlayer w = (`M.lookup` worldPlayers w)