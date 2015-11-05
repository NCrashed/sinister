{-# LANGUAGE DeriveGeneric, RecordWildCards, TypeSynonymInstances, FlexibleInstances #-}
module Game.World(
    World(..)
  , WorldId(..)
  , worldInfoMsg
  , boxedModelsIds
  , boxedModelsIdsAround
  , WorldFindPlayer(..)
  , isModelInViewRange
  , worldPlayersAround
  , worldPlayersInViewRange
  , worldPlayersAroundModel
  ) where 

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M

import Network.Protocol.Message
import Game.Boxed.Model 
import Game.Player
import Data.Vec as Vec 

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Hashable
import Data.Maybe 
import Math.Vector 

import Data.Text (Text)

-- | Миры это скорее dimentions в mc
data World = World {
    worldId :: !WorldId
  , worldName :: !Text
  , worldModels :: !(HashMap BoxedModelId BoxedModel)
  , worldPlayers :: !(HashMap PlayerId Player)
  , worldPlayersByName :: !(HashMap Text Player)
  , worldViewRange :: !Double
} deriving (Generic)

instance NFData World

newtype WorldId = WorldId { unWorldId :: Int } deriving (Eq, Show, Generic)

instance NFData WorldId
instance Hashable WorldId

worldInfoMsg :: World -> NetworkMessage
worldInfoMsg (World{..}) = PlayerWorld (unWorldId worldId) worldName

boxedModelsIds :: World -> [BoxedModelId]
boxedModelsIds = M.keys . worldModels

boxedModelsIdsAround :: World -> Vec3 Double -> [BoxedModelId]
boxedModelsIdsAround (World{..}) pos = fmap modelId $ filter inRange $ M.elems worldModels
  where inRange = pointInRange pos worldViewRange . modelPos

class WorldFindPlayer k where 
  -- | Finds player by specific key
  worldFindPlayer :: World -> k -> Maybe Player

-- | You can search players by name
instance WorldFindPlayer Text where 
  worldFindPlayer w = (`M.lookup` worldPlayersByName w)

-- | And you can search players by theirs ids
instance WorldFindPlayer PlayerId where 
  worldFindPlayer w = (`M.lookup` worldPlayers w)

-- | For givent player and model check if boxed model in world view distance
isModelInViewRange :: PlayerId -> BoxedModelId -> World -> Bool 
isModelInViewRange pid mid (World{..}) = fromMaybe False $ do 
  playerPos <- playerPos <$> M.lookup pid worldPlayers
  modelPos <- modelPos <$> M.lookup mid worldModels
  return $! pointInRange playerPos worldViewRange modelPos

-- | Returns players ids around specified point in radius
worldPlayersAround :: Vec3 Double -> Double -> World -> [Player]
worldPlayersAround pos rad (World{..}) = filter inRange . M.elems $ worldPlayers
  where inRange = pointInRange pos rad . playerPos

-- | Returns players ids around specified point in world view range
worldPlayersInViewRange :: Vec3 Double -> World -> [Player]
worldPlayersInViewRange pos w = worldPlayersAround pos (worldViewRange w) w

-- | Returns players around boxed model in view range
worldPlayersAroundModel :: BoxedModelId -> World -> [Player]
worldPlayersAroundModel mid w = fromMaybe [] $ do 
  bm <- M.lookup mid $ worldModels w
  return $! worldPlayersInViewRange (modelPos bm) w