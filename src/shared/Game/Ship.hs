{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Game.Ship(
    Ship(..)
  , ShipId 
  ) where 

import qualified Data.Text as T
import Game.Boxed.Model 

import Game.Ship.Mask 

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Hashable

-- | Ship is 
data Ship = Ship {
  shipId :: !ShipId 
  -- | User defined name of the ship
, shipName :: !T.Text 
  -- | Ship type, type defines appeareance of ship hull
, shipType :: !T.Text 
  -- | Internal boxed model for floors
, shipInterior :: !BoxedModel 
  -- | Mask that restricts interior boxed model
, shipMask :: !ShipMask
} deriving (Generic)

instance NFData Ship 

newtype ShipId = ShipId { unShipId :: Int } deriving (Eq, Show, Generic)

instance NFData ShipId
instance Hashable ShipId

newShip :: ShipId -> BoxedModelId -> Ship 
newShip sId bmId = Ship {
  shipId = sId
, shipName = ""
, shipType = ""
, shipInterior = emptyModel bmId 
, shipMask = newShipMask 0 0
}