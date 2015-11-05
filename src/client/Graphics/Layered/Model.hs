{-# LANGUAGE DeriveGeneric #-}
module Graphics.Layered.Model(
    LayeredModel(..)
  ) where 

import Graphics.Layered.Layer
import Data.Vec as Vec 

import GHC.Generics (Generic)
import Control.DeepSeq

-- | Model that consists of several floors-layers
data LayeredModel = LayeredModel {
-- | Layered model name
  lmodelName :: !String
-- | Position of model in 3D space (Z is depth)
, lmodelPos :: !(Vec3 Double)
-- | Model rotation in XY plane
, lmodelRot :: !Double 
-- | List of model floors, each is a layer
, lmodelFloors :: ![Layer]
-- | Distance between floors (in Z axis)
, lmodelFloorHeight :: !Double
} deriving (Show, Generic)

instance NFData LayeredModel