{-# LANGUAGE RecordWildCards, DeriveGeneric, FlexibleInstances #-}
module Graphics.Model(
    ModelMesh(..)
  , Model(..)
  ) where 

import Graphics.Mesh
import Graphics.GPipe
import qualified Data.HashMap.Strict as M 
import Math.Quaternion
import GHC.Generics (Generic)
import Control.DeepSeq
import Util.Vec()
import Util.Texture()

import Data.Text (Text)
import TextShow
import TextShow.Generic

data ModelMesh = ModelMesh {
  modelMeshName :: Text
, modelMesh :: Mesh 
, modelMeshTexture :: Maybe (Texture2D RGBAFormat)
} deriving (Generic)

instance NFData ModelMesh 

data Model = Model {
  modelName :: Text 
, modelMeshes :: M.HashMap Text ModelMesh
, modelPos :: Vec3 Double
, modelRot :: Quaternion Double
} deriving (Generic)

instance NFData Model 

instance TextShow (M.HashMap Text ModelMesh) where 
  showb = showb . M.toList
  
instance TextShow Model where
  showbPrec = genericShowbPrec

instance TextShow ModelMesh where
  showb (ModelMesh{..}) = unwordsB ["ModelMesh("
    , "name =", showb modelMeshName
    , ", texture =", maybe "none" (const "set") modelMeshTexture
    , ", data =", showb modelMesh
    , ")"]