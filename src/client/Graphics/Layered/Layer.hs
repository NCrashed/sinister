{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Layered.Layer(
    LayerRenderer
  , Layer(..)
  ) where 

import Data.Vec as Vec 
import Graphics.Light 
import Graphics.GPipe 
import Graphics.Camera2D

import GHC.Generics (Generic)
import Control.DeepSeq

-- | Function that renders layer with specified external parameters
type LayerRenderer = 
     Camera2D -- ^ View camera that is used for rendering
  -> Vec3 Float -- ^ World translation of layer
  -> Float -- ^ World rotation of layer
  -> Float -- ^ Depth level of the layer
  -> [Light Float] -- ^ List of dynamic lights
  -> Vec4 Float -- ^ Ambient color
  -> Vec2 Int -- ^ Size of viewport
  -- | Fragment shader with depth info and alpha channel
  -> FragmentStream (Color RGBAFormat (Fragment Float), FragmentDepth)

instance Show LayerRenderer where
  show = const "renderer"

-- | Hierarchy of textures that forms whole 2D model
data Layer = Layer {
-- | Unique name per model
  layerName :: !String
-- | Origin offset of texture
, layerOffset :: !(Vec2 Double)
-- | Rotation of texture
, layerRotation :: !Double
-- | Additional color 
, layerColor :: Vec4 Float
-- | Layer renderer
, layerRenderer :: LayerRenderer
-- | Layer childs that should be rendered after the parent
, layerChilds :: ![Layer]
} deriving (Show, Generic)

instance NFData Layer