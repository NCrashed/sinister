{-# LANGUAGE TupleSections #-}
module Graphics.Render.Quad(
    quadWithNormal
  , quadNoShading
  ) where 

import Graphics.GPipe
import Graphics.Quad 
import Graphics.Camera2D
import Graphics.Light
import Graphics.Render.Camera 
import Graphics.Render.Light
import Data.Vec as Vec
import Control.Applicative
import Data.Maybe (fromMaybe)

-- | Simple rendering of quad with diffuse texture
quadNoShading :: Camera2D -- ^ Camera
  -- | Diffuse texture
  -> Texture2D RGBAFormat
  -- | Ambient color, alpha is intensity
  -> Vec4 Float 
  -- | Color modifier, alpha is intensity
  -> Vec4 Float
  -- | Translation in world
  -> Vec2 Float
  -- | Rotation in world
  -> Float
  -- | Depth (Z coordinate)
  -> Float
  -- | Size of viewport
  -> Vec2 Int 
  -- | Fragments
  -> FragmentStream (Color RGBAFormat (Fragment Float), FragmentDepth)
quadNoShading cam2d tex ambient clr trans2d rot2d depth size = 
  storeDepth . texturise <$> rasterizeFront (transform <$> screenQuad depth)
  where 
    storeDepth = (, fragDepth)
    transform = cameraTransform2D cam2d trans2d rot2d size
    texturise = enlightSimple tex ambient clr

-- | Render 2D quad with diffuse and normal map
quadWithNormal :: Camera2D -- ^ Camera
  -- | Diffuse texture
  -> Texture2D RGBAFormat
  -- | Normal texture
  -> Texture2D RGBAFormat
  -- | Ambient color, alpha is entensity
  -> Vec4 Float 
  -- | Color modifier, alpha is intensity
  -> Vec4 Float
  -- | Lights that is used for enlighting
  -> [Light Float]
  -- | Translation in world
  -> Vec2 Float
  -- | Rotation in world
  -> Float
  -- | Depth (Z coordinate)
  -> Float
  -- | Size of viewport
  -> Vec2 Int 
  -- | Fragments
  -> FragmentStream (Color RGBAFormat (Fragment Float), FragmentDepth)
quadWithNormal cam2d@(Camera2D cam) tex ntex ambient clr lights trans2d rot2d depth size = 
  storeDepth . texturise <$> rasterizeFront (transform <$> screenQuad depth)
  where 
    storeDepth = (, fragDepth)
    vpMat = vpMatrix cam size
    vpMatInv = fromMaybe Vec.identity . Vec.invert $ vpMat
    transform = cameraTransform2D cam2d trans2d rot2d size
    texturise = enlightNormal size tex ntex ambient clr lights vpMatInv