{-# LANGUAGE RecordWildCards #-}
module Graphics.Render.Model(
    modelFrameBuffer
  , modelFragmentStream
  ) where 

import Graphics.Model 
import Graphics.GPipe
import Graphics.Camera 
import Graphics.Render.Common 
import Graphics.Render.Mesh
import qualified Data.HashMap.Strict as M 
import Data.Maybe 
import GHC.Float 
import Data.Vec as Vec 
import Math.Quaternion

modelFrameBuffer :: Camera -- ^ Camera of player to render from
  -> Model -- ^ Model for rendering
  -> Texture2D RGBAFormat -- ^ Default texture for meshes that doesn't have own
  -> Vec2 Int -- ^ Window size
  -> FrameBuffer RGBAFormat DepthFormat () -- ^ Framebuffer with alpha channel and depth info, no stencil buffer
modelFrameBuffer cam model defaultTexture size = paintSolidDepthAlpha litModel emptyFrameBufferDepthAlpha
  where litModel = modelFragmentStream cam model defaultTexture size

modelFragmentStream :: Camera -- ^ Camera of player to render from
  -> Model -- ^ Model for rendering
  -> Texture2D RGBAFormat -- ^ Default texture for meshes that doesn't have own
  -> Vec2 Int -- ^ Window size
  -- | Stream of fragments with color and depth info
  -> FragmentStream (Color RGBAFormat (Fragment Float), FragmentDepth) 
modelFragmentStream cam (Model{..}) defaultTexture size = mconcat $ renderMesh <$> M.elems modelMeshes 
  where 
    renderMesh (ModelMesh{..}) = meshFragmentStream cam tex pos rot modelMesh size
      where 
        tex = fromMaybe defaultTexture modelMeshTexture
        pos = Vec.map double2Float modelPos
        rot = let Quaternion v = modelRot in Quaternion (Vec.map double2Float v)
