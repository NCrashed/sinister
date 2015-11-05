{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Camera2D(
    Camera2D(..)
  , newCamera2D
  , module Camera
  -- | Getters
  , camera2DPos
  , camera2DRot
  , camera2DZoom
  , camera2DUp
  , camera2DDown
  , camera2DLeft
  , camera2DRight
  -- | Setters
  , setCamera2DPos
  , setCamera2DRot
  , setCamera2DZoom
  -- | Updaters
  , updateCamera2DPos
  , updateCamera2DRot
  , updateCamera2DZoom
  ) where 

import Graphics.Camera as Camera
import Control.DeepSeq
import Math.Quaternion
import Math.Vector
import Data.Vec as Vec 

-- | 2D camera is wrapper around 3D camera that is looking always on XY plane
newtype Camera2D = Camera2D { unCamera2D :: Camera } deriving (Show, NFData)

instance HasCamera Camera2D where 
  getCamera = unCamera2D 

-- | Makes new 2D camera
newCamera2D :: CameraId -- ^ Id of camera object
  -> Vec2 Double -- ^ Camera position
  -> Double -- ^ Camera rotation
  -> Double -- ^ Camera zoom (distance from XY plane)
  -> Camera2D -- ^ Resulting camera 
newCamera2D cid pos2d rot2d depth = Camera2D $ newCamera cid pos3d zunit upv
  where 
    rot3d = fromAxis zunit rot2d
    upv = rotateVec rot3d yunit
    pos3d = Vec.snoc pos2d (negate depth)

-- | Returns XY coordinate of camera
camera2DPos :: Camera2D -> Vec2 Double 
camera2DPos (Camera2D cam) = Vec.take n2 $ cameraEye cam

-- | Returns rotation around Z axis of camera
camera2DRot :: Camera2D -> Double 
camera2DRot cam = angleBetweenVecsSigned yunit (camera2DUp cam)

-- | Returns camera Z coordinate over XY plane
camera2DZoom :: Camera2D -> Double 
camera2DZoom (Camera2D cam) = let (_:._:.z:.()) = cameraEye cam in negate z

-- | Updates camera position
setCamera2DPos :: Vec2 Double -> Camera2D -> Camera2D 
setCamera2DPos pos cam2d@(Camera2D cam) = Camera2D $ cameraMove (Vec.snoc pos z) cam
  where z = negate $ camera2DZoom cam2d

-- | Updates camera rotation around Z axis
setCamera2DRot :: Double -> Camera2D -> Camera2D 
setCamera2DRot rot2d (Camera2D cam) = Camera2D $ cam { cameraUp = upv } 
  where 
    rot3d = fromAxis zunit rot2d
    upv = rotateVec rot3d yunit

-- | Updates camera Z coordinate over XY plane
setCamera2DZoom :: Double -> Camera2D -> Camera2D 
setCamera2DZoom z (Camera2D cam) = Camera2D $ cam { cameraEye = (x:.y:.(negate z):.()) }
  where (x:.y:._:.()) = cameraEye cam

-- | Updates camera XY coordinate with function
updateCamera2DPos :: (Vec2 Double -> Vec2 Double) -> Camera2D -> Camera2D 
updateCamera2DPos f cam = flip setCamera2DPos cam $ f $ camera2DPos cam

-- | Updates camera rotation around Z axis with function
updateCamera2DRot :: (Double -> Double) -> Camera2D -> Camera2D 
updateCamera2DRot f cam = flip setCamera2DRot cam $ f $ camera2DRot cam

-- | Updates camera distance to XY plane with function
updateCamera2DZoom :: (Double -> Double) -> Camera2D -> Camera2D 
updateCamera2DZoom f cam = flip setCamera2DZoom cam $ f $ camera2DZoom cam

-- | Retuns camera up direction
camera2DUp :: Camera2D -> Vec2 Double
camera2DUp (Camera2D cam) = Vec.normalize . Vec.take n2 $ cameraUp cam 

-- | Returns camera down direction
camera2DDown :: Camera2D -> Vec2 Double 
camera2DDown = negate . camera2DUp 

-- | Returns camera left direction
camera2DLeft :: Camera2D -> Vec2 Double
camera2DLeft (Camera2D cam) = Vec.normalize . Vec.take n2 $ cameraLeft cam 

-- | Returns camera right direction
camera2DRight :: Camera2D -> Vec2 Double
camera2DRight = negate. camera2DLeft