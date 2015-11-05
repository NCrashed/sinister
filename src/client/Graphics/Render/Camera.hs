module Graphics.Render.Camera(
    cameraTransform
  , cameraTransform2D
  , vpMatrix
  ) where 

import Graphics.GPipe
import Graphics.Camera 
import Graphics.Camera2D

import Data.Vec as Vec
import Math.Quaternion

-- | Transforms one vertex, normal, uv triplet from model space to OpenGL render space
cameraTransform :: Camera -- ^ View camera
  -> Vec3 Float -- ^ World translation
  -> Quaternion Float  -- ^ World rotation
  -> Vec2 Int -- ^ Viewport size
  -- | Input vertex coordinates on GPU side
  -> Vec3 (Vertex Float)
  -- | Input normal coordinates on GPU side
  -> Vec3 (Vertex Float)
  -- | Output pairs chain of vertex, normal coordinates in GPU side
  -> (Vec4 (Vertex Float), Vec3 (Vertex Float))
cameraTransform cam mpos rot size pos normv = (newPos, newNorm)
  where 
    modelMat = toRotationMatrix rot `multmm` translation mpos
    viewProjMat = vpMatrix cam size
    viewProjModelMat = viewProjMat `multmm` modelMat
    newPos  = toGPU viewProjModelMat `multmv` (homPoint pos :: Vec4 (Vertex Float))
    newNorm = toGPU (mat4ToMat3 modelMat) `multmv` normv
    mat4ToMat3 = Vec.map (Vec.take n3) . Vec.take n3

-- | Transforms one vertex and uv from model space to OpenGL render space, using XY plane for 2D
-- projection. Z axis is depth axis.
cameraTransform2D :: Camera2D -- ^ Camera view, rotation only around Z axis
  -> Vec2 Float -- ^ World translation (XY plane)
  -> Float -- ^ World rotation around Z axis
  -> Vec2 Int -- ^ Viewport size 
  -- | Input vertex
  -> Vec3 (Vertex Float)
  -- | Output vertex
  -> Vec4 (Vertex Float)
cameraTransform2D (Camera2D cam) trans2d rot2d size pos = newPos
  where 
    trans3d = Vec.snoc trans2d 0
    rot3d = fromAxis (0:.0:.1:.()) rot2d 
    modelMat = toRotationMatrix rot3d `multmm` translation trans3d
    viewProjMat = vpMatrix cam size
    viewProjModelMat = viewProjMat `multmm` modelMat
    newPos  = toGPU viewProjModelMat `multmv` (homPoint pos :: Vec4 (Vertex Float))

-- | Calculates view-projection matrix (maps world coordinates to window coordinates)
vpMatrix :: Camera -- ^ Camera view
  -> Vec2 Int -- ^ Viewport size
  -> Mat44 Float -- ^ Resulting 4x4 matrix
vpMatrix cam (width:.height:.()) = projMat `multmm` viewMat
  where 
    (near,far) = cameraClippingF cam 
    fov = cameraFovF cam
    viewMat  = cameraMatrix cam 
    projMat  = perspective near far fov (fromIntegral width / fromIntegral height)