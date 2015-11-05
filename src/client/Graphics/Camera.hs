{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
module Graphics.Camera(
    Camera(..)
  , CameraId(..)
  , HasCamera(..)
  , newCamera
  , emptyCamera
  , cameraLeft
  , cameraRight
  , cameraMatrix
  , cameraLookAt
  , cameraMove
  , cameraRotate
  , cameraRotateAround
  , cameraRotateAround'
  -- | Conversion helpers
  , cameraClippingF
  , cameraFovF
  ) where 

import Data.Vec as Vec
import GHC.Float 
import Math.Quaternion

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Hashable
import Util.Vec()
import TextShow
import TextShow.Generic

-- | Game camera for view transform of scene
data Camera = Camera {
  cameraId :: !CameraId
-- | Position of camera eye
, cameraEye :: !(Vec3 Double)
-- | Camera look direction
, cameraForward :: !(Vec3 Double)
-- | Camera up direction
, cameraUp :: !(Vec3 Double)
-- | Camera field of view in radians
, cameraFov :: !Double 
-- | Camera near and far clipping planes
, cameraClipping :: !(Double, Double)
} deriving (Show, Generic)

instance NFData Camera

instance TextShow Camera where 
  showbPrec = genericShowbPrec

newtype CameraId = CameraId { unCameraId :: Int } deriving (Eq, Show, Generic)

instance NFData CameraId
instance Hashable CameraId

instance TextShow CameraId where 
  showbPrec = genericShowbPrec

-- | Class that helps abstract out wrappers around camera (like Camera2D)
class HasCamera a where 
  -- | Unwraps a to Camera
  getCamera :: a -> Camera 

instance HasCamera Camera where 
  getCamera = id 
  
-- | Pretty looking field of view
defaultFov :: Double 
defaultFov = pi/3

-- | Pretty looking clipping planes
defaultClipping :: (Double, Double)
defaultClipping = (0.01, 100)

-- | Camera at origin with default view vectors
emptyCamera :: CameraId -> Camera 
emptyCamera i = Camera i (0:.0:.0:.()) (1:.0:.0:.()) (0:.1:.0:.()) defaultFov defaultClipping

-- | Wrapper for camera defining
-- Normalizes input vector (forward and up only)
newCamera :: CameraId -- ^ Camera id for messaging protocol
  -> Vec3 Double -- ^ Camera eye
  -> Vec3 Double -- ^ Camera forward
  -> Vec3 Double -- ^ Camera up
  -> Camera 
newCamera i eye forward up = Camera i eye (normalize forward) (normalize up) defaultFov defaultClipping

-- | Calculates camera left direction
cameraLeft :: Camera -> Vec3 Double 
cameraLeft (Camera{..}) = normalize $ cameraUp `cross` cameraForward

-- | Calculates camera right direction
cameraRight :: Camera -> Vec3 Double 
cameraRight = negate . cameraLeft 

-- | Makes camera to look at specified location
cameraLookAt :: Vec3 Double -- ^ Target
  -> Camera -- ^ Old camera
  -> Camera -- ^ New camera
cameraLookAt target cam@(Camera{..}) = cam { cameraForward = forward }
  where forward = normalize (target - cameraEye)

-- | Sets camera eye location
cameraMove :: Vec3 Double -- ^ New eye pos
  -> Camera -- ^ Old camera
  -> Camera -- ^ New camera
cameraMove eye' cam = cam { cameraEye = eye' }

-- | Calculates rotation matrix from camera
-- The matrix is used at renderer to calcluate MVP matrix (camera matrix is View component) 
cameraMatrix :: Camera -> Mat44 Float
cameraMatrix (Camera _ eye' forward' up' _ _) = rowX :. rowY :. rowZ :. rowW :. ()
  where
    xaxis = normalize $ up `cross` zaxis
    yaxis = normalize $ zaxis `cross` xaxis
    zaxis = negate $ forward
    rowX = xaxis `append` (-(xaxis `dot` eye):.())
    rowY = yaxis `append` (-(yaxis `dot` eye):.())
    rowZ = zaxis `append` (-(zaxis `dot` eye):.())
    rowW :: Vec4 Float
    rowW = 0.0 :. 0.0 :. 0.0 :. 1.0 :. ()

    eye, forward, up :: Vec3 Float
    eye = toFloatVec eye'
    forward = toFloatVec forward' 
    up = toFloatVec up'

    toFloatVec :: Vec3 Double -> Vec3 Float 
    toFloatVec = Vec.map double2Float

-- | Rotate camera forward vector with quaternion
cameraRotate :: Quaternion Double -> Camera -> Camera 
cameraRotate q c@(Camera{..}) = c { cameraForward = rotateVec q cameraForward }

-- | Rotate camera eye around position with quaternion
cameraRotateAround :: Quaternion Double -> Vec3 Double -> Camera -> Camera 
cameraRotateAround q p c@(Camera{..}) = c {
    cameraEye = p + r' 
  }
  where 
    r = cameraEye - p
    r' = rotateVec q r

-- | Rotate camera eye and forward vector around position
cameraRotateAround' :: Quaternion Double -> Vec3 Double -> Camera -> Camera 
cameraRotateAround' q p = cameraLookAt p . cameraRotateAround q p

-- | Float values of clipping
cameraClippingF :: Camera -> (Float, Float) 
cameraClippingF (Camera{..}) = let (n, f) = cameraClipping in (double2Float n, double2Float f)

-- | Float value for field of view angle
cameraFovF :: Camera -> Float 
cameraFovF (Camera{..}) = double2Float cameraFov