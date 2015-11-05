{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, TypeFamilies, Arrows, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Client.Camera(
    CameraMsg(..)
  , camera
  , module C
  ) where 

import Core 

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Typeable 

import Prelude hiding (id, (.))
import FRP.Netwire 
import Control.Wire.Unsafe.Event 

import Graphics.Camera as C
import Data.Vec as Vec
import Math.Quaternion
import TextShow
import TextShow.Generic 

data CameraMsg = 
  -- | Setting view with pos, forward and up vectors
    CameraSetView !(Vec3 Double) !(Vec3 Double) !(Vec3 Double)
  -- | Setting camera target
  | CameraLookAt !(Vec3 Double) 
  -- | Setting field of view angle in raidans
  | CameraSetFOV !Double 
  -- | Setting near and far clipping planes
  | CameraSetClipping !Double !Double 
  deriving (Typeable, Generic)

instance NFData CameraMsg

instance TextShow CameraMsg where 
  showbPrec = genericShowbPrec

instance Messagable CameraId where 
  type MessageType CameraId = CameraMsg 
  fromCounter = CameraId 
  toCounter = unCameraId

-- | Constructs camera controller, uses specified camera as
-- inital state. Wire id is registered via new index API
camera :: Vec3 Double -- ^ Start position
  -> Vec3 Double -- ^ Start target
  -> Vec3 Double -- ^ Start up
  -> GameActor CameraId a Camera
camera startPos startTarget startUp = makeIndexed $ \cid -> loop $ proc (_, cam_) -> do 
  cam1 <- processMessages processMessage cid . delay (startCamera cid) -< cam_

  -- Mouse move rotates camera
  dp <- deltaMouseMove -< ()
  let cam2 = event cam1 (cameraRotateXY cam1) dp

  -- Event processing
  cam3 <- 
    -- Strafing
      keyReaction (CharKey 'w') cameraMoveForward
    . keyReaction (CharKey 's') cameraMoveBackward
    . keyReaction (CharKey 'a') cameraMoveLeft
    . keyReaction (CharKey 'd') cameraMoveRight

    -- Rotating
    . keyReaction (CharKey 'q') cameraRollLeft
    . keyReaction (CharKey 'e') cameraRollRight -< cam2

  forceNF -< (cam3, cam3)
  where
    startCamera cid = cameraLookAt startTarget $ newCamera cid startPos 0 startUp

    processMessage cam msg = case msg of 
      CameraSetView pos forw up -> cam {
          cameraEye = pos 
        , cameraForward = normalize forw 
        , cameraUp = normalize up 
        }
      CameraLookAt pos -> cameraLookAt pos cam
      CameraSetFOV angle -> cam {
          cameraFov = angle 
        }
      CameraSetClipping near far -> cam {
          cameraClipping = (near, far)
        }

    cameraSpeed :: Double
    cameraSpeed = 0.01

    rotationSpeed :: Double
    rotationSpeed = pi/100

    cameraRotateXY :: Camera -> Vec2 Double -> Camera 
    cameraRotateXY c (x:.y:.()) = cameraRotate (qx*qy) c
      where 
        qx = fromAxis (cameraUp c) (-rotationSpeed*x)
        qy = fromAxis (cameraLeft c) (rotationSpeed*y)
{-        
    mouseReaction :: MouseButton -> (a -> a) -> GameWire a a 
    mouseReaction mbtn f = proc cam -> do 
      e <- mouseClick mbtn Up -< ()
      returnA -< event cam (const $ f cam) e
-}
    keyReaction :: Key -> (a -> a) -> GameWire a a
    keyReaction key f = proc cam -> do 
      kwUp <- mapE (const Up) . keyEvent key Up -< ()
      kwDown <- mapE (const Down) . keyEvent key Down -< ()
      modes Up (\k -> case k of 
        Up -> id
        _ -> arr f) -< (cam, kwUp `mergeL` kwDown)

    scalev s = Vec.map (s*)

    updateEye :: (Vec3 Double -> Vec3 Double) -> Camera -> Camera 
    updateEye f cam@(Camera{..}) = cam { cameraEye = f cameraEye }

    cameraMoveForward :: Camera -> Camera 
    cameraMoveForward c@(Camera{..}) = updateEye (\e -> e + cameraSpeed `scalev` cameraForward) c

    cameraMoveBackward :: Camera -> Camera 
    cameraMoveBackward c@(Camera{..}) = updateEye (\e -> e - cameraSpeed `scalev` cameraForward) c

    cameraMoveLeft :: Camera -> Camera 
    cameraMoveLeft cam = updateEye (\e -> e + cameraSpeed `scalev` cameraLeft cam) cam

    cameraMoveRight :: Camera -> Camera 
    cameraMoveRight cam = updateEye (\e -> e - cameraSpeed `scalev` cameraLeft cam) cam

    cameraRoll :: Double -> Camera -> Camera 
    cameraRoll val cam@(Camera {..}) = cam { cameraUp = newUp }
      where 
        newUp = rotateVec q cameraUp
        q = fromAxis cameraForward val 

    cameraRollLeft = cameraRoll cameraSpeed
    cameraRollRight = cameraRoll (-cameraSpeed)