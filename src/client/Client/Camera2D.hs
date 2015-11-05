{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, TypeFamilies, Arrows, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Client.Camera2D(
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

import Graphics.Camera2D as C
import Data.Vec as Vec

data CameraMsg = 
    -- | Setting view with pos rotation and zoom
    CameraSetView !(Vec2 Double) !Double !Double 
  deriving (Typeable, Generic)

instance NFData CameraMsg

instance Messagable CameraId where 
  type MessageType CameraId = CameraMsg 
  fromCounter = CameraId 
  toCounter = unCameraId

-- | Constructs camera controller, uses specified camera as
-- inital state. Wire id is registered via new index API
camera :: Vec2 Double -- ^ Start position
  -> Double -- ^ Start rotation
  -> Double -- ^ Start zoom
  -> GameActor CameraId a Camera2D
camera startPos startRot startZoom = makeIndexed $ \cid -> loop $ proc (_, cam_) -> do 
  cam1 <- processMessages processMessage cid . delay (startCamera cid) -< cam_

  -- Event processing
  cam2 <- 
    -- Zooming
      mouseReaction WheelUp cameraZoomIn
    . mouseReaction WheelDown cameraZoomOut

    -- Strafing
    . keyReaction (CharKey 'w') cameraMoveForward
    . keyReaction (CharKey 's') cameraMoveBackward
    . keyReaction (CharKey 'a') cameraMoveLeft
    . keyReaction (CharKey 'd') cameraMoveRight

    -- Rotating
    . keyReaction (CharKey 'q') cameraRotateLeft
    . keyReaction (CharKey 'e') cameraRotateRight -< cam1

  forceNF -< (cam2, cam2)
  where
    startCamera cid = newCamera2D cid startPos startRot startZoom

    processMessage cam msg = case msg of 
      CameraSetView pos rot zoom -> 
          setCamera2DZoom zoom 
        . setCamera2DRot rot 
        . setCamera2DPos pos $ cam  

    cameraSpeed :: Double
    cameraSpeed = 0.01

    rotationSpeed :: Double
    rotationSpeed = pi/60

    mouseReaction :: MouseButton -> (a -> a) -> GameWire a a 
    mouseReaction mbtn f = proc cam -> do 
      e <- mouseClick mbtn Up -< ()
      returnA -< event cam (const $ f cam) e

    keyReaction :: Key -> (a -> a) -> GameWire a a
    keyReaction key f = proc cam -> do 
      kwUp <- mapE (const Up) . keyEvent key Up -< ()
      kwDown <- mapE (const Down) . keyEvent key Down -< ()
      modes Up (\k -> case k of 
        Down -> arr f
        Up -> id) -< (cam, kwUp `mergeL` kwDown)

    scalev s = Vec.map (s*)

    cameraMoveForward :: Camera2D -> Camera2D 
    cameraMoveForward c = updateCamera2DPos (+ (cameraSpeed `scalev` camera2DUp c)) c

    cameraMoveBackward :: Camera2D -> Camera2D 
    cameraMoveBackward c = updateCamera2DPos (\v -> v - (cameraSpeed `scalev` camera2DUp c)) c

    cameraMoveLeft :: Camera2D -> Camera2D 
    cameraMoveLeft c = updateCamera2DPos (+ (cameraSpeed `scalev` camera2DLeft c)) c

    cameraMoveRight :: Camera2D -> Camera2D 
    cameraMoveRight c = updateCamera2DPos (\v -> v - (cameraSpeed `scalev` camera2DLeft c)) c

    cameraRotate2D :: Double -> Camera2D -> Camera2D 
    cameraRotate2D val = updateCamera2DRot (+ val)

    cameraRotateLeft = cameraRotate2D rotationSpeed
    cameraRotateRight = cameraRotate2D (-rotationSpeed)

    cameraZoomStep :: Double
    cameraZoomStep = 0.1

    cameraZoomOut :: Camera2D -> Camera2D 
    cameraZoomOut = updateCamera2DZoom (+cameraZoomStep)

    cameraZoomIn :: Camera2D -> Camera2D 
    cameraZoomIn = updateCamera2DZoom ((max 0.2).(\v->v-cameraZoomStep))