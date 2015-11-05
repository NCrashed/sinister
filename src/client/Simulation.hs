{-# LANGUAGE Arrows, DeriveGeneric, RecordWildCards, TupleSections #-}
module Simulation(
    cubeRenderer
  ) where 

import Prelude hiding (id, (.))
import FRP.Netwire 

import Graphics.Core
import Graphics.Render.Common
import Graphics.GPipe
--import Graphics.Light

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar (TVar)
import Core
import Data.IORef 
import Network.Protocol.Message
import GHC.Generics (Generic)
import Control.DeepSeq

import Client.World
import Client.Player

import Math.Vector 
import qualified Data.Foldable as F 
--import Graphics.Render.Boxed.Model
import Client.Event.Camera
import Data.Vec as Vec 
import Client.Boxed.Model
import Game.Boxed.Block 
import Assets.Texture 
import Control.Wire.Unsafe.Event (event)
--import Game.Ship 

import Data.Text (Text)
import TextShow 

import Assets.ObjMesh
import Graphics.Render.Model

-- | Composable fragment streams that are bleated to screen
type SceneFragmentStream = FragmentStream (Color RGBAFormat (Fragment Float), FragmentDepth)

-- | All info needed to render game to user screen
data Scene = Scene {
  -- | Main layer of scene (ship, background, effects)
  sceneObjects :: Vec2 Int -> [SceneFragmentStream]
  -- | Overlay over sceneObjects, usually GUI
, sceneGui :: Vec2 Int -> [SceneFragmentStream]
} deriving (Generic)

emptyScene :: Scene 
emptyScene = Scene mempty mempty

renderScene :: Scene -> Vec2 Int -> FrameBuffer RGBAFormat DepthFormat ()
renderScene (Scene{..}) size = F.foldl' paint emptyFrameBufferDepthAlpha finalStream
  where 
    finalStream = sceneObjects size ++ sceneGui size 
    paint acc obj = paintSolidDepthAlpha obj acc 

instance NFData Scene 

cubeRenderer :: TChan NetworkMessage -- ^ Channel with messages to send to server
  -> TChan NetworkMessage -- ^ Channel with received network messages from server
  -> TChan () -- ^ Channel that is filled when player is connected
  -> TChan Text -- ^ Channel that is filled when player is disconnected
  -> TChan Text -- ^ Channel that is filled with connection errors
  -> TVar (Maybe InputEvent, [InputEvent]) -- ^ Channel with user input event (mouse/keyboard)
  -> Text -- ^ Username
  -> IO Renderer 
cubeRenderer messageBusOutput messageBusInput connectBus disconnectBus connErrorBus userInputBus username = do 
  stateRef <- newIORef $ initialGameState (mainWire username)
  return $ \size -> do
    state <- readIORef stateRef 
    (mscene, newState) <- stepGame messageBusOutput messageBusInput connectBus disconnectBus connErrorBus userInputBus size state
    writeIORef stateRef $ newState `deepseq` newState
    case mscene of 
      Nothing -> return emptyFrameBufferDepthAlpha
      Just scene -> return $ renderScene scene size

mainWire :: Text -> GameMonad (GameWire () Scene) 
mainWire username = do
  logInfo "Starting client simulation..."
  Core.addNewFileSystemPack "media" "media"
  Core.addNewFileSystemPack "ships" "media/ships"
  Core.addNewFileSystemPack "tiles" "media/tiles"
  Core.addNewFileSystemPack "npc" "media/npc"
  return $ proc _ -> do 
    -- DEBUG
    logInfoE . mapE (const $ "Player is connected") . playerConnected -< ()
    logInfoE . mapE (const $ "Player is disconnected") . playerDisconnected -< ()
    logInfoE . mapE showt . playerNetMessages -< ()
    --logInfoE . mapE show . mouseClick LeftButton Down -< () -- too much spam into console
    --logInfoE . mapE show . mouseMove -< ()
    -- DEBUG

    preloadWorld -< ()

  where 
    preloadWorld = proc a -> do 
      -- Send request for world
      sendNetMessage . mapE (const PlayerRequestWorld) . playerConnected -< ()
      -- Respond for the request
      gotWorldRespond <- singleNetMessage isPlayerWorld -< ()
      worldRespond <- mapE (worldFromMsg username) -< gotWorldRespond
      logInfoE . arr (fmap $ \(PlayerWorld i bname ) 
        -> "Got info about world '" <> bname <> "' with id " <> showt i) 
        -< gotWorldRespond
      -- When received the world message, switch to simulate the new world
      rSwitch (pure emptyScene) -< (a, preloadResources <$> worldRespond)

    preloadResources w = proc a -> do 
      --rse <- loadTerrM1Ship -< () --loadDebugShip -< ()
      rse <- loadResource2 
        "tiles:floor.obj" (ObjResourceName "FloorModel") 
        "tiles:floor_02.png" (Par2DRGBA RGBA8) -< ()

      rSwitch (pure emptyScene) -< (a, preloadResourcesSwitch w <$> rse)

    preloadResourcesSwitch w mres = case mres of 
      Left err -> proc _ -> do
        logWarnE . mapE (const err) . now -< ()
        returnA -< emptyScene
      Right res -> renderDebug w res

    renderDebug worldWire (ObjResource model, TextureResource tex) = loop $ proc (_, state_) -> do 
      
      -- | Lock pointer for camera
      lockPointer . now -< True 

      w <- runIndexed' worldWire -< ()
      let bs = clientWorldModels w 

      layerChange <- periodicList 1 (cycle [0 :: Int .. 4]) -< ()
      (oldP, layer) <- delay (zunit, 0) -< state_
      let newLayer = event layer id layerChange

      let mcam = clientPlayerCamera <$> clientWorldPlayer w
      case mcam of 
        Nothing -> forceNF -< (w `deepseq` Scene mempty mempty, (oldP, newLayer))
        Just cam -> do 
          mpos <- mouseWorldPos -< cam
          let addZ = case bs of
                        [] -> 0 :: Double
                        (cb:_) -> fromIntegral (clientBoxedLayer cb) * blockSize
              lightPos = Vec.snoc mpos (addZ + 3.0)
              --l1 = PointLight lightPos 4 (PowerLoss 0.5 0.1 0) 1
              --testLights = lightDouble2Float <$> [l1]
              --testCam = newCamera (CameraId 0) ((-5):0:.0:.()) (1:.0:.0:.()) (0:.0:.1:.())
              scene = Scene (\s -> [modelFragmentStream cam model tex s]) mempty
              --scene = Scene (mconcat ((\сb -> renderLayeredBoxedModel (clientBoxedRendered сb) (clientBoxedLayer сb) cam testLights 0) <$> bs)) mempty
              --scene = Scene (const [fmap (,fragDepth) $ sliceTextureToLayers tex depthTex 5 !! newLayer]) mempty
          forceNF -< (w `deepseq` scene, (lightPos, newLayer))