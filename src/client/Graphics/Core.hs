module Graphics.Core(
    Renderer
  , drawLoop
  ) where 

import Core.State
import Core.Input
import Control.Concurrent (yield)
import Control.Concurrent.STM.TVar (TVar)
import Graphics.GPipe
import qualified Graphics.UI.GLFW as GLFW 
import Util.Monad 

import qualified Data.Text as T 
import Data.Text (Text)

type Renderer = Vec2 Int -> IO (FrameBuffer RGBAFormat DepthFormat ())

drawLoop :: Text -> Vec2 Int -> Vec2 Int -> IO Renderer -> TVar (Maybe InputEvent, [InputEvent]) -> IO () -> IO ()
drawLoop windowTitle windowPos windowSize renderer userEventBus terminator = do 
  _ <- GLFW.init
  
  renderFunc <- renderer
  (w, gpipeCallback) <- newWindow (T.unpack windowTitle) windowPos windowSize renderFunc initWindow
  mainLoop w gpipeCallback
  GLFW.terminate
  where 
  initWindow :: GLFW.Window -> IO ()
  initWindow win = do
    GLFW.setErrorCallback $ Just $ \err msg ->
      putStrLn $ unwords ["GLFW error:", show err, ", details:", msg]
    GLFW.setWindowCloseCallback win $ Just $ const terminator
    makeUserInputChannel win userEventBus

  mainLoop :: GLFW.Window -> IO () -> IO ()
  mainLoop win gpipeCallback = unlessM (GLFW.windowShouldClose win) $ do
    GLFW.pollEvents 
    gpipeCallback
    yield
    mainLoop win gpipeCallback