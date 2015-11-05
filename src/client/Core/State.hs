{-# LANGUAGE BangPatterns, DeriveGeneric, RecordWildCards, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Core.State(
    GameState(..)
  , stepGame
  , initialGameState
  , makeUserInputChannel
  ) where

import Core.Context 
import Core.Monad 

import Prelude hiding (id, (.))
import FRP.Netwire
import Control.Wire.Core (stepWire)

import GHC.Generics (Generic)
import Control.DeepSeq
import Control.Monad as Monad
import Control.Monad.State.Strict
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Network.Protocol.Message 

import Util.Concurrent

import Core.Input
import Core.InputGLFW()
import qualified Graphics.UI.GLFW as GLFW 
import qualified Data.Traversable as T 
import qualified Data.Sequence as S 
import Data.Vec (Vec2)

import qualified Data.Text.IO as T
import Data.Text (Text)

-- | The main state of game
data GameState a = GameState {
  -- | Holds info about simulation time and delta time
    gameSession :: !GameSession
  -- | Current simulation wire (arrow)
  , gameMainWire :: !(GameWire () a)
  -- | Current game context (events, inner messages, everthing that could be used in GameMonad)
  , gameContext :: !GameContext
} deriving (Generic)

instance NFData GameSession where 
  rnf = (`seq` ())

instance NFData (GameWire () a) where
  rnf = (`seq` ())
  
instance NFData a => NFData (GameState a)

-- | Simulate one step (frame) of game simulation
-- That function is a heart of game core, it reactimates all objects and systems.
-- stepGame reads enqued network messages, listens connection/disconnection events
-- , loads fast changing player inputs from keyboard/mouse/joystick
-- , performs high-level resource management.
stepGame :: NFData a => TChan NetworkMessage -- ^ Channel with messages to send to server
  -> TChan NetworkMessage -- ^ Channel with received network messages from server
  -> TChan () -- ^ Channel that is filled when player is connected
  -> TChan Text -- ^ Channel that is filled when player is disconnected
  -> TChan Text -- ^ Channel that is filled with connection errors
  -> TVar (Maybe InputEvent, [InputEvent]) -- ^ Channel with user inputs
  -> Vec2 Int -- ^ Current size of window
  -> GameState a -- ^ Previous game state
  -> IO (Maybe a, GameState a) -- ^ Next game state
stepGame messageBusOutput messageBusInput connChan disconnChan connErrorBus userInputBus size (!GameState{..}) = do
  -- Send quequed messages  
  mapM_ sendMessage $ reverse $ eventMngMessages2Send $ clientEventMng $ gameCustomContext gameContext
  -- Read new events and empty queue of out messages
  emng' <- EventManager <$> readMessages <*> readInputEvents <*> pure []
  let gameContext' = gameContext { gameCustomContext = (gameCustomContext gameContext) { 
        clientEventMng = emng' }
      }
  
  -- Simulate one step
  (s, session') <- stepSession gameSession 
  let ((eres, wire'), context1) = runState (stepWire gameMainWire s (Right ())) gameContext'
  
  -- Put all messages from log
  _ <- T.mapM T.putStrLn $ gameLogMessages context1
  let context2 = context1 { gameLogMessages = S.empty }

  -- Handle resources
  let resmng1 = clientResMng $ gameCustomContext context2
  resmng2 <- runResourceLoaders resmng1
  resmng3 <- readResourceResults resmng2 
  let context3 = context2 { gameCustomContext = (gameCustomContext context2) {
      clientResMng = resmng3
    , clientViewportSize = size -- also update current viewport size
    } 
  }

  -- Calculate new state of game
  let newState = GameState session' wire' context3
      res = either (const Nothing) Just eres

  -- Locking of cursor
  mw <- GLFW.getCurrentContext
  case mw of 
    Just w -> lockCursor w (clientCursorLock $ gameCustomContext gameContext) (clientCursorLock $ gameCustomContext context3)
    Nothing -> return ()
  
  return $ newState `deepseq` (res, newState)

  where 
    sendMessage :: NetworkMessage -> IO () 
    sendMessage !msg = do
      putStrLn $ unwords ["Message sent:", show msg]
      atomically $ writeTChan messageBusOutput msg 
      yield -- let core server thread to awake

    readMessages :: IO [NetworkMessage]
    readMessages = readTChanWhileCould messageBusInput

    readConnects :: IO (Maybe ())
    readConnects = atomically $ tryReadTChan connChan

    readDisconnects :: IO (Maybe Text)
    readDisconnects = atomically $ tryReadTChan disconnChan

    readConnErrors :: IO (Maybe Text)
    readConnErrors = atomically $ tryReadTChan connErrorBus

    readInputEvents :: IO [InputEvent]
    readInputEvents = do 
      conn <- fmap (maybe [] (const [ConnectEvent])) readConnects
      disconn <- fmap (maybe [] (\s -> [DisconnectEvent s])) readDisconnects
      connErrs <- fmap (maybe [] (\s -> [ConnectionError s])) readConnErrors
      (mpos, keys) <- readTVarIO userInputBus
      atomically $ writeTVar userInputBus (Nothing, [])

      --putStrLn $ show user
      return $! connErrs ++ conn ++ disconn ++ maybe [] (\a->[a]) mpos ++ keys

    -- | Performing locking of cursor
    lockCursor :: GLFW.Window 
      -> Bool -- ^ Previous flag of locking
      -> Bool -- ^ Current flag of locking
      -> IO ()
    lockCursor w False True = do 
      GLFW.setCursorInputMode w GLFW.CursorInputMode'Hidden
    lockCursor w True False = do
      GLFW.setCursorInputMode w GLFW.CursorInputMode'Normal
    lockCursor _ _ _ = return ()

-- | Runs initialization that calclulates first game state
initialGameState :: GameMonad (GameWire () a) -> GameState a
initialGameState w =
  let (mainWire, gContext) = runState w newGameContext 
  in GameState clockSession_ mainWire gContext

-- | Makes channel with events from user input
makeUserInputChannel :: GLFW.Window -> TVar (Maybe InputEvent, [InputEvent]) -> IO ()
makeUserInputChannel w tvar = do 
  GLFW.setKeyCallback w $ Just $ \_ key _ keyState modifiers -> do 
    (x, y) <- GLFW.getCursorPos w
    let ev = KeyEvent x y (toKey key) (toKeyState keyState) (toModifiers modifiers)
    void $ atomically $ modifyTVar tvar (\(a, b) -> (a, ev:b))
  
  GLFW.setMouseButtonCallback w $ Just $ \_ btn btnState modifiers -> do 
    (x, y) <- GLFW.getCursorPos w
    let ev = KeyEvent x y (toKey btn) (toKeyState btnState) (toModifiers modifiers)
    void $ atomically $ modifyTVar tvar (\(a, b) -> (a, ev:b))

  GLFW.setCursorPosCallback w $ Just $ \_ x y -> do 
    void $ atomically $ modifyTVar tvar (\(_, b) -> (Just $ MouseMoveEvent x y, b))
