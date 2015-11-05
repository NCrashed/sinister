{-# LANGUAGE BangPatterns, DeriveGeneric, RecordWildCards, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Core.State(
    GameState(..)
  , stepGame
  , initialGameState
  ) where

import Core.Context 
import Core.Monad 

import Prelude hiding (id, (.))
import FRP.Netwire
import Control.Wire.Core (stepWire)

import GHC.Generics (Generic)
import Control.DeepSeq
import Control.Monad.State.Strict
import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TQueue
import Network.Protocol.Message 
import Data.HashMap.Strict as HM 
import Control.Monad as Monad 

import qualified Data.Traversable as T 
import qualified Data.Sequence as S 
import Util.Concurrent

import qualified Data.Text.IO as T 
import Data.Text (Text)

-- | The main state of game
data GameState = GameState {
  -- | Holds info about simulation time and delta time
    gameSession :: !GameSession
  -- | Current simulation wire (arrow)
  , gameMainWire :: !(GameWire () ())
  -- | Current game context (events, inner messages, everthing that could be used in GameMonad)
  , gameContext :: !GameContext
} deriving (Generic)

instance NFData GameSession where 
  rnf = (`seq` ())

instance NFData (GameWire () ()) where 
  rnf = (`seq` ())

instance NFData GameState 

-- | Simulate one step (frame) of game simulation
stepGame :: TQueue (Text, NetworkMessage) -- ^ Channel with received network messages from server core
  -> TQueue Text -- ^ Channel with names of connected players
  -> TQueue Text -- ^ Channel with names of disconnected players
  -> GameState -- ^ Previous game state
  -> IO GameState -- ^ Next game state
stepGame messageBus connChan disconnChan (!GameState{..}) = do
  -- Send quequed messages  
  _ <- mapM sendMessage $ reverse $ eventMngMessages2Send $ gameCustomContext gameContext
  -- Read new events and empty queue of out messages
  emng' <- EventManager <$> readMessagesMap <*> readConnects <*> readDisconnects <*> pure []
  let gameContext' = gameContext { gameCustomContext = emng' }
  -- Simulate one step
  (s, session') <- stepSession gameSession 
  let ((_, wire'), context') = runState (stepWire gameMainWire s (Right ())) gameContext'
  -- Put all messages from log
  _ <- T.mapM T.putStrLn . S.reverse $ gameLogMessages context'
  -- Calculate new state of game
  return $! GameState session' wire' context' { gameLogMessages = S.empty }

  where 
    sendMessage :: (Text, NetworkMessage) -> IO () 
    sendMessage !msg = do
      putStrLn $ unwords ["Message sent:", show msg]
      atomically $ writeTQueue messageBus msg 
      yield -- let core server thread to awake

    readMessages :: IO [(Text, NetworkMessage)]
    readMessages = readTQueueWhileCould' messageBus

    readMessagesMap :: IO (HashMap Text [NetworkMessage])
    readMessagesMap = do
      msgs <- readMessages
      Monad.unless (Prelude.null msgs) $ print msgs -- DEBUG
      return $! Prelude.foldr go HM.empty msgs
      where 
        go (name, msg) acc = HM.insertWith (\_ msgs -> msg : msgs) name [msg] acc

    readConnects :: IO [Text]
    readConnects = readTQueueWhileCould' connChan 

    readDisconnects :: IO [Text]
    readDisconnects = readTQueueWhileCould' disconnChan

-- | Runs initialization that calclulates first game state
initialGameState :: GameMonad (GameWire () ()) -> GameState 
initialGameState w =
  let (mainWire, gContext) = runState w newGameContext
  in GameState clockSession_ mainWire gContext

