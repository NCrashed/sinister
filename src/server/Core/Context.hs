{-# LANGUAGE StandaloneDeriving, DeriveGeneric, Arrows #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Core.Context(
    GameSession
  , GameWire
  , GameWireI
  , GameMonad
  , GameActor
  , GameContext
  , EventManager(..)
  , getEventMng
  , putEventMng
  ) where

import Core.Monad
import Core.Indexed 

import GHC.Generics (Generic)
import Control.Monad.State.Strict
import Control.DeepSeq

import Network.Protocol.Message 
import qualified Data.HashMap.Strict as HM 

import Data.Text (Text)

type GameContext = GameContextG EventManager
type GameMonad a = GameMonadG EventManager a
type GameWire a b = GameWireG EventManager a b
type GameWireI i a b = GameWireGI EventManager i a b 
type GameActor i a b = GameActorG EventManager i a b 

instance GameContextClass EventManager where 
  newCustomContext = EventManager HM.empty [] [] []

-- | TODO: OutputEvent for manual disconnects

data EventManager = EventManager {
  -- | Holds network messages for players (indexed by player name)
    eventMngNetMessages :: !(HM.HashMap Text [NetworkMessage])
  -- | Holds list of player names that were connected
  , eventMngConnects :: ![Text]
  -- | Holds list of player names that were disconnected
  , eventMngDisconnects :: ![Text]
  -- | Holds messages that should be sent to client side
  -- Note: first message in list - the last message in time
  , eventMngMessages2Send :: ![(Text, NetworkMessage)]
} deriving (Generic)

instance NFData EventManager

getEventMng :: GameMonad EventManager 
getEventMng = fmap gameCustomContext get 

putEventMng :: EventManager -> GameMonad () 
putEventMng emng = do 
  cntx <- get 
  put $ cntx { gameCustomContext = emng }