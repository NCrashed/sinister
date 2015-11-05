{-# LANGUAGE Arrows, TupleSections #-}
module Server.Game.World.Event(
  -- | Network events 
    worldNetMessages
  , worldNetMessages'
  , worldPlayerNetMessages
  , worldPlayerNetMessages'
  , worldPlayerSingleMessage
  -- | Logic events
  , playerReqChunks
  ) where 

import Core 
import Prelude as P hiding (id, (.))
import FRP.Netwire 

import Game.World 
import Game.Player 
import Game.Boxed.Model 

import qualified Data.HashMap.Strict as M 
import Network.Protocol.Message 
import Data.Maybe 

-- | Arrow that fires event when a player in the world sends messages
worldNetMessages :: GameWire World (Event (M.HashMap PlayerId [NetworkMessage]))
worldNetMessages = proc w -> do 
  e <- netMessages -< ()
  filterE (not . M.null) -< M.fromList . catMaybes . fmap (convertMap w) . M.toList <$> e 
  where 
    convertMap w (k, v) = ((,v) . playerId) <$> worldFindPlayer w k

-- | Same as worldNetMessages but filters messages with given function
worldNetMessages' :: (NetworkMessage -> Bool) -> GameWire World (Event (M.HashMap PlayerId [NetworkMessage]))
worldNetMessages' f = filterE (not . M.null) . mapE (M.filter (not.null) . fmap (filter f)) . worldNetMessages 

-- | Arrow that fires event when specific player sends messages in specific world
worldPlayerNetMessages :: GameWire (World, PlayerId) (Event [NetworkMessage])
worldPlayerNetMessages = proc (w, pid) -> do 
  e <- worldNetMessages -< w 
  filterE (not . null) -< fromMaybe [] . M.lookup pid <$> e

-- | Arrow that fires event when specific player sends specific messages in specific world
worldPlayerNetMessages' :: (NetworkMessage -> Bool) -> GameWire (World, PlayerId) (Event [NetworkMessage])
worldPlayerNetMessages' f = filterE (not.null) . mapE (filter f) . worldPlayerNetMessages

-- | Arrow that fires event when specific player sends one specific message in specific world
worldPlayerSingleMessage :: (NetworkMessage -> Bool) -> GameWire (World, PlayerId) (Event NetworkMessage)
worldPlayerSingleMessage f = mapE P.head . worldPlayerNetMessages' f

-- | Event fires when player in range of specific model requests chunks
playerReqChunks :: BoxedModelId -> GameWire World (Event [PlayerId])
playerReqChunks mid = mapE M.keys . worldNetMessages' isNeededMsg
  where 
    isNeededMsg (RequestBoxedModelData i) = i == unBoxedModelId mid 
    isNeededMsg _ = False