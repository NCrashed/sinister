{-# LANGUAGE BangPatterns, RecordWildCards, Arrows #-}
module Core.Event(
    deltaTime
  -- | Player relayted events
  , playersConnected
  , playersDisconnected
  , playerNetMessages
  , singlePlayerNetMessage
  , filterPlayerNetMessages
  , netMessages
  , filterNetMessages 
  -- | Sending network messages
  , sendNetMessageM
  , sendNetMessage
  , sendNetMessagesM
  , sendNetMessages
  ) where

import Core.Context 
import Core.Monad 
import Core.CoreEvent

import Prelude hiding (id, (.))
import FRP.Netwire
import Control.Wire.Core (mkPure)
import Control.Wire.Unsafe.Event
import qualified Data.HashMap.Strict as HM 
import Data.Maybe 

import Network.Protocol.Message 
import Control.DeepSeq 

import Data.Text (Text)

-- | Sometimes you need to know dt scince last simulation step
deltaTime :: Fractional b => GameWire a b
deltaTime = mkPure $ \ds _ -> 
  let dt = realToFrac (dtime ds)
  in dt `seq` (Right dt, deltaTime)

-- | Returns all network messages for specified player
-- List of messages contains only messages that were received
-- scince last simulation step.
playerNetMessages' :: Text -- ^ Player name
  -> GameMonad [NetworkMessage]
playerNetMessages' name = do 
  emng <- getEventMng
  return $! fromMaybe [] $ name `HM.lookup` eventMngNetMessages emng

-- | Fires when network messages for specific player are arrived
-- Note: handshake and login messages are handled before entering to the simulation
-- 
-- Takes player name, event carries all messages that have arrived for player
-- scince last frame. 
playerNetMessages :: GameWire Text (Event [NetworkMessage])
playerNetMessages = proc name -> do 
  msgs <- liftGameMonad1 playerNetMessages' -< name 
  if Prelude.null msgs 
  then forceNF -< NoEvent
  else forceNF -< Event msgs

-- | Returns all network messages for all players scince last simulation step
netMessages' :: GameMonad (HM.HashMap Text [NetworkMessage])
netMessages' = fmap eventMngNetMessages getEventMng

-- | Returns all network messages for all players scince last simulation step
netMessages :: GameWire a (Event (HM.HashMap Text [NetworkMessage]))
netMessages = proc _ -> do 
  msgs <- liftGameMonad netMessages' -< ()
  if HM.null msgs 
  then forceNF -< NoEvent
  else forceNF -< Event msgs 

-- | Fires when players log in (connect, handshake and authenticate)
-- Returns: player name
playersConnected :: GameWire a (Event [Text])
playersConnected = liftGameMonad $ do 
  emng <- getEventMng
  return $! case eventMngConnects emng of 
    [] -> NoEvent
    names -> names `deepseq` Event names  

-- | Fires when players are disconnected from server (either by themself, either by force)
playersDisconnected :: GameWire a (Event [Text])
playersDisconnected = liftGameMonad $ do 
  emng <- getEventMng
  return $! case eventMngDisconnects emng of 
    [] -> NoEvent
    names -> names `deepseq` Event names  

-- | Sends network message to specified player name
sendNetMessageM :: Text -> NetworkMessage -> GameMonad ()
sendNetMessageM name msg = do 
    emng <- getEventMng
    putEventMng $ msg `deepseq` emng { eventMngMessages2Send = (name, msg) : eventMngMessages2Send emng }

-- | Wire get discrete stream of (player name, messsage to send) and
-- sends the messages to client side as they arrived
sendNetMessage :: GameWire (Event (Text, NetworkMessage)) ()
sendNetMessage = proc ev -> case ev of 
  NoEvent -> forceNF -< ()
  Event msg -> forceNF . liftGameMonad1 (uncurry sendNetMessageM) -< msg

-- | Sends many network messages to specified players (by name)
sendNetMessagesM :: [(Text, NetworkMessage)] -> GameMonad ()
sendNetMessagesM = mapM_ (uncurry sendNetMessageM)

-- | Wire get discrete stream of packs (player name, messsage to send) and
-- sends the messages to client side as they arrived
sendNetMessages :: GameWire (Event [(Text, NetworkMessage)]) ()
sendNetMessages = proc ev -> case ev of 
  NoEvent -> forceNF -< ()
  Event msgs -> forceNF . liftGameMonad1 sendNetMessagesM -< msgs

-- | Filters stream of input messages and fires when one (first one) satisfies predicate
singlePlayerNetMessage :: (NetworkMessage -> Bool) -> GameWire Text (Event NetworkMessage)
singlePlayerNetMessage f = mapE head . filterPlayerNetMessages f

-- | Filters stream of input messages and fires when resulting set isn't empty
filterPlayerNetMessages :: (NetworkMessage -> Bool) -> GameWire Text (Event [NetworkMessage])
filterPlayerNetMessages f = filterE (not.null) . mapE (filter f) . playerNetMessages

-- | Filters stream of input messages and fires when resulting set isn't empty
filterNetMessages :: (Text -> NetworkMessage -> Bool) -> GameWire a (Event (HM.HashMap Text [NetworkMessage]))
filterNetMessages f = filterE (not . HM.null) . mapE (HM.filterWithKey f') . netMessages
  where f' k = not . null . filter (f k)