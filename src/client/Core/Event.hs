{-# LANGUAGE BangPatterns, RecordWildCards, Arrows #-}
module Core.Event(
    newEventManager
  , deltaTime
  -- | Player relayted events
  , playerConnected
  , playerDisconnected
  , playerNetMessages
  , singleNetMessage
  , filterNetMessages
  -- | Sending network messages
  , sendNetMessage
  , sendNetMessages
  -- | Mouse and keyboard events
  , mouseClick
  , centeredMouseClick
  , keyEvent
  , mouseMove
  , deltaMouseMove
  , centeredMouseMove
  -- | Pointer locking
  , lockPointer
  , isPointerLocked
  , pointerLocked
  ) where

import Core.Context 
import Core.CoreEvent
import Core.Monad 
import Core.Input 

import Prelude hiding (id, (.))
import FRP.Netwire

import Control.Wire.Core (mkPure, mkGen)
import Control.Wire.Unsafe.Event
import Network.Protocol.Message 
import Control.DeepSeq 
import Data.Vec hiding (reverse, head)

import Control.Monad.State.Strict as S 

import Data.Text (Text)

-- | Sometimes you need to know dt scince last simulation step
deltaTime :: Fractional b => GameWire a b
deltaTime = mkPure $ \ds _ -> 
  let dt = realToFrac (dtime ds)
  in dt `seq` (Right dt, deltaTime)

-- | Returns all network messages for specified player
-- List of messages contains only messages that were received
-- scince last simulation step.
playerNetMessages' :: GameMonad [NetworkMessage]
playerNetMessages' = eventMngNetMessages <$> getEventMng

-- | Fires when network messages for specific player are arrived
-- Note: handshake and login messages are handled before entering to the simulation
-- 
-- Takes player name, event carries all messages that have arrived for player
-- scince last frame. 
playerNetMessages :: GameWire a (Event [NetworkMessage])
playerNetMessages = proc _ -> do 
  msgs <- liftGameMonad playerNetMessages' -< () 
  if Prelude.null msgs 
  then forceNF -< NoEvent
  else forceNF -< Event msgs

-- | Fires when players log in (connect, handshake and authenticate)
-- Returns: player name
playerConnected :: GameWire a (Event ())
playerConnected = liftGameMonad $ do 
  emng <- getEventMng
  return $! case Prelude.filter isConnect $ eventMngInputEvents emng of 
    [] -> NoEvent
    _ -> Event ()  
  where 
    isConnect (ConnectEvent) =  True
    isConnect _ = False 

-- | Fires when players are disconnected from server (either by themself, either by force)
-- Event carries describing message 
playerDisconnected :: GameWire a (Event Text)
playerDisconnected = liftGameMonad $ do 
  emng <- getEventMng
  return $! case Prelude.filter isDisconnect $ eventMngInputEvents emng of 
    (DisconnectEvent msg : _) -> Event msg
    _ -> NoEvent
  where 
    isDisconnect (DisconnectEvent _) = True 
    isDisconnect _ = False 

-- | Wire get discrete stream of messsages to send and
-- sends the messages to server side as they arrived
sendNetMessage :: GameWire (Event NetworkMessage) ()
sendNetMessage = proc ev -> case ev of 
  NoEvent -> forceNF -< ()
  Event msg -> forceNF . liftGameMonad1 send -< msg
  where 
    send msg = do 
      emng <- getEventMng
      putEventMng $ msg `deepseq` emng { eventMngMessages2Send = msg : eventMngMessages2Send emng }

-- | Wire get discrete stream of packs (messsages to send) and
-- sends the messages to server side as they arrived
sendNetMessages :: GameWire (Event [NetworkMessage]) ()
sendNetMessages = proc ev -> case ev of 
  NoEvent -> forceNF -< ()
  Event msgs -> forceNF . liftGameMonad1 send -< msgs
  where 
    send msgs = do 
      emng <- getEventMng
      putEventMng $ msgs `deepseq` emng { eventMngMessages2Send = reverse msgs ++ eventMngMessages2Send emng }

-- | Filters stream of input messages and fires when one (first one) satisfies predicate
singleNetMessage :: (NetworkMessage -> Bool) -> GameWire a (Event NetworkMessage)
singleNetMessage f = mapE head . filterNetMessages f

-- | Filters stream of input messages and fires when resulting set isn't empty
filterNetMessages :: (NetworkMessage -> Bool) -> GameWire a (Event [NetworkMessage])
filterNetMessages f = filterE (not.null) . mapE (filter f) . playerNetMessages

-- | Fires when user click in viewport with specified mouse button and key state
-- Event carries coordinates in range 0 .. 1 from left upper corner of viewport
mouseClick :: MouseButton -> KeyState -> GameWire a (Event (Vec2 Double, Modifiers))
mouseClick mb ks = keyEvent (MouseButton mb) ks

-- | Same as mouseClick but origin is moved to center and each corner has amplitude of 1.0
centeredMouseClick :: MouseButton -> KeyState -> GameWire a (Event (Vec2 Double, Modifiers))
centeredMouseClick mb ks = mapE (\(x:.y:.(), mds) -> ((2*x-1):.(1-2*y):.(), mds)) . mouseClick mb ks

-- | Fires when user press/unpress specified key (either keyboard or mouse) 
-- Event carries coordinates in range 0 .. 1 from left upper corner of viewport
keyEvent :: Key -> KeyState -> GameWire a (Event (Vec2 Double, Modifiers))
keyEvent k ks = liftGameMonad $ do 
  events <- eventMngInputEvents <$> getEventMng
  let neededEvents = filter criteria events 
  case neededEvents of 
    (KeyEvent x y _ _ mds :_) -> return $! (x, y, mds) `deepseq` Event (x:.y:.(), mds)
    _ -> return NoEvent
  where 
    criteria :: InputEvent -> Bool 
    criteria (KeyEvent _ _ k' ks' _) = k' == k && ks' == ks
    criteria _ = False 

-- | Fires when user changes mouse position within window
-- Event carries coordinates in range 0 .. 1 from left upper corner of viewport
mouseMove :: GameWire a (Event (Vec2 Double))
mouseMove = liftGameMonad $ do 
  events <- eventMngInputEvents <$> getEventMng
  let neededEvents = filter criteria events 
  case neededEvents of 
    (MouseMoveEvent x y : _) -> return $! (x, y) `deepseq` Event (x:.y:.())
    _ -> return NoEvent
  where
    criteria :: InputEvent -> Bool 
    criteria (MouseMoveEvent _ _) = True
    criteria _ = False 

-- | Fires when you move mouse within window
-- Event carries delta coordinates from last movement
deltaMouseMove :: GameWire a (Event (Vec2 Double))
deltaMouseMove = mapE fst . notYet . accumE (\(_, lastVec) v -> (v - lastVec, v)) (0, 0) . mouseMove

-- | Same as mouseMove but origin is moved to center and each corner has amplitude of 1.0
centeredMouseMove :: GameWire a (Event (Vec2 Double))
centeredMouseMove = mapE (\(x:.y:.()) -> (2*x-1):.(1-2*y):.()) . mouseMove

-- | Sets current state of cursor locking
lockPointerM :: Bool -> GameMonad ()
lockPointerM flag = do 
  cntx <- S.get 
  S.put $ cntx { gameCustomContext = (gameCustomContext cntx) { clientCursorLock = flag } }

-- | Returns current state of cursor locking
getPointerLockM :: GameMonad Bool 
getPointerLockM = fmap (clientCursorLock . gameCustomContext) S.get

-- | Hides pointer and sticks it within window
-- Passing False makes reverse operation to release and show the pointer
lockPointer :: GameWire (Event Bool) (Event ())
lockPointer = onEventM lockPointerM

-- | Returns current state of cursor locking
isPointerLocked :: GameWire a Bool
isPointerLocked = liftGameMonad getPointerLockM

-- | Fires when cursor locking mode is changed
pointerLocked :: GameWire a (Event Bool)
pointerLocked = go False 
  where 
    go cur = mkGen $ \_ _ -> do 
      val <- getPointerLockM
      let e = if cur == val then NoEvent else Event val 
      return $ val `seq` e `seq` (Right e, go val)