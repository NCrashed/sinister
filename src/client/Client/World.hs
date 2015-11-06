{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, TypeFamilies, Arrows, BangPatterns, TupleSections, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Client.World(
    WorldMsg(..)
  , ClientWorld(..)
  , world
  , worldFromMsg
  , module W
  ) where

import Client.Player 
import Control.DeepSeq hiding (force)
import Core 
import Data.Text (Text)
import Data.Typeable 
import FRP.Netwire 
import Game.World as W
import GHC.Generics (Generic)
import Network.Protocol.Message 
import Prelude hiding (id, (.))
import qualified Data.HashMap.Strict as M 
import TextShow 

-- | World extended with client specific data
data ClientWorld = ClientWorld {
    clientWorld :: World 
  , clientWorldPlayer :: Maybe ClientPlayer
} deriving (Generic)

instance NFData ClientWorld

data WorldMsg = WorldMsgStub
  deriving (Typeable, Generic)

instance NFData WorldMsg

instance Messagable WorldId where 
  type MessageType WorldId = WorldMsg 
  fromCounter = WorldId 
  toCounter = unWorldId

-- | Creates world from data of PlayerWorld message
-- Note: throw error if the message of wrong type
worldFromMsg :: Text -> NetworkMessage -> GameActor WorldId a ClientWorld
worldFromMsg username (PlayerWorld i name) = world (WorldId i) name username
worldFromMsg _ _ = error "worldFromMsg: wrong message type"

-- | Makes wire for world
-- Client receives world id by request to server
world :: WorldId -> Text -> Text -> GameActor WorldId a ClientWorld
world wid name username = makeFixedIndexed wid $ const $ switch initalProcotol1
  where
    -- | First, get info about player
    initalProcotol1 :: GameWire a (ClientWorld, Event (GameWire a ClientWorld))
    initalProcotol1 = proc _ -> do 
      -- | Request player info
      sendNetMessage . mapE (const PlayerRequestEnviroment) . now -< ()
      -- | Server respond
      gotRespond <- singleNetMessage isPlayerData -< ()
      logInfoE . mapE (\m -> "Got player data: " <> showt m) -< gotRespond
      eventWithPlayer <- mapE (playerFromMsg username) -< gotRespond
      nextController <- mapE (\p -> mainController p startWorld) -< eventWithPlayer

      first forceNF -< (ClientWorld startWorld Nothing, nextController) 

    mainController :: Player -> World -> GameWire a ClientWorld
    mainController startPlayer initialWorld = loop $ proc (_, w_) -> do 
      w <- processMessages processMessage wid . delay initialWorld -< w_
      p <- runIndexed' (player startPlayer) -< w

      let cw = ClientWorld w (Just p)
      forceNF -< (cw, w)

    startWorld = World wid name M.empty M.empty

    processMessage _ _ = error "Client.World.processMessage unimplemented"