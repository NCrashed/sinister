{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, TypeFamilies, Arrows, BangPatterns, TupleSections, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Client.World(
    WorldMsg(..)
  , ClientWorld(..)
  , world
  , worldFromMsg
  , module W
  ) where

import Core 

import qualified Data.HashMap.Strict as M 

import GHC.Generics (Generic)
import Control.DeepSeq hiding (force)
import Data.Typeable 

import Prelude hiding (id, (.))
import FRP.Netwire 

import Game.World as W
import Client.Player 
import Network.Protocol.Message 
import Game.Boxed.Model
import Control.Wire.Collection
import Client.Boxed.Model

import Data.Text (Text)
import TextShow 

-- | World extended with client specific data
data ClientWorld = ClientWorld {
    clientWorld :: World 
  , clientWorldPlayer :: Maybe ClientPlayer
  , clientWorldModels :: [ClientBoxedModel]
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
      nextController <- mapE (\p -> switch $ initalProcotol2 p) -< eventWithPlayer

      first forceNF -< (ClientWorld startWorld Nothing [], nextController) 

    -- | Second, get info about boxed models around
    initalProcotol2 :: Player -> GameWire a (ClientWorld, Event (GameWire a ClientWorld))
    initalProcotol2 startPlayer = proc _ -> do 
      gotRespond <- singleNetMessage isBoxedModelsAround -< ()
      logInfoE . mapE (\m -> "Got boxed models data: " <> showt m) -< gotRespond
          -- Form list of models from message
      let boxedModels = fmap ( \(BoxedModelsAround ids) 
            -> fmap (emptyModel . BoxedModelId) ids) gotRespond
          -- Form new world from list of models
          w ms = startWorld { worldModels = M.fromList $ fmap modelId ms `zip` ms }
          -- Create new controller from new world
          nextController = fmap ( \ms -> mainController startPlayer $ w ms) boxedModels

      first forceNF -< (ClientWorld startWorld Nothing [], nextController)

    mainController :: Player -> World -> GameWire a ClientWorld
    mainController startPlayer initialWorld = loop $ proc (_, w_) -> do 
      w <- processMessages processMessage wid . delay initialWorld -< w_
      p <- runIndexed' (player startPlayer) -< w

      modelAddEvent <- never -< () -- TODO: stub, nothing can add or remove
      modelRemoveEvent <- never -< () -- models at the moment
      cbs <- dynCollection initalModels -< ((w, clientPlayerCamera p), modelAddEvent, modelRemoveEvent)
      let bs = clientBoxedModel <$> cbs 
          w2 = w { worldModels = M.fromList $ fmap modelId bs `zip` bs }
          cw = ClientWorld w2 (Just p) cbs
      forceNF -< (cw, w2)
      where 
        initalModels = boxedModel <$> M.elems (worldModels initialWorld)

    startWorld = World wid name M.empty M.empty M.empty 1000

    processMessage _ _ = error "Client.World.processMessage unimplemented"