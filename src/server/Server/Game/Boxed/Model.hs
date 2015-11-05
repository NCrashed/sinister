{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, TypeFamilies, Arrows, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Server.Game.Boxed.Model(
    BoxedModelMsg(..)
  , boxedModel 
  , module BM
  -- | Message API
  , sendModelSetBlock
  , sendModelSetMeta
  , sendModelSetBlockAndMeta
  ) where 

import Core 

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Typeable 
import Data.Maybe 

import Prelude hiding (id, (.))
import FRP.Netwire 

import Game.World
import Game.Player
import Game.Boxed.Model as BM
import Game.Boxed.Block
import Data.Vec as Vec 
import Server.Game.World.Event
import Network.Protocol.Message 
import Control.Monad as Monad 

import Data.Text (Text)

data BoxedModelMsg = 
    BoxedModelSetBlock !(Vec3 Int) !Block
  | BoxedModelSetMeta !(Vec3 Int) !BlockMeta
  | BoxedModelSetBlockAndMeta !(Vec3 Int) !Block !BlockMeta
  deriving (Typeable, Generic)

instance NFData BoxedModelMsg

instance Messagable BoxedModelId where 
  type MessageType BoxedModelId = BoxedModelMsg 
  fromCounter = BoxedModelId 
  toCounter = unBoxedModelId

-- | Sends message to boxed model to set block at specified location
sendModelSetBlock :: GameWire (Event (BoxedModelId, Vec3 Int, Block)) (Event ())
sendModelSetBlock = putMessageE . mapE (\(i, p, b) -> (i, BoxedModelSetBlock p b))

-- | Sends message to boxed model to set block meta at specified location
sendModelSetMeta :: GameWire (Event (BoxedModelId, Vec3 Int, BlockMeta)) (Event ())
sendModelSetMeta = putMessageE . mapE (\(i, p, b) -> (i, BoxedModelSetMeta p b))

-- | Sends message to boxed model to set block meta at specified location
sendModelSetBlockAndMeta :: GameWire (Event (BoxedModelId, Vec3 Int, Block, BlockMeta)) (Event ())
sendModelSetBlockAndMeta = putMessageE . mapE (\(i, p, b, m) -> (i, BoxedModelSetBlockAndMeta p b m))

-- | Server side actor that controlls boxed model
-- Boxed model is created from single block at given position and rotation
boxedModel :: 
  Vec3 Double -- ^ position in world
  -> Double -- ^ rotation around Z axis
  -> Block -- ^ Block type
  -> GameActor BoxedModelId World BoxedModel
boxedModel pos rot block = makeIndexed simBoxed
  where 
    simBoxed mid = loop $ proc (w, m_) -> do 
      m <- delay (startModel mid) -< m_
      (m2, _) <- processMessagesM processMessage mid -< (m, w)

      -- player wants our data
      reqEvent <- playerReqChunks mid -< w
      sendNetMessages -< (concat . catMaybes . fmap (makeChunkDataMessages w m2)) <$> reqEvent

      forceNF -< (m2, m2)

      where 
      -- | Prepares set of messages to send to client about boxed model
      makeChunkDataMessages :: World -> BoxedModel -> PlayerId -> Maybe [(Text, NetworkMessage)]
      makeChunkDataMessages w bm pid = do 
        p <- worldFindPlayer w pid  
        return $ (playerName p,) <$> makeBlockMappingMessage bm : makeChunkMessages bm 

    startModel mid = 
      setModelBlock 0 0 0 block 
      $ (emptyModel mid) { modelPos = pos, modelRot = rot }

    makeBlockUpdateMsg mid p x y z b = (playerName p, BoxedModelBlockUpdate (toCounter mid) (x,y,z) b)
    makeBlockMetaUpdateMsg mid p x y z b = (playerName p, BoxedModelBlockMetaUpdate (toCounter mid) (x,y,z) b)
    makeBlockBlockAndMetaUpdateMsg mid p x y z b m = (playerName p, BoxedModelBlockAndMetaUpdate (toCounter mid) (x,y,z) b m)

    -- Sends mapping update for specified players
    sendMappingUpdate :: BoxedModel -> [Player] -> GameMonad ()
    sendMappingUpdate bm ps = Monad.when (modelMappingChanged bm) $
      sendNetMessagesM $ (\p -> (playerName p, makeBlockMappingMessage bm)) <$> ps

    processMessage :: (BoxedModel, World) -> BoxedModelMsg -> GameMonad (BoxedModel, World)
    processMessage (m, w) msg = 
      let ps = worldPlayersAroundModel mid w
      in case msg of 
        BoxedModelSetBlock (x:.y:.z:.()) b -> let
          update = do 
            let (m', bi) = setModelBlock' x y z b m
            sendMappingUpdate m' ps
            sendNetMessagesM $ (\p -> makeBlockUpdateMsg mid p x y z bi) <$> ps
            return (m', w)
          in case modelBlock x y z m of
            Nothing -> update
            Just oldBlock | oldBlock /= b -> update 
            _ -> return (m, w)

        BoxedModelSetMeta (x:.y:.z:.()) bmeta -> let 
          update = do 
            sendNetMessagesM $ (\p -> makeBlockMetaUpdateMsg mid p x y z bmeta) <$> ps
            return (setModelBlockMeta x y z bmeta m, w)
          in case modelBlockMeta x y z m of 
            oldMeta | oldMeta /= bmeta -> update 
            _ -> return (m, w)

        BoxedModelSetBlockAndMeta (x:.y:.z:.()) b bmeta -> let 
          updateAll = do 
            let (m', bi) = setModelBlockAndMeta' x y z b bmeta m
            sendMappingUpdate m' ps
            sendNetMessagesM $ (\p -> makeBlockBlockAndMetaUpdateMsg mid p x y z bi bmeta) <$> ps
            return (m', w)
          updateId = do 
            let (m', bi) = setModelBlock' x y z b m
            sendMappingUpdate m' ps
            sendNetMessagesM $ (\p -> makeBlockUpdateMsg mid p x y z bi) <$> ps
            return (m', w)
          updateMeta = do 
            sendNetMessagesM $ (\p -> makeBlockMetaUpdateMsg mid p x y z bmeta) <$> ps
            return (setModelBlockMeta x y z bmeta m, w)
          in case modelBlockAndMeta x y z m of
            Nothing -> updateAll 
            Just (oldB, oldMeta) | oldB /= b && oldMeta /= bmeta -> updateAll 
            Just (oldB, _) | oldB /= b && bmeta /= 0 -> updateAll
            Just (oldB, _) | oldB /= b && bmeta == 0 -> updateId
            Just (_, oldMeta) | oldMeta /= bmeta -> updateMeta
            _ -> return (m, w)
      where 
        mid = modelId m