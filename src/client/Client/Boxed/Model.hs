{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, TypeFamilies, Arrows, RecordWildCards, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Client.Boxed.Model(
    ClientBoxedModel(..)
  , BoxedModelMsg(..)
  , BoxedModelLayered
  , boxedModel 
  , module BM
  ) where 

import Core 

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Typeable 

import Prelude hiding (id, (.))
import FRP.Netwire 

import Game.World
import Game.Boxed.Model as BM
import Game.Boxed.Chunk
import Game.Boxed.Block
import Graphics.Render.Boxed.Model
import Network.Protocol.Message
import Control.Wire.Unsafe.Event
import Data.Foldable as F 
import Data.Vec as Vec 
import Graphics.Camera2D 
import Client.Event.Camera 

import Data.Text (Text)

-- | On client side client tracks model and maintaince layered renderer for the model
data ClientBoxedModel = ClientBoxedModel {
-- | Inner boxed model
  clientBoxedModel :: !BoxedModel
-- | Rendered version of the boxed model
, clientBoxedRendered :: !BoxedModelLayered
-- | Current layer to render
, clientBoxedLayer :: Int 
} deriving (Typeable, Generic)

instance NFData ClientBoxedModel

data BoxedModelMsg = BoxedModelMsgStub
  deriving (Typeable, Generic)

instance NFData BoxedModelMsg

instance Messagable BoxedModelId where 
  type MessageType BoxedModelId = BoxedModelMsg 
  fromCounter = BoxedModelId 
  toCounter = unBoxedModelId

-- Helpers
-- ============================================================================

-- | At the moment - fixed size of texture in atlas
tileSize :: Vec2 Int 
tileSize = 128 

-- | Makes new unrendered client side boxed model 
newClientBoxedModel :: BoxedModel -> ClientBoxedModel
newClientBoxedModel bm = ClientBoxedModel bm (emptyBoxedModelLayered tileSize) 0

-- | Performs full rerender of client boxed model
clientBoxedRerenderAtlases :: ClientBoxedModel -> GameWire a (Event (Either Text BoxedModelLayered))
clientBoxedRerenderAtlases (ClientBoxedModel{..}) = render clientBoxedRendered
  where 
  render = blitBoxedModelAtlases . makeBoxedModelAtlases tileSize clientBoxedModel

clientBoxedUpdate :: ClientBoxedModel -> (BoxedModel -> BoxedModel) -> ClientBoxedModel 
clientBoxedUpdate cbm f = cbm { clientBoxedModel = f $ clientBoxedModel cbm}

clientBoxedUpdateM :: ClientBoxedModel -> (BoxedModel -> GameMonad BoxedModel) -> GameMonad ClientBoxedModel 
clientBoxedUpdateM cbm f = do 
  m <- f $ clientBoxedModel cbm
  return $ cbm { clientBoxedModel = m}

clientLayeredUpdate :: (BoxedModel -> BoxedModelLayered -> BoxedModelLayered) -- ^ Updator of rendered model
  -> ClientBoxedModel -- ^ Client model to update
  -> ClientBoxedModel -- ^ New client model
clientLayeredUpdate f cbm = cbm { clientBoxedRendered = f (clientBoxedModel cbm) $ clientBoxedRendered cbm}

clientLayeredUpdates :: Int -- ^ Z chunk (will iterate over all layers for the level)
  -- | Handler that renders new layer with model, old layer and it z level (absolute)
  -> (BoxedModel -> BoxedModelLayered -> Int -> BoxedModelLayered) 
  -> ClientBoxedModel -- ^ Client model to update
  -> ClientBoxedModel -- ^ New client model
clientLayeredUpdates chunkZ handler cbm = cbm { 
    clientBoxedRendered = F.foldl' (\l i -> handler (clientBoxedModel cbm) l i) (clientBoxedRendered cbm) zs
  }
  where 
    zStart = chunkZ * chunkSize
    zEnd = zStart + chunkSize - 1
    zs = [zStart .. zEnd]

updateClientBoxedLayer :: (Int -> Int) -> ClientBoxedModel -> ClientBoxedModel
updateClientBoxedLayer f cbm = cbm { clientBoxedLayer = f $ clientBoxedLayer cbm }

-- Events
-- ============================================================================

-- | Fires when server sends blockId update for specific boxed model
clientBoxedModelBlockUpdate :: BoxedModelId -> GameWire a (Event [(Vec3 Int, BlockId)])
clientBoxedModelBlockUpdate mid = mapE (fmap $ \(BoxedModelBlockUpdate _ (x, y, z) bid) -> (x:.y:.z:.(), bid)) 
  . filterNetMessages isMessage
  where 
    isMessage (BoxedModelBlockUpdate i _ _) = i == toCounter mid 
    isMessage _ = False 

-- | Fires when server sends blockId update for specific boxed model
clientBoxedModelMetaUpdate :: BoxedModelId -> GameWire a (Event [(Vec3 Int, BlockMeta)])
clientBoxedModelMetaUpdate mid = mapE (fmap $ \(BoxedModelBlockMetaUpdate _ (x, y, z) bmeta) -> (x:.y:.z:.(), bmeta)) 
  . filterNetMessages isMessage
  where 
    isMessage (BoxedModelBlockMetaUpdate i _ _) = i == toCounter mid 
    isMessage _ = False 

-- | Fires when server sends blockId update for specific boxed model
clientBoxedModelBlockAndMetaUpdate :: BoxedModelId -> GameWire a (Event [(Vec3 Int, BlockId, BlockMeta)])
clientBoxedModelBlockAndMetaUpdate mid = mapE (fmap $ \(BoxedModelBlockAndMetaUpdate _ (x, y, z) bid bmeta) -> (x:.y:.z:.(), bid, bmeta)) 
  . filterNetMessages isMessage
  where 
    isMessage (BoxedModelBlockAndMetaUpdate i _ _ _) = i == toCounter mid 
    isMessage _ = False 


-- Controller
-- ============================================================================

boxedModel :: BoxedModel -> GameActor BoxedModelId (World, Camera) ClientBoxedModel
boxedModel startModel_ = makeFixedIndexed (modelId startModel_) $ \mid -> simModel startModel_ mid
  where
    simModel :: BoxedModel -> BoxedModelId -> GameWire (World, Camera) ClientBoxedModel
    simModel startModel mid = loop $ proc ((_, cam), m_) -> do 
      m <- processMessages processMessage mid . delay (newClientBoxedModel startModel) -< m_

      -- At creation request full data from server 
      sendNetMessage . now -< RequestBoxedModelData (toCounter mid)
      -- Server sends chunks
      mappingEvents <- filterNetMessages (isModelMappingMsg mid) -< ()
      chunkEvents <- filterNetMessages (isModelChunkMsg mid) -< ()

      em2 <- updateMappings -< fmap (,m) mappingEvents
      let m2 = updateChunkEvents chunkEvents . updateMappingsE em2 $ m

      -- Full rerender if we get mapping update
      m3 <- mappingUpdate -< (m2, mappingEvents)
      -- Updates model when server sends single block updates
      m4 <- singleBlocksUpdate mid -< m3 

      -- DEBUG: placing a floor at mouse pos 
      clickEvent <- mouseWorldClickZ LeftButton Up -< (cam, fromIntegral (clientBoxedLayer m4) * blockSize + Vec.get n2 (modelPos $ clientBoxedModel m4))
      sendNetMessage -< placeBlockMsg mid (clientBoxedLayer m4) . worldCoord2Local m4 . fst <$> clickEvent

      -- DEBUG: changing current floor
      floorUpEvent <- mapE (const (+1)) . keyEvent (CharKey 'r') Up -< ()
      floorDownEvent <- mapE (const (\a -> a-1)) . keyEvent (CharKey 'f') Up -< ()
      let m5 = event m4 (\f -> updateClientBoxedLayer f m4) (floorUpEvent `mergeL` floorDownEvent)

      forceNF -< (m5, m5)

    processMessage m msg = case msg of 
      BoxedModelMsgStub -> m -- TODO

    worldCoord2Local :: ClientBoxedModel -> Vec2 Double -> Vec2 Int
    worldCoord2Local cbm p = Vec.take n2 $ Vec.map (floor.(/ blockSize)) $ Vec.snoc p (blockSize * fromIntegral (clientBoxedLayer cbm)) - modelPos bm
      where bm = clientBoxedModel cbm 

    placeBlockMsg :: BoxedModelId -> Int -> Vec2 Int -> NetworkMessage
    placeBlockMsg mid z (x:.y:.()) = PlayerDebugPlaceBlock (unBoxedModelId mid) (x,y,z) (blockName thinWallBlock) 0

    -- | Updates atlases when mapping arrives and after that makes full rerender
    mappingUpdate :: GameWire (ClientBoxedModel, Event a) ClientBoxedModel
    mappingUpdate = proc (currModel, mappingEvent) -> do 
      let rerenderEvent = fmap (const currModel) mappingEvent
          currLayered = clientBoxedRendered currModel
          currBoxed = clientBoxedModel currModel 

      renderedEvent <- renderModelAtlases -< rerenderEvent 
      let rendered = event currLayered (copyAltases currLayered) renderedEvent 

      forceNF -< currModel { clientBoxedRendered = remakeAllBoxedModelLayers currBoxed rendered } 

    -- | Wire that works in two phases (no rendering request / rendering in progress)
    renderModelAtlases :: GameWire (Event ClientBoxedModel) (Event BoxedModelLayered)
    renderModelAtlases = switch phase1 
      where 
        phase1 :: GameWire (Event ClientBoxedModel) (Event BoxedModelLayered, Event (GameWire (Event ClientBoxedModel) (Event BoxedModelLayered)))
        phase1 = proc renderEvent -> do 
          renderedEvent <- never -< ()
          returnA -< (renderedEvent, dSwitch . phase2 . clientBoxedRerenderAtlases <$> renderEvent)

        phase2 :: GameWire () (Event (Either Text BoxedModelLayered)) -> GameWire (Event ClientBoxedModel) (Event BoxedModelLayered, Event (GameWire (Event ClientBoxedModel) (Event BoxedModelLayered)))
        phase2 renderer = proc rerenderEvent -> do 
          wasRequested <- holdJust . notYet -< rerenderEvent -- first occurence the request we are processing now
          renderedEvent <- filterEitherE . renderer -< ()
          returnA -< (renderedEvent, nextPhase wasRequested <$> renderedEvent)
          where
            nextPhase Nothing = const $ dSwitch phase1 -- no request while rendering, going to rest
            nextPhase (Just bm) = const $ dSwitch . phase2 . clientBoxedRerenderAtlases $ bm -- requst while rendering, again...

    isModelChunkMsg mid (BoxedModelChunk i _ _) = fromCounter i == mid 
    isModelChunkMsg _ _ = False

    isModelMappingMsg mid (BoxedModelMapping i _) = fromCounter i == mid 
    isModelMappingMsg _ _ = False 

    updateChunkEvent cm msg = case fromBoxedModelChunkMsg msg of 
      Nothing -> cm 
      Just (_, (x,y,z), ch) -> 
          clientLayeredUpdates (z-1) (\bm l lz -> makeBoxedModelLayerChunk bm (x:.y:.()) lz l) -- could be a chunk border
        . clientLayeredUpdates z (\bm l lz -> makeBoxedModelLayerChunk bm (x:.y:.()) lz l)
        . clientBoxedUpdate cm $ \m -> modelUpdateChunkUnsafe x y z ch m

    updateMapping cm msg = case msg of 
      (BoxedModelMapping _ mp) -> clientBoxedUpdateM cm $ \m -> modelUpdateBlockMappingUnsafe mp m
      _ -> return cm 

    updateChunkEvents e m = event m (F.foldl' updateChunkEvent m) e 
    updateMappingsE e m = event m id e

    updateMappings :: GameWire (Event ([NetworkMessage], ClientBoxedModel)) (Event ClientBoxedModel)
    updateMappings = onEventM (\(msgs, m) -> F.foldlM updateMapping m msgs)

    singleBlocksUpdate :: BoxedModelId -> GameWire ClientBoxedModel ClientBoxedModel
    singleBlocksUpdate mid = onlyBlock . onlyMeta . blockAndMeta 
      where 
        onlyBlock = proc m -> do
          es <- clientBoxedModelBlockUpdate mid -< ()
          returnA -< event m (onlyBlock' m) es 
        onlyMeta = proc m -> do
          es <- clientBoxedModelMetaUpdate mid -< ()
          returnA -< event m (onlyMeta' m) es 
        blockAndMeta = proc m -> do
          es <- clientBoxedModelBlockAndMetaUpdate mid -< ()
          returnA -< event m (blockAndMeta' m) es 

        onlyBlock' cm msgs = F.foldl' go cm msgs
          where go acc (pos@(x:.y:.z:.()), bi) = 
                    clientLayeredUpdate (\m l -> makeLayeredSingleBlock m pos l)
                  $ clientBoxedUpdate acc (setModelBlockUnsafe x y z bi)

        onlyMeta' cm msgs = F.foldl' go cm msgs
          where go acc (pos@(x:.y:.z:.()), bmeta) = 
                    clientLayeredUpdate (\m l -> makeLayeredSingleBlock m pos l)
                  $ clientBoxedUpdate acc (setModelBlockMeta x y z bmeta)

        blockAndMeta' cm msgs = F.foldl' go cm msgs
          where go acc (pos@(x:.y:.z:.()), bi, bmeta) = 
                    clientLayeredUpdate (\m l -> makeLayeredSingleBlock m pos l)
                  $ clientBoxedUpdate acc (setModelBlockAndMetaUnsafe x y z bi bmeta)