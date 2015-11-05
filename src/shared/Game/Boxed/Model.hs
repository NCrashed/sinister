{-# LANGUAGE DeriveGeneric, RecordWildCards, TupleSections #-}
module Game.Boxed.Model(
    ChunkX, ChunkY, ChunkZ 
  , ChunkBlockX, ChunkBlockY, ChunkBlockZ
  , BlockX, BlockY, BlockZ
  , BoxedModel(..)
  , BoxedModelId(..)
  , emptyModel
  , chunkIndex
  , blockIndex
  , modelResetMappingDirtyFlag
  -- | Reading blocks
  , modelBlock
  , modelBlockMeta
  , modelBlockAndMeta
  , modelFoldlXYChunk
  -- | Updating blocks
  , setModelBlock
  , setModelBlock'
  , setModelBlockMeta
  , setModelBlockAndMeta
  , setModelBlockAndMeta'
  , fillModelBlock
  -- | Getting boxed data
  , boxedModelBlocks
  , modelBlockMapping
  , modelChunk
  , modelChunkNeighbours
  , modelXYChunkSlice
  -- | Unsafe operations
  , modelUpdateChunkUnsafe
  , modelUpdateBlockMappingUnsafe
  , setModelBlockUnsafe
  , setModelBlockAndMetaUnsafe
  -- | Network utilities
  , makeBlockMappingMessage
  , makeChunkMessages
  , fromBoxedModelChunkMsg
  , boxedModelChunkMsg
  ) where

import Core.Monad
import Core.Block
import Game.Boxed.Block
import Game.Boxed.Chunk
import Game.Boxed.Direction
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M 
import qualified Codec.Compression.GZip as GZ
import qualified Data.Serialize as S 
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Int 

import GHC.Generics (Generic)
import Control.DeepSeq as DS
import Data.Hashable
import Data.Vec as Vec 
import Data.Foldable as F
import Network.Protocol.Message
import Control.Monad 
import Util.Vec()

import Data.Text (Text)

-- | Synonym for block X coordinate for convinience
type BlockX = Int 
-- | Synonym for block Y coordinate for convinience
type BlockY = Int 
-- | Synonym for block Z coordinate for convinience
type BlockZ = Int 

-- | Synonym for chunk X coordinate in model for convinience
type ChunkX = Int 
-- | Synonym for chunk Y coordinate in model for convinience
type ChunkY = Int 
-- | Synonym for chunk Z coordinate in model for convinience
type ChunkZ = Int 

data BoxedModel = BoxedModel {
    modelId :: !BoxedModelId 
  , modelPos :: !(Vec3 Double)
  , modelRot :: !Double
  , modelName2Block :: !(HashMap Text Block)
  , modelBlock2Index :: !(HashMap Text BlockId)
  , modelIndex2Block :: !(HashMap BlockId Text)
  -- | Nested maps allows to make slices
  , modelChunks :: !(HashMap ChunkZ (HashMap (ChunkX, ChunkY) Chunk))
  , modelBlockCounter :: Int32
  -- | When True server knows when send mapping update
  , modelMappingChanged :: Bool
} deriving (Generic)

instance NFData BoxedModel

newtype BoxedModelId = BoxedModelId { unBoxedModelId :: Int } deriving (Eq, Show, Generic)

instance NFData BoxedModelId
instance Hashable BoxedModelId

emptyModel :: BoxedModelId -> BoxedModel 
emptyModel i = BoxedModel {
    modelId = i
  , modelPos = 0
  , modelRot = 0
  , modelName2Block = M.empty
  , modelBlock2Index = M.empty 
  , modelIndex2Block = M.empty 
  , modelChunks = M.empty 
  , modelBlockCounter = 1
  , modelMappingChanged = False 
  }

-- | Sets to False modelMappingChanged
modelResetMappingDirtyFlag :: BoxedModel -> BoxedModel
modelResetMappingDirtyFlag bm = bm { modelMappingChanged = False }

-- | Gets mapping from blockid to block for the boxed model
modelBlockMapping :: BoxedModel -> HashMap BlockId Block 
modelBlockMapping (BoxedModel{..}) = 
  fmap fromJust $
  M.filter isJust $
  (\s -> M.lookup s modelName2Block) <$> modelIndex2Block

chunkIndex :: BlockX -> BlockY -> BlockZ -> (ChunkX, ChunkY, ChunkZ)
chunkIndex x y z = (x `div` chunkSize, y `div` chunkSize, z `div` chunkSize)

blockIndex :: BlockX -> BlockY -> BlockZ -> (ChunkBlockX, ChunkBlockY, ChunkBlockZ)
blockIndex x y z = (x `mod` chunkSize, y `mod` chunkSize, z `mod` chunkSize)

-- | Returns boxed model chunk by coordinates
modelChunk :: ChunkX -> ChunkY -> ChunkZ -> BoxedModel -> Maybe Chunk 
modelChunk x y z = M.lookup (x,y) <=< M.lookup z . modelChunks

-- | Updates chunk at specified coordinates
setModelChunk :: ChunkX -> ChunkY -> ChunkZ -> Chunk -> BoxedModel -> BoxedModel 
setModelChunk x y z ch bm@(BoxedModel{..}) = bm {
  modelChunks = M.insert z (M.insert (x,y) ch $ M.lookupDefault M.empty z modelChunks) modelChunks
}

-- | Returns list of all chunks in model
modelChunksList :: BoxedModel -> [((ChunkX, ChunkY, ChunkZ), Chunk)]
modelChunksList (BoxedModel{..}) = F.concat $ (\(z, m) -> (\((x,y), ch) -> ((x,y,z), ch)) <$> M.toList m ) <$> M.toList modelChunks

-- | Read block id at specified coordinates, Nothing indicates that there is no block
modelBlock :: BlockX -> BlockY -> BlockZ -> BoxedModel -> Maybe Block 
modelBlock x y z bm = case modelChunk cx cy cz bm of 
  Nothing -> Nothing 
  Just c -> case getBlockId bx by bz c of 
    0 -> Nothing
    i -> do
      bname <- M.lookup i $ modelIndex2Block bm
      M.lookup bname $ modelName2Block bm
  where 
    (cx, cy, cz) = chunkIndex x y z 
    (bx, by, bz) = blockIndex x y z

-- | Returns meta for block at coordinates
modelBlockMeta :: BlockX -> BlockY -> BlockZ -> BoxedModel -> BlockMeta
modelBlockMeta x y z bm = case modelChunk cx cy cz bm of 
  Nothing -> 0 
  Just c -> getBlockMeta bx by bz c
  where 
    (cx, cy, cz) = chunkIndex x y z 
    (bx, by, bz) = blockIndex x y z

-- | Returns block and meta at specified coordinates, Nothing idicates that there is no block
modelBlockAndMeta :: BlockX -> BlockY -> BlockZ -> BoxedModel -> Maybe (Block, BlockMeta)
modelBlockAndMeta x y z bm = case modelChunk cx cy cz bm of 
  Nothing -> Nothing 
  Just c -> case getBlockId bx by bz c of 
    0 -> Nothing
    i -> do
      bname <- M.lookup i $ modelIndex2Block bm
      b <- M.lookup bname $ modelName2Block bm
      let m = getBlockMeta bx by bz c
      return (b, m)
  where 
    (cx, cy, cz) = chunkIndex x y z 
    (bx, by, bz) = blockIndex x y z

-- | Updates block id at specified block coordinates
setModelBlock' :: BlockX -> BlockY -> BlockZ -> Block -> BoxedModel -> (BoxedModel, BlockId)
setModelBlock' x y z b bm = setModelBlockAndMeta' x y z b 0 bm

-- | Updates block id at specified block coordinates
setModelBlock :: BlockX -> BlockY -> BlockZ -> Block -> BoxedModel -> BoxedModel
setModelBlock x y z b = fst . setModelBlock' x y z b

-- | Updates block id and meta data at specified block coodinates
setModelBlockAndMeta :: BlockX -> BlockY -> BlockZ -> Block -> BlockMeta -> BoxedModel -> BoxedModel 
setModelBlockAndMeta x y z b bmeta = fst . setModelBlockAndMeta' x y z b bmeta 

-- | Updates block id and meta data at specified block coodinates
setModelBlockAndMeta' :: BlockX -> BlockY -> BlockZ -> Block -> BlockMeta -> BoxedModel -> (BoxedModel, BlockId) 
setModelBlockAndMeta' x y z b bmeta bm = (newModel, bi)
  where 
    (cx, cy, cz) = chunkIndex x y z 
    (bx, by, bz) = blockIndex x y z

    (bi, dirtyFlag, newName2Block, newBlock2Index, newIndex2Block, newCounter) = case M.lookup (blockName b) (modelBlock2Index bm) of 
      Nothing -> let i = modelBlockCounter bm
        in ( i
           , True
           , M.insert (blockName b) b $ modelName2Block bm
           , M.insert (blockName b) i $ modelBlock2Index bm
           , M.insert i (blockName b) $ modelIndex2Block bm
           , i+1)
      Just i -> 
        ( i
        , False
        , modelName2Block bm
        , modelBlock2Index bm
        , modelIndex2Block bm
        , modelBlockCounter bm)

    newChunk = setBlockIdAndMeta bx by bz bi bmeta $ fromMaybe emptyChunk $ modelChunk cx cy cz bm

    newModel = setModelChunk cx cy cz newChunk $ bm {
        modelName2Block = newName2Block
      , modelBlock2Index = newBlock2Index
      , modelIndex2Block = newIndex2Block
      , modelBlockCounter = newCounter
      , modelMappingChanged = dirtyFlag
      }

-- | Updates block meta at specified location
-- If there is no chunk or block, does nothing
setModelBlockMeta :: BlockX -> BlockY -> BlockZ -> BlockMeta -> BoxedModel -> BoxedModel 
setModelBlockMeta x y z m bm = fromMaybe bm $ do
  ch <- modelChunk cx cy cz bm 
  let newChunk = setBlockMeta bx by bz m ch 
  return $! setModelChunk cx cy cz newChunk bm 
  where 
    (cx, cy, cz) = chunkIndex x y z 
    (bx, by, bz) = blockIndex x y z

-- | WARNING: UNSAFE DANGEROUS WASN"T TESTED, LEVON PLEASE TEST
fillModelBlock :: Block -> [(BlockX, BlockY, BlockZ)] -> BoxedModel -> BoxedModel
fillModelBlock b cs bm = newChunks $ bm {
    modelName2Block = newName2Block
  , modelBlock2Index = newBlock2Index
  , modelIndex2Block = newIndex2Block
  , modelBlockCounter = newCounter
  }
  where 
    (bi, newName2Block, newBlock2Index, newIndex2Block, newCounter) = case M.lookup (blockName b) (modelBlock2Index bm) of 
      Nothing -> let i = modelBlockCounter bm
        in ( i
           , M.insert (blockName b) b $ modelName2Block bm
           , M.insert (blockName b) i $ modelBlock2Index bm
           , M.insert i (blockName b) $ modelIndex2Block bm
           , i+1)
      Just i -> 
        ( i
        , modelName2Block bm
        , modelBlock2Index bm
        , modelIndex2Block bm
        , modelBlockCounter bm)

    newChunks :: BoxedModel -> BoxedModel
    newChunks bm' = F.foldl go bm' cs
      where 

        go acc (x,y,z) = let 
          (cx, cy, cz) = (chunkIndex x y z)
          (bx, by, bz) = blockIndex x y z

          newChunk :: Chunk
          newChunk = setBlockId bx by bz bi
                  $ fromMaybe emptyChunk
                  $ modelChunk cx cy cz acc

          in setModelChunk cx cy cz newChunk acc

-- | Inserts raw chunk into specified coordinates
-- Note: chunk id <-> block name mapping should be updated
-- separatly.
modelUpdateChunkUnsafe :: ChunkX -> ChunkY -> ChunkZ -> Chunk -> BoxedModel -> BoxedModel 
modelUpdateChunkUnsafe = setModelChunk

-- | Updates model id <-> block mapping
-- Note: new data will override old mapping if there is id/name collisions.
modelUpdateBlockMappingUnsafe :: [(BlockId, Text)] -> BoxedModel -> GameMonadG cntx BoxedModel
modelUpdateBlockMappingUnsafe pairs bm@(BoxedModel {..}) = do
  bs <- knownBlocks
  let (b2i, i2b, n2b) = F.foldl' (go bs) (modelBlock2Index, modelIndex2Block, modelName2Block) pairs
  return $!! bm {
      modelBlock2Index = b2i 
    , modelIndex2Block = i2b 
    , modelName2Block = n2b
    }
  where 
    go :: M.HashMap Text Block    
      -> (M.HashMap Text BlockId, M.HashMap BlockId Text, M.HashMap Text Block)
      -> (BlockId, Text)
      -> (M.HashMap Text BlockId, M.HashMap BlockId Text, M.HashMap Text Block)
    go blocks acc@(b2i, i2b, n2b) (bid, n) = fromMaybe acc $ do 
      block <- M.lookup n blocks -- ^ Find block in known blocks
      return $!! (
          M.insert n bid b2i
        , M.insert bid n i2b
        , M.insert n block n2b)

-- | Updates block id at specified block coodinates without updating of inner mapping
setModelBlockUnsafe :: BlockX -> BlockY -> BlockZ -> BlockId -> BoxedModel -> BoxedModel
setModelBlockUnsafe x y z bi = setModelBlockAndMetaUnsafe x y z bi 0

-- | Updates block id and meta data at specified block coodinates without updating of inner mapping
setModelBlockAndMetaUnsafe :: BlockX -> BlockY -> BlockZ -> BlockId -> BlockMeta -> BoxedModel -> BoxedModel
setModelBlockAndMetaUnsafe x y z bi bmeta bm = newModel
  where 
    (cx, cy, cz) = chunkIndex x y z 
    (bx, by, bz) = blockIndex x y z

    newChunk = setBlockIdAndMeta bx by bz bi bmeta $ fromMaybe emptyChunk $ modelChunk cx cy cz bm
    newModel = setModelChunk cx cy cz newChunk bm 


-- | Makes network message with model mapping
makeBlockMappingMessage :: BoxedModel -> NetworkMessage
makeBlockMappingMessage (BoxedModel{..}) = BoxedModelMapping (unBoxedModelId modelId) (M.toList modelIndex2Block)

-- | Makes set of messages carrying chunks
makeChunkMessages :: BoxedModel -> [NetworkMessage]
makeChunkMessages bm@(BoxedModel{..}) = uncurry makeChunkMessage <$> modelChunksList bm
  where 
    makeChunkMessage i ch = boxedModelChunkMsg modelId i ch

-- | Compress data of chunk
boxedModelChunkMsg :: BoxedModelId -> (ChunkX, ChunkY, ChunkZ) -> Chunk -> NetworkMessage
boxedModelChunkMsg bid coords = BoxedModelChunk (unBoxedModelId bid) coords . BSL.toStrict . GZ.compress . BSL.fromStrict . S.encode

-- | Decompress data of chunk
fromBoxedModelChunkMsg :: NetworkMessage -> Maybe (BoxedModelId, (ChunkX, ChunkY, ChunkZ), Chunk)
fromBoxedModelChunkMsg (BoxedModelChunk bid coords bs) = 
  DS.force . fmap (BoxedModelId bid, coords, ) . e2m . S.decode . BSL.toStrict . GZ.decompress . BSL.fromStrict $ bs
  where 
    e2m (Left _) = Nothing 
    e2m (Right x) = Just x 
fromBoxedModelChunkMsg _ = Nothing

-- | Returns all used blocks in boxed model
boxedModelBlocks :: BoxedModel -> [Block]
boxedModelBlocks = M.elems . modelName2Block

-- | Returns all neighbours of chunk by chunk coordinates
modelChunkNeighbours :: ChunkX -> ChunkY -> ChunkZ -> BoxedModel -> EachSide (Maybe Chunk)
modelChunkNeighbours x y z bm = EachSide { 
  upward = modelChunk x y (z+1) bm
, downward = modelChunk x y (z-1) bm 
, rightward = modelChunk x (y+1) z bm 
, leftward = modelChunk x (y-1) z bm 
, forward = modelChunk (x+1) y z bm 
, backward = modelChunk (x-1) y z bm
}

-- | Returns all chunks with given Z coordinate
modelXYChunkSlice :: Int -> BoxedModel -> [Vec2 Int]
modelXYChunkSlice z (BoxedModel{..}) = fmap (\(x,y) -> x:.y:.()) $ maybe [] M.keys $ M.lookup z modelChunks

-- | Iterates over blocks for given chunk and z level (block coordinate absolute to model)
modelFoldlXYChunk :: ChunkX -> ChunkY -> BlockZ -> (BlockX -> BlockY -> Block -> BlockMeta -> a -> a) -> a -> BoxedModel -> a 
modelFoldlXYChunk chx chy z f acc0 bm = F.foldl' (\accX x -> F.foldl' (\accY y -> goXY accY x y) accX [chy * chunkSize .. (chy+1) * chunkSize - 1]) acc0 [chx * chunkSize .. (chx+1) * chunkSize - 1]
  where 
    goXY acc x y = fromMaybe acc $! do
      (block, meta) <- modelBlockAndMeta x y z bm
      return $! f x y block meta acc