{-# LANGUAGE DeriveGeneric #-}
module Game.Boxed.Chunk(
    ChunkBlockX, ChunkBlockY, ChunkBlockZ
  , BlockId
  , chunkSize
  , chunkArea
  , chunkVolume
  , chunkWorldSize
  , chunkWorldArea
  , chunkWorldVolume
  , Chunk 
  , emptyChunk
  , getBlockId
  , setBlockId
  , getBlockMeta
  , setBlockMeta
  , getBlockIdAndMeta
  , setBlockIdAndMeta
  , foldlXYPlane
  ) where 

import Data.Vector.Unboxed as V
import Game.Boxed.Block

import GHC.Generics (Generic)
import Data.Serialize 
import Control.DeepSeq
import Data.Vector.Serialize()
import qualified Data.Foldable as F

-- | Synonym for block X coordinate local for a chunk for convinience
type ChunkBlockX = Int 
-- | Synonym for block Y coordinate local for a chunk for convinience
type ChunkBlockY = Int 
-- | Synonym for block Z coordinate local for a chunk for convinience
type ChunkBlockZ = Int 

-- | Chunk size on each axis
chunkSize :: Num a => a
chunkSize = 8 

-- | Area of chunk (count of blocks in one layer)
chunkArea :: Num a => a
chunkArea = chunkSize * chunkSize

-- | Volume of chunk (count of blocks in chunk)
chunkVolume :: Num a => a 
chunkVolume = chunkArea * chunkSize

-- | Length of chunk side in world coordinates
chunkWorldSize :: Floating a => a 
chunkWorldSize = fromIntegral (chunkSize :: Int) * blockSize 

-- | Area of chunk slice in world coordinates
chunkWorldArea :: Floating a => a 
chunkWorldArea = fromIntegral (chunkArea :: Int) * blockSize 

-- | Volume of chunk in world coordinates
chunkWorldVolume :: Floating a => a 
chunkWorldVolume = fromIntegral (chunkVolume :: Int) * blockSize 

data Chunk = Chunk {
    chunkData :: !(Vector BlockId)
  , chunkMetaData :: !(Vector BlockMeta)
} deriving (Generic, Show)

instance NFData Chunk 
instance Serialize Chunk 

-- | Creates new empty chunk
emptyChunk :: Chunk 
emptyChunk = Chunk v v
  where v = V.replicate chunkVolume 0 

-- | Calculates linear index
chunkIndex :: ChunkBlockX -> ChunkBlockY -> ChunkBlockZ -> Int 
chunkIndex x y z = x + y*chunkSize + z*chunkArea

-- | Returns id of block inside of chunk, doesn't check boundaries
getBlockId :: ChunkBlockX -> ChunkBlockY -> ChunkBlockZ -> Chunk -> BlockId
getBlockId x y z ch = chunkData ch `unsafeIndex` chunkIndex x y z

-- | Updates content of chunk 
setBlockId :: ChunkBlockX -> ChunkBlockY -> ChunkBlockZ -> BlockId -> Chunk -> Chunk
setBlockId x y z i ch = ch { chunkData = chunkData ch `unsafeUpd` [(chunkIndex x y z, i)]}

-- | Returns block metadata by coordinates
getBlockMeta :: ChunkBlockX -> ChunkBlockY -> ChunkBlockZ -> Chunk -> BlockMeta 
getBlockMeta x y z ch = chunkMetaData ch `unsafeIndex` chunkIndex x y z 

-- | Sets block metadata by coordinates
setBlockMeta :: ChunkBlockX -> ChunkBlockY -> ChunkBlockZ -> BlockMeta -> Chunk -> Chunk 
setBlockMeta x y z m ch = ch { chunkMetaData = chunkMetaData ch `unsafeUpd` [(chunkIndex x y z, m)]}

-- | Returns block id paired with it metadata at specified coordinates
getBlockIdAndMeta :: ChunkBlockX -> ChunkBlockY -> ChunkBlockZ -> Chunk -> (BlockId, BlockMeta)
getBlockIdAndMeta x y z = (,) <$> getBlockId x y z <*> getBlockMeta x y z 

-- | Sets block id and metadata by coordinates
setBlockIdAndMeta :: ChunkBlockX -> ChunkBlockY -> ChunkBlockZ -> BlockId -> BlockMeta -> Chunk -> Chunk 
setBlockIdAndMeta x y z i m = setBlockMeta x y z m . setBlockId x y z i 

-- | Iterates over given z layer of chunk
foldlXYPlane :: ChunkBlockZ -> (ChunkBlockX -> ChunkBlockY -> BlockId -> BlockMeta -> a -> a) -> a -> Chunk -> a 
foldlXYPlane z_ f a0 ch = F.foldl' (\accX x -> F.foldl' (\accY y -> goXY accY x y) accX [0 .. chunkSize-1]) a0 [0 .. chunkSize-1]
  where 
    z = max 0 $ min (chunkSize-1) z_
    goXY acc x y = f x y (getBlockId x y z ch) (getBlockMeta x y z ch) acc