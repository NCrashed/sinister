{-# LANGUAGE DeriveGeneric, RecordWildCards, MultiWayIf, TupleSections #-}
module Game.Boxed.Block(
    Block(..)
  , BlockMeta
  , BlockId
  , blockSize 
  , blockAllDiffuseTextures
  , blockAllNormalTextures
  -- | List of all known blocks
  , standartBlocks
  -- | Floor block
  , floorBlock 
  , floorBlockCovered
  , floorBlockUncovered
  -- | Wall block
  , wallBlock
  , thinWallBlock
  ) where 

import Game.Boxed.Direction

import GHC.Generics (Generic)
import Control.DeepSeq 

import Data.Int 
import Data.Hashable 

import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid

import Text.Printf

type BlockMeta = Int32 
type BlockId = Int32 
type ZOffset = Int 

-- | Size of block in world coordinates
blockSize :: Floating a => a  
blockSize = 1.0

-- | Kind of block that is used in chunk
data Block = Block {
    blockName :: !Text
  -- | Textures of block could vary with blocks around, block state and Z offset within rendering plane
  , blockTextures :: EachSide (Maybe (Block, BlockMeta)) -- ^ Blocks around the block, if there is no block on a side, lookup will return nothing
    -> BlockMeta -- ^ Meta-data of block 
    -> ZOffset -- ^ Z offset, 0 is rendering plane, 1 is block one way down and e.t.c (you should never get negative offset as it woudn't be rendered)
    -> (Text, Text) -- ^ Resulting info about tile
  -- | Pairs of all used diffuse and normal textures
  , blockAllTextures :: [(Text, Text)]
  } deriving (Generic)

instance NFData Block 

instance Show Block where 
  show (Block{..}) = unwords ["Block", T.unpack blockName]

-- | Equality by block name
instance Eq Block where 
  b1 == b2 = blockName b1 == blockName b2 

-- | Hashing only block name
instance Hashable Block where 
  hashWithSalt s = hashWithSalt s . blockName 

-- | Returns block all diffuse textures in corresponding order with normal textures
blockAllDiffuseTextures :: Block -> [Text]
blockAllDiffuseTextures = fmap fst . blockAllTextures

-- | Returns block all normal textures in corresponding order with diffuse textures
blockAllNormalTextures :: Block -> [Text]
blockAllNormalTextures = fmap snd . blockAllTextures

-- | Blocks that are statically known 
standartBlocks :: [Block]
standartBlocks = [floorBlock, wallBlock, thinWallBlock]

-- | Debug block that represents floor of ship level
floorBlock :: Block 
floorBlock = Block {
  blockName = "Debug floor"
, blockTextures = \_ meta z -> if 
    | z == 1 -> floorUp
    | meta == floorBlockCovered -> floor01
    | otherwise -> floor02
, blockAllTextures = [floor02, floor01, floorUp]
} where 
  floor02 = ("tiles:floor_02.png", "tiles:floor_02_normal.png")
  floor01 = ("tiles:floor_01.png", "tiles:floor_01_normal.png")
  floorUp = ("tiles:floor_up.png", "tiles:floor_up_normal.png")

floorBlockCovered :: BlockMeta 
floorBlockCovered = 1 

floorBlockUncovered :: BlockMeta 
floorBlockUncovered = 0 

wallBlock :: Block 
wallBlock = Block { 
  blockName = wallName
, blockTextures = renderer
, blockAllTextures = [("tiles:wall/wall_up.png", "tiles:wall/wall_normall.png")] ++
  [("tiles:wall/wall_" <> T.pack (printf "%02d" i) <> ".png", "tiles:wall/wall_normall.png") | i <- [0 :: Int .. 15]]
} where 
  wallName = "Debug wall"
  renderer sided _ z = if 
    | z == 0 -> mkWall "tiles:wall/wall_up.png"
    | isWall Forward && isWall Backward && isWall Rightward && isWall Leftward -> mkWall "tiles:wall/wall_13.png"
    | isWall Forward && isWall Backward && isWall Leftward -> mkWall "tiles:wall/wall_12.png"
    | isWall Backward && isWall Rightward && isWall Leftward -> mkWall "tiles:wall/wall_11.png"
    | isWall Forward && isWall Backward && isWall Rightward -> mkWall "tiles:wall/wall_10.png"
    | isWall Forward && isWall Rightward && isWall Leftward -> mkWall "tiles:wall/wall_09.png"
    | isWall Forward && isWall Leftward -> mkWall "tiles:wall/wall_08.png"
    | isWall Backward && isWall Leftward -> mkWall "tiles:wall/wall_07.png"
    | isWall Backward && isWall Rightward -> mkWall "tiles:wall/wall_06.png"
    | isWall Forward && isWall Rightward -> mkWall "tiles:wall/wall_05.png"
    | isWall Forward && isWall Backward -> mkWall "tiles:wall/wall_14.png"
    | isWall Rightward && isWall Leftward -> mkWall "tiles:wall/wall_15.png"
    | isWall Leftward -> mkWall "tiles:wall/wall_04.png"
    | isWall Backward -> mkWall "tiles:wall/wall_03.png"
    | isWall Rightward -> mkWall "tiles:wall/wall_02.png"
    | isWall Forward -> mkWall "tiles:wall/wall_01.png"
    | otherwise -> mkWall "tiles:wall/wall_00.png"
    where 
      mkWall = (, "tiles:wall/wall_normall.png")
      isWall s = case fromEachSide s sided of 
        Just (b, _) -> blockName b == wallName
        Nothing -> False

thinWallBlock :: Block 
thinWallBlock = Block { 
  blockName = wallName
, blockTextures = renderer
, blockAllTextures = [ ("tiles:wall2/wall_08.png", "tiles:wall/wall_normall.png")
                     , ("tiles:wall2/wall_09N.png", "tiles:wall/wall_normall.png")
                     , ("tiles:wall2/wall_08.png", "tiles:wall/wall_normall.png")
                     , ("tiles:wall2/wall_15.png", "tiles:wall/wall_normall.png")] ++
  [("tiles:wall2/" <> T.pack (printf "%d" i) <> ".png", "tiles:wall2/wall_15N.png") | i <- [1 :: Int .. 9]]
} where 
  wallName = "Debug thin wall"
  renderer sided _ z = if 
    | z == 0 -> mkWall "tiles:wall2/wall_15.png"
    | isWall Forward && isWall Backward && isWall Rightward && isWall Leftward -> mkWall "tiles:wall2/7.png"
    | isWall Forward && isWall Backward && isWall Leftward -> mkWall "tiles:wall2/2.png"
    | isWall Backward && isWall Rightward && isWall Leftward -> mkWall "tiles:wall2/1.png"
    | isWall Forward && isWall Backward && isWall Rightward -> mkWall "tiles:wall2/wall_08.png"
    | isWall Forward && isWall Rightward && isWall Leftward -> mkWall "tiles:wall2/wall_09N.png"
    | isWall Forward && isWall Leftward -> mkWall "tiles:wall2/9.png"
    | isWall Backward && isWall Leftward -> mkWall "tiles:wall2/4.png"
    | isWall Backward && isWall Rightward -> mkWall "tiles:wall2/3.png"
    | isWall Forward && isWall Rightward -> mkWall "tiles:wall2/8.png"
    | isWall Forward && isWall Backward -> mkWall "tiles:wall2/5.png"
    | isWall Rightward && isWall Leftward -> mkWall "tiles:wall2/6.png"
    | isWall Leftward -> mkWall "tiles:wall2/6.png"
    | isWall Backward -> mkWall "tiles:wall2/5.png"
    | isWall Rightward -> mkWall "tiles:wall2/6.png"
    | isWall Forward -> mkWall "tiles:wall2/5.png"
    | otherwise -> mkWall "tiles:wall2/wall_15.png"
    where 
      mkWall = (, "tiles:wall/wall_normall.png")
      isWall s = case fromEachSide s sided of 
        Just (b, _) -> blockName b == wallName
        Nothing -> False