{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, Arrows, DeriveDataTypeable, RecordWildCards, TupleSections, DeriveGeneric #-}
module Graphics.Render.Boxed.Model( 
    BoxedModelLayered
  , emptyBoxedModelLayered
  -- | Atlases
  , makeBoxedModelAtlases
  , copyAltases
  , blitBoxedModelAtlases
  , makeAndBlitBoxedModelAtlases
  -- | Rendering chunks
  , makeBoxedModelLayerChunk
  , makeLayeredSingleBlock
  , makeAllBoxedModelLayer
  , remakeAllBoxedModelLayers
  -- | GPU rendering
  , renderLayeredBoxedModel
  ) where 

import Core 
import Prelude hiding (id, (.))
import FRP.Netwire 

import Game.Boxed.Model
import Game.Boxed.Chunk 
import Game.Boxed.Block 
import Game.Boxed.Direction

import Graphics.GPipe 
import Graphics.Texture.Atlas
import Graphics.Light 
import Graphics.Camera2D
import Graphics.Render.Camera
import Graphics.Render.Light

import qualified Data.HashMap.Strict as M
import qualified Data.Foldable as F 
import qualified Data.Sequence as S
import Data.Sequence ((|>))
import Data.Maybe 
import Data.Typeable
import Control.DeepSeq
import GHC.Float
import GHC.Generics (Generic)
import Util.Vec()
import Data.Vec as Vec 
import Data.List as L

import Data.Text (Text)
import TextShow 

data BoxedModelLayered = BoxedModelLayered {
  -- | Atlas of diffuse textures of tiles 
  layeredDiffuseAtlas :: Atlas
  -- | Atlas of normal textures of tiles
, layeredNormalAtlas :: Atlas 
  -- | Layers indexed by Z coordinate
, layeredLayers :: M.HashMap Int BoxedModelLayer
  -- | Translation in world
, layeredPos :: Vec3 Float 
  -- | Rotation in world (around z axis)
, layeredRot :: Float 
} deriving (Generic, Typeable)

instance NFData BoxedModelLayered

instance TextShow BoxedModelLayered where 
  showb (BoxedModelLayered{..}) = unlinesB $ [ "BoxedModelLayered {"
    , unwordsB ["\tdiffuseAtlas dirty:", showb $ isAtlasModified layeredDiffuseAtlas]
    , unwordsB ["\tnormalAtlas dirty:", showb $ isAtlasModified layeredNormalAtlas]
    , unwordsB ["\tpos:", showb layeredPos]
    , unwordsB ["\tpos:", showb layeredRot]
    , unwordsB ["\tlayers count:", showb $ M.size layeredLayers]
    ] <> fmap (\(z, l) -> unwordsB ["\tlayer", showb z, ":", printLayerInfo l]) (M.toList layeredLayers) 
    <> ["}"]
    where 
    printLayerInfo :: BoxedModelLayer -> Builder 
    printLayerInfo (BoxedModelLayer{..}) = unlinesB $ ["{"
      , unwordsB ["\t\tlayer chunks count:", showb $ M.size layerMesh]
      ] <> fmap (\(xy, m) -> unwordsB ["\t\tchunk ", showb xy, " verticies:", showb $ S.length $ unMesh2D m]) (M.toList layerMesh)
      <> ["\t}"]

emptyBoxedModelLayered :: Vec2 Int -- ^ Size of atlas element (one tile size)
  -> BoxedModelLayered
emptyBoxedModelLayered tileSize = BoxedModelLayered {
  layeredDiffuseAtlas = emptyAtlas tileSize
, layeredNormalAtlas = emptyAtlas tileSize 
, layeredLayers = M.empty 
, layeredPos = 0 
, layeredRot = 0
}

-- | For given Z layer returns list of layers that have z less or equal Z, maximum layers is count
boxedLayeredLayersForZ :: Int -- ^ Z level to compare with layers
  -> Int -- ^ Maximum count of layers to return
  -> BoxedModelLayered -- ^ Where layers are stored
  -> [BoxedModelLayer] -- ^ Layers which z level is less or equal first parameter
boxedLayeredLayersForZ z count (BoxedModelLayered{..}) = 
  L.take count $ fmap snd $ filter ((<= z).fst) $ L.reverse $ sortOn fst $ M.toList layeredLayers

newtype Mesh2D = Mesh2D { unMesh2D :: S.Seq (Vec3 Float, Vec2 Float, Vec2 Float) }
  deriving (Generic, Monoid, Typeable, Show)

instance NFData Mesh2D 

-- | Transfers Mesh2D to GPU
mesh2GPU :: Mesh2D -> PrimitiveStream Triangle (Vec3 (Vertex Float), Vec2 (Vertex Float), Vec2 (Vertex Float))
mesh2GPU (Mesh2D s) = toGPUStream TriangleList . F.toList $ s 

-- | Single layer of rendered boxed model
data BoxedModelLayer = BoxedModelLayer {
-- | Flat meshes that contains two triangles for each tile, each mesh corresponds to one chunk
  layerMesh :: M.HashMap (Int, Int) Mesh2D
} deriving (Generic, Typeable, Show)

instance NFData BoxedModelLayer 

-- | Creates empty boxed layer 
emptyBoxedModelLayer :: BoxedModelLayer 
emptyBoxedModelLayer = BoxedModelLayer {
  layerMesh = M.empty
}

-- | Makes generic altas from block description
boxedModelAtlas :: (Block -> [Text]) 
  -> Vec2 Int -- ^ Size of atlas element (one tile size)
  -> BoxedModel -- ^ Boxed model to take tiles from
  -> Atlas -- ^ Atlas, need to be rendered
boxedModelAtlas texGetter tileSize = F.foldl' addBlockToAtlas (emptyAtlas tileSize) . boxedModelBlocks 
  where 
    addBlockToAtlas :: Atlas -> Block -> Atlas
    addBlockToAtlas atlas = updateAtlas atlas . texGetter

-- | Make atlas of diffuse textures of tiles
boxedModelDiffuseAtlas :: Vec2 Int -- ^ Size of atlas element (one tile size)
  -> BoxedModel -- ^ Boxed model to take tiles from
  -> Atlas -- ^ Atlas, need to be rendered
boxedModelDiffuseAtlas = boxedModelAtlas blockAllDiffuseTextures

-- | Make atlas of normal textures of tiles
boxedModelNormalAtlas :: Vec2 Int -- ^ Size of atlas element (one tile size)
  -> BoxedModel -- ^ Boxed model to take tiles from
  -> Atlas -- ^ Atlas, need to be rendered
boxedModelNormalAtlas = boxedModelAtlas blockAllNormalTextures

-- | Updates all atlases of layer 
makeBoxedModelAtlases :: Vec2 Int -- ^ Size of tile in pixels
  -> BoxedModel -- ^ Model to render 
  -> BoxedModelLayered -- ^ Where to save updated atlases
  -> BoxedModelLayered
makeBoxedModelAtlases tileSize bm layer = layer {
  layeredDiffuseAtlas = boxedModelDiffuseAtlas tileSize bm 
, layeredNormalAtlas = boxedModelNormalAtlas tileSize bm
} 

-- | Copies atlases from second to first layered
copyAltases :: BoxedModelLayered -> BoxedModelLayered -> BoxedModelLayered
copyAltases to from = to {
    layeredDiffuseAtlas = layeredDiffuseAtlas from 
  , layeredNormalAtlas = layeredNormalAtlas from 
  } 

makeBoxedModelChunkSlice :: 
     Atlas -- ^ Diffuse atlas of tiles
  -> Atlas -- ^ Normal atls of tiles
  -> ChunkX -> ChunkY -> BlockZ -- ^ Global z layer to slice
  -> BoxedModel -- ^ Model to render chunk from
  -> Mesh2D -- ^ Resulting mesh
makeBoxedModelChunkSlice atlasDiffuse atlasNormal chunkX chunkY layerZ boxed = let 
    zeroLayer = modelFoldlXYChunk chunkX chunkY layerZ (bakeTile layerZ) mempty boxed
    eyeLayerZ = layerZ + 1
    eyeLayer = modelFoldlXYChunk chunkX chunkY eyeLayerZ (bakeTile eyeLayerZ) zeroLayer boxed
    in eyeLayer
  where 
    bakeTile :: BlockZ -> BlockX -> BlockY -> Block -> BlockMeta -> Mesh2D -> Mesh2D
    bakeTile zi xi yi block blockMeta m = fromMaybe m $ do
      let zoffset = zi - layerZ
          (diffuseTex, normalTex) = (blockTextures block) blocksAround blockMeta zoffset
      (uvx0:.uvy0:.(), uvW:.uvH:.()) <- textureUvInAtlas atlasDiffuse diffuseTex
      (uvxn0:.uvyn0:.(), uvnW:.uvnH:.()) <- textureUvInAtlas atlasNormal normalTex
      let uvx1 = uvx0 + uvW
          uvy1 = uvy0 + uvH
          uvxn1 = uvxn0 + uvnW
          uvyn1 = uvyn0 + uvnH 
      {-
      In chunk:
      ^ Y
      |
      | v3 ----- v2
      | |         |
      | |  block  |
      | |         |
      | v0 ----- v1
      |
      0--------------> X
      First triangle : v0, v2, v3
      Second triangle : v0, v1, v2

      In uv:
      0--------------> X
      |
      | u0 ------- 
      | |         |
      | |  block  |
      | |         |
      |  ------- u1
      |
      V Y
      -}
      let x0 = double2Float $ fromIntegral (xi - chunkX * chunkSize) * blockSize 
          y0 = double2Float $ fromIntegral (yi - chunkY * chunkSize) * blockSize 
          x1 = x0 + 1 
          y1 = y0 + 1 
          sameLevelParallax = 0.01
          z  = double2Float $ fromIntegral layerZ * blockSize + fromIntegral zoffset * sameLevelParallax
          d  = 0.001 -- Calibrating gaps
      let v0 = (x0    :.y0    :.z:.(), uvx0:.uvy1:.(), uvxn0:.uvyn1:.())
          v1 = ((x1+d):.y0    :.z:.(), uvx1:.uvy1:.(), uvxn1:.uvyn1:.())
          v2 = ((x1+d):.(y1+d):.z:.(), uvx1:.uvy0:.(), uvxn1:.uvyn0:.())
          v3 = (x0    :.(y1+d):.z:.(), uvx0:.uvy0:.(), uvxn0:.uvyn0:.())

      return $!! Mesh2D $ unMesh2D m 
        |> v0 |> v2 |> v3 -- triangle 1
        |> v0 |> v1 |> v2 -- triangle 2 
      where 
      blocksAround = EachSide {
        upward = modelBlockAndMeta xi yi (zi+1) boxed
      , downward = modelBlockAndMeta xi yi (zi-1) boxed
      , forward = modelBlockAndMeta (xi+1) yi zi boxed
      , backward = modelBlockAndMeta (xi-1) yi zi boxed
      , rightward = modelBlockAndMeta xi (yi-1) zi boxed
      , leftward = modelBlockAndMeta xi (yi+1) zi boxed
      }

-- | Renders single chunk of boxed model
-- Note: atlas should be updated separatly 
makeBoxedModelLayerChunk :: BoxedModel -- ^ Model to render
  -> Vec2 Int -- ^ index of chunk
  -> Int -- ^ Z layer
  -> BoxedModelLayered -- ^ Where to save prepared chunk
  -> BoxedModelLayered
makeBoxedModelLayerChunk bm (chx:.chy:.()) z layered = layered { 
    layeredLayers = (\l -> M.insert z l (layeredLayers layered))
      . updateLayer 
      . fromMaybe emptyBoxedModelLayer
      . M.lookup z . layeredLayers $ layered
  }
  where 
    updateLayer :: BoxedModelLayer -> BoxedModelLayer 
    updateLayer layer = layer {
      layerMesh = M.insert (chx, chy) (makeBoxedModelChunkSlice 
          (layeredDiffuseAtlas layered)
          (layeredNormalAtlas layered)
          chx chy z bm) 
          $ layerMesh layer
    }

-- | Updates single block in layered model (acutally rerenders chunk with it)
makeLayeredSingleBlock :: BoxedModel 
  -> Vec3 Int 
  -> BoxedModelLayered
  -> BoxedModelLayered
makeLayeredSingleBlock bm (x:.y:.z:.()) = 
    makeBoxedModelLayerChunk bm (chx:.chy:.()) (z-1)
  . makeBoxedModelLayerChunk bm (chx:.(chy-1):.()) (z-1) 
  . makeBoxedModelLayerChunk bm (chx:.(chy+1):.()) (z-1) 
  . makeBoxedModelLayerChunk bm ((chx+1):.chy:.()) (z-1) 
  . makeBoxedModelLayerChunk bm ((chx-1):.chy:.()) (z-1) 

  . makeBoxedModelLayerChunk bm (chx:.chy:.()) (z+1)
  . makeBoxedModelLayerChunk bm (chx:.(chy-1):.()) (z+1)
  . makeBoxedModelLayerChunk bm (chx:.(chy+1):.()) (z+1) 
  . makeBoxedModelLayerChunk bm ((chx+1):.chy:.()) (z+1) 
  . makeBoxedModelLayerChunk bm ((chx-1):.chy:.()) (z+1) 

  . makeBoxedModelLayerChunk bm (chx:.(chy-1):.()) z 
  . makeBoxedModelLayerChunk bm (chx:.(chy+1):.()) z 
  . makeBoxedModelLayerChunk bm ((chx+1):.chy:.()) z 
  . makeBoxedModelLayerChunk bm ((chx-1):.chy:.()) z 
  . makeBoxedModelLayerChunk bm (chx:.chy:.()) z 
  where (chx, chy, _) = chunkIndex x y z

-- | Renders all chunks in given XY slice
makeAllBoxedModelLayer :: BoxedModel -- ^ Model to render 
  -> Int -- ^ Z layer 
  -> BoxedModelLayer -- ^ Layer where stored rendered chunks
  -> BoxedModelLayered -- ^ Where to save prepared layer
  -> BoxedModelLayered 
makeAllBoxedModelLayer bm z layer layered = 
  F.foldl' updateLayered layered $ M.keys $ layerMesh layer
  where 
    updateLayered :: BoxedModelLayered -> (Int, Int) -> BoxedModelLayered
    updateLayered l (x,y) = makeBoxedModelLayerChunk bm (x:.y:.()) z l

-- | Rerenders all layers (neded when atlases are rerendered)
remakeAllBoxedModelLayers :: BoxedModel -> BoxedModelLayered -> BoxedModelLayered
remakeAllBoxedModelLayers bm layered = M.foldlWithKey' (\l z layer -> makeAllBoxedModelLayer bm z layer l) layered $ layeredLayers layered 

-- | Performs actual rendering of atlas for boxed model
blitBoxedModelAtlases :: BoxedModelLayered -> GameWire a (Event (Either Text BoxedModelLayered))
blitBoxedModelAtlases layered = proc _ -> do 
  diffAtlasE <- renderAtlasE (layeredDiffuseAtlas layered) -< ()
  normAtlasE <- renderAtlasE (layeredNormalAtlas layered) -< ()
  e <- waitEvents2 -< (diffAtlasE, normAtlasE)
  returnA -< fmap (\(ma, mb) -> do 
    a <- ma 
    b <- mb 
    return $! layered {
      layeredDiffuseAtlas = a 
    , layeredNormalAtlas = b
    }) e 

makeAndBlitBoxedModelAtlases :: Vec2 Int -> BoxedModel -> BoxedModelLayered -> GameWire a (Event (Either Text BoxedModelLayered))
makeAndBlitBoxedModelAtlases tileSize bm = blitBoxedModelAtlases . makeBoxedModelAtlases tileSize bm

renderLayeredBoxedModel :: 
  -- | Model to render
     BoxedModelLayered
  -- | Z level to render
  -> Int
  -- | View camera of player
  -> Camera2D
  -- | Dynamic lights
  -> [Light Float]
  -- | Ambient RGB color, the fourth component is intensity
  -> Vec4 Float 
  -- | Viewport size
  -> Vec2 Int 
  -- | Stream of fragments with RGBA color plus depth info
  -> [FragmentStream (Color RGBAFormat (Fragment Float), FragmentDepth)]
renderLayeredBoxedModel l@(BoxedModelLayered{..}) z cam lights ambient size = 
  concat $ renderOneLayer <$> boxedLayeredLayersForZ z maximumLayers l
  where 
  maximumLayers = 10
  renderOneLayer layer = renderLayers layer (atlasTexture layeredDiffuseAtlas) (atlasTexture layeredNormalAtlas) (Vec.take n2 layeredPos) layeredRot
  renderLayers (BoxedModelLayer{..}) tex ntex trans2dGlobal rot2d = uncurry renderLayer <$> M.toList layerMesh
    where 
    renderLayer (chX, chY) mesh = storeDepth . texturise <$> rasterizeFront (transform <$> mesh2GPU mesh)
      where
      storeDepth = (, fragDepth)
      trans2d = trans2dGlobal + ((fromIntegral chX * chunkWorldSize):.(fromIntegral chY * chunkWorldSize):.())
      vpMat = vpMatrix (unCamera2D cam) size
      vpMatInv = fromMaybe Vec.identity . Vec.invert $ vpMat
      transform (pos, uv, uvn) = (cameraTransform2D cam trans2d rot2d size pos, (uv, uvn))
      texturise (uv, uvn) = enlightNormal' size tex ntex ambient 0 lights vpMatInv uv uvn