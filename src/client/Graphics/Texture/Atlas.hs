{-# LANGUAGE ExistentialQuantification, TupleSections, DeriveGeneric #-}
module Graphics.Texture.Atlas(
    Atlas()
  , SomeTexture(..)
  , isAtlasModified
  , atlasTexture
  , emptyAtlas
  , atlasShape
  , updateAtlas
  , renderAtlas
  , renderAtlasE
  , removeFromAtlas
  , textureUvInAtlas
  ) where

import Core 
import FRP.Netwire 
import Prelude hiding (lookup, id, (.))

import Graphics.GPipe
import Graphics.Texture.Render
import Graphics.Texture.Repa
import Assets.Manager
import Assets.Texture
import Data.HashMap as M
import Data.List (foldl')
import Control.Monad.Trans.Either as E
import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)

import GHC.Generics (Generic)
import Control.DeepSeq
import Util.Vec()
import Util.Texture()

import Data.Text (Text)

-- | Texture atlas, assumes that subtextures has same size (will resize smaller ones to fit ceil)  
data Atlas = Atlas 
  -- | Modified flag, if true, then final texture should be rendered again
  Bool
  -- | Element size in pixels 
  (Vec2 Int)
  -- | Map from texture resource name to position in the atlas 
  LookupTable 
  -- | Final texture of the atlas
  (Texture2D RGBAFormat)
  deriving (Generic)

instance NFData Atlas 

type LookupTable = Map Text SubtexPlace
type SubtexPlace = Vec2 Int
type AtlasShape = Vec2 Int

-- | Checks modified flag of the atlas
isAtlasModified :: Atlas -> Bool
isAtlasModified (Atlas f _ _ _) = f

-- | Gets altas current texture
atlasTexture :: Atlas -> Texture2D RGBAFormat
atlasTexture (Atlas _ _ _ tex) = tex

-- | Returns empty atlas with empty texture inside
emptyAtlas :: Vec2 Int -- ^ Atlas element size in pixels
  -> Atlas
emptyAtlas elemSize = Atlas False elemSize M.empty (fromFrameBufferColor RGBA8 (0:.0:.()) (newFrameBufferColorDepth (RGBA 0 0) 100))

-- | Returns atlas shape, how subtextures will be layered into final texture
atlasShape :: Atlas -> AtlasShape
atlasShape (Atlas _ _ lookt _) = formShape $ size lookt 

-- | Calculates square shape from number of subtextures
formShape :: Int -> AtlasShape
formShape n = let side = ceiling (sqrt (fromIntegral n :: Double)) in side :. side :. ()

-- | Finds place for new texture in atlas filled with n textures and provided shape
toPlace :: Int -> AtlasShape -> SubtexPlace
toPlace n (shx:.shy:.()) = (n `mod` shx):.((n `div` shx) `mod` shy):.()
 
-- | Adds new textures to the atlas
updateAtlas :: Atlas -> [Text] -> Atlas
updateAtlas (Atlas _ esize table tex) texs = Atlas True esize table'' tex
  where
    -- update old values
    table'  = snd $ mapAccum (\i _ -> (i+1, newPlace i) ) 0 table
    -- add new values
    table'' = snd $ foldl' (\(i, t) subtex -> (i+1, insert subtex (newPlace i) t)) (size table', table') texs'
    
    newPlace i = toPlace i (formShape newsize)
    texs' = Prelude.filter (\e -> not $ e `member` table) texs
    newsize = size table + length texs

-- | Removes textures from the atlas,
removeFromAtlas :: Atlas -> [Text] -> Atlas
removeFromAtlas (Atlas _ esize table tex) texs = Atlas True esize table' tex
  where
    table' = snd $ foldWithKey (\subtex _ (i, t)-> if subtex `member` delTable then (i, t) else (i+1, insert subtex (newPlace i) t)) (0, M.empty) table
    delTable = fromList $ Prelude.map (,()) texs
    newPlace i = toPlace i (formShape newsize)
    newsize = size table - foldl' (\i subtex -> if subtex `member` table then i+1 else i) 0 texs

-- | Retrieves textures from resource manager and rerenders atlas
renderAtlas :: Atlas -> ResourceManager -> EitherT Text IO (Atlas, ResourceManager)
renderAtlas oldAtlas@(Atlas modified esize table _) mng 
  | not modified = E.right (oldAtlas, mng)
  | otherwise    = do
    (texs, newmng) <- collect $ first loadTex <$> toList table
    atlasTex <- liftIO $ renderAtlasTexture esize texs (atlasShape oldAtlas)
    E.right (Atlas False esize table atlasTex, newmng)
  where
    loadTex :: Text -> ResourceManager -> EitherT Text IO (SomeTexture, ResourceManager)
    loadTex name mng' = first (\(TextureResource t) -> SomeTexture t) <$> getResource mng' name (Par2DRGBA RGBA8)
    
    collect :: [(ResourceManager -> EitherT Text IO (SomeTexture, ResourceManager), SubtexPlace)] -> EitherT Text IO ([(SomeTexture, SubtexPlace)], ResourceManager)
    collect = collect' mng []
      where
      collect' mng' _ [] = E.right ([], mng')
      collect' mng' acc [(action, place)] = do
        (tex, newmng) <- action mng'
        E.right ((tex, place) : acc, newmng) 
      collect' mng' acc ((action, place):es) = do
        (tex, mng'') <- action mng'
        collect' mng'' ((tex, place):acc) es

-- | Rendering atlas with new FRP API
renderAtlasE :: Atlas -> GameWire a (Event (Either Text Atlas))
renderAtlasE oldAtlas@(Atlas modified esize table _) 
  | not modified = mapE (const $ Right oldAtlas) . now 
  | otherwise = let  
    ts = toList table

    texResources :: GameWire a (Event (Either Text [TextureResource (Texture2D RGBAFormat)]))
    texResources = loadResources $ (,Par2DRGBA RGBA8).fst <$> ts

    attachPlaces :: Either Text [TextureResource (Texture2D RGBAFormat)] -> Either Text [(SomeTexture, SubtexPlace)]
    attachPlaces = fmap $ (`zip` fmap snd ts) . fmap (\(TextureResource t) -> SomeTexture t)

    renderFinal :: Either Text [(SomeTexture, SubtexPlace)] -> Either Text (Texture2D RGBAFormat)
    renderFinal = fmap $ \tps -> unsafePerformIO $ renderAtlasTexture esize tps (atlasShape oldAtlas)

    saveTexture :: Either Text (Texture2D RGBAFormat) -> Either Text Atlas
    saveTexture = fmap $ Atlas False esize table

    in mapE (saveTexture . renderFinal . attachPlaces) . texResources

-- | Calculates atlas region for subtexture. Coordinates are relative from 0 to 1.
-- Resulting vector consists of origin coordnates and width-height vector.
getPlaceRegion :: AtlasShape -> SubtexPlace -> (Vec2 Float, Vec2 Float)
getPlaceRegion (shx:.shy:.()) (ix:.iy:.()) = (ox:.oy:.(), sx:.sy:.())
  where
    sx = 1 / fromIntegral shx
    sy = 1 / fromIntegral shy
    ox = sx * fromIntegral ix
    oy = sy * fromIntegral iy

-- | Actually rerenders atlas inner texture
renderAtlasTexture :: Vec2 Int -> [(SomeTexture, SubtexPlace)] -> AtlasShape -> IO (Texture2D RGBAFormat)
renderAtlasTexture (esx:.esy:.()) texs shape@(shx:.shy:.()) =
  cacheTexture atlasSize $ blitTextures atlasSize $ second (getPlaceRegion shape) <$> texs
  where
    atlasSize = (esx*shx):.(esy*shy):.()

-- | If texture is located in the atlas then returns texture region.    
-- Resulting vector consists of origin coordnates and width-height vector.
textureUvInAtlas :: Atlas -> Text -> Maybe (Vec2 Float, Vec2 Float)
textureUvInAtlas atlas@(Atlas _ _ table _) texname = do
  place <- texname `lookup` table
  return $ getPlaceRegion (atlasShape atlas) place