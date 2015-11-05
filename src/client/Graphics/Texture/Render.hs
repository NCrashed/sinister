{-# LANGUAGE ExistentialQuantification, TypeFamilies #-}
module Graphics.Texture.Render(
    renderTexture
  , textureQuad
  , blitTexture
  , SomeTexture(..)
  , blitTextures
  , remapCoords
  ) where
  
import Prelude as P hiding ((<*))
import Graphics.GPipe
import Graphics.Quad
import Graphics.Render.Common
import Data.List

-- | Incapsulates textures with different color formats
data SomeTexture = forall f . (ColorFormat f) => SomeTexture (Texture2D f)

-- | Combines a number of textures into one.  
blitTextures :: Vec2 Int -- ^ Size of required final texture, all textures auto-resized 
  -> [(SomeTexture, (Vec2 Float, Vec2 Float))] -- ^ list of textures and their region pos and size in resulting textures, coordinates
                                             -- are relative from 0 to 1.
  -> Texture2D RGBAFormat
blitTextures buffSize [] = fromFrameBufferColor RGBA8 buffSize emptyFrameBufferAlpha
blitTextures buffSize [(SomeTexture tex, (origin, size))] = fromFrameBufferColor RGBA8 buffSize $ renderTexture True tex origin size
blitTextures buffSize (tex:texs) = foldl' combine (blitTextures buffSize [tex]) texs
  where
    combine :: Texture2D RGBAFormat -> (SomeTexture, (Vec2 Float, Vec2 Float)) -> Texture2D RGBAFormat
    combine acc (SomeTexture blitTex, (origin, size)) = fromFrameBufferColor RGBA8 buffSize $ blitTexture True acc blitTex origin size

-- | Renders texture in framebuffer with specified position and size. 
renderTexture :: (ColorFormat f) => Bool -- ^ Vertical invertion flat, gpipe has bug in converting framebuffer to texture that leads
                                         -- to flipping. Thus this flag flips framebuffer.
  -> Texture2D f -- ^ Texture to render 
  -> Vec2 Float  -- ^ Position of texture relative to top left corner of window. Coordinates takes values from 0 to 1. 
  -> Vec2 Float  -- ^ Size of texture in resulting framebuffer. Coordinates takes values from 0 to 1. 
  -> FrameBuffer RGBAFormat DepthFormat ()
renderTexture vflip tex orig size = paintSolidDepthAlpha (textureQuad vflip tex orig size) emptyFrameBufferDepthAlpha

-- | Blits a texture into another texture. Size shouldn't consists zeros.
blitTexture :: (ColorFormat f1, ColorFormat f2) =>
  Bool -- ^ Vertical invertion flat, gpipe has bug in converting framebuffer to texture that leads
       -- to flipping. Thus this flag flips framebuffer.
  -> Texture2D f1 -- ^ Base texture where second one is blitted 
  -> Texture2D f2 -- ^ Texture to blit
  -> Vec2 Float -- ^ Second texture position relative to top left corner of window. Coordinates takes values from 0 to 1. 
  -> Vec2 Float -- ^ Second texture size to be blitted. Coordinates takes values from 0 to 1. 
  -> FrameBuffer RGBAFormat DepthFormat ()
blitTexture vflip base tex origin size = paintSolidDepthAlpha (textureQuad' vflip base tex origin size) emptyFrameBufferDepthAlpha


-- | Fragment shader to blit one texture.
textureQuad :: (ColorFormat f) => Bool -> Texture2D f ->  Vec2 Float -> Vec2 Float 
  -> FragmentStream (Color RGBAFormat (Fragment Float), FragmentDepth)
textureQuad vflip tex origin@(ox:.oy:.()) size@(sx:.sy:.()) = storeDepth . texturise <$> rasterizeFront (transformedQuad 0)
  where 
    storeDepth uv = (uv, fragDepth)
    texturise uv@(uvx:.uvy:.()) = if vflip 
      then let uv' = (uvx:.(1-uvy):.()) in texturise' uv' 
      else texturise' uv
      
    texturise' uv = ifB (isInside uv) (texColor uv) (RGBA (0:.0:.0:.()) 1)
    texColor = toColor . fromColor 0 1 . sample (Sampler Linear Wrap) tex  . remapCoords origin size
    isInside (uvx:.uvy:.()) = uvx >=* toGPU ox &&* uvy >=* toGPU oy &&* uvx <* toGPU (ox+sx) &&* uvy <* toGPU (oy+sy)

-- | Fragment shader to blit two textures.    
textureQuad' :: (ColorFormat f1, ColorFormat f2) => Bool -> Texture2D f1 -> Texture2D f2
  -> Vec2 Float -> Vec2 Float
  -> FragmentStream (Color RGBAFormat (Fragment Float), FragmentDepth)
textureQuad' vflip base tex origin@(ox:.oy:.()) size@(sx:.sy:.()) = storeDepth . texturise <$> rasterizeFront (transformedQuad 0)
  where
    storeDepth uv = (uv, fragDepth)
    texturise uv@(uvx:.uvy:.()) = if vflip 
      then let uv' = (uvx:.(1-uvy):.()) in texturise' uv' 
      else texturise' uv
    texturise' uv = ifB (isInside uv) (newColor uv) (baseColor uv)
    baseColor = toColor . fromColor 0 1 . sample (Sampler Linear Wrap) base
    newColor  = toColor . fromColor 0 1 . sample (Sampler Linear Wrap) tex  . remapCoords origin size
    isInside (uvx:.uvy:.()) = uvx >=* toGPU ox &&* uvy >=* toGPU oy &&* uvx <* toGPU (ox+sx) &&* uvy <* toGPU (oy+sy)  