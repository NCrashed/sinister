module Graphics.Render.Tilenazier(
    sliceTexture
  , sliceTextureToLayers
  ) where 

import Graphics.GPipe
import Graphics.Quad
import Control.Applicative

-- | Fragment shader slice texture with depth mask
sliceTexture :: (ColorFormat f) => Texture2D f -- ^ Original texture to slice
  -> Texture2D f -- ^ Alpha-depth texture (more brightly - less distance to camera)
  -> Float -- ^ Min value of distance [0 .. 1]
  -> Float -- ^ Max value of distance [0 .. 1]
  -> FragmentStream (Color RGBAFormat (Fragment Float))
sliceTexture tex depthTex minDepth maxDepth = texturise <$> rasterizeFront (transformedQuad 0)
  where 
    depthFromColor (RGBA (r:._) _) = 1 - r

    texturise uv = ifB (isInside uv) (texColor uv) (RGBA (0:.0:.0:.()) 0)
    texColor = toColor . fromColor 0 1 . sample (Sampler Point Wrap) tex 
    depthValue = depthFromColor . toColor . fromColor 0 1 . sample (Sampler Linear Wrap) depthTex

    isInside uv = (depthValue uv <=* toGPU maxDepth) &&* (depthValue uv >=* toGPU minDepth)

-- | Fragment shader to slice texture with depth mask to defined count of layers (uniformly)
sliceTextureToLayers :: (ColorFormat f) => Texture2D f -- ^ Original texture to slice
  -> Texture2D f -- ^ Alpha-depth texture (more brightly - less distance to camera)
  -> Int -- ^ Number of slices
  -> [FragmentStream (Color RGBAFormat (Fragment Float))]
sliceTextureToLayers tex depthTex count = slice <$> [0 .. count-1]
  where 
    stepDepth = 1 / fromIntegral count
    slice i = sliceTexture tex depthTex (stepDepth * fromIntegral i) (stepDepth * fromIntegral (i + 1))