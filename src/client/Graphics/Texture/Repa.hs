{-# LANGUAGE GADTs #-}
module Graphics.Texture.Repa(
    fromTexture
  , toTexture
  , cacheTexture
  ) where

import Graphics.GPipe 
import Graphics.Texture.Render 
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.ForeignPtr as R
import Data.Word
import Foreign.ForeignPtr

loadBuffer :: (ColorFormat c, CPUFormat c ~ CPUFormat4Comp) => Vec2 Int -> FrameBuffer c d s -> IO (ForeignPtr Word8)
loadBuffer size@(sx:.sy:.()) framebuffer= do
  fptr <- mallocForeignPtrBytes (sx*sy*4) :: IO (ForeignPtr Word8)
  withForeignPtr fptr $ getFrameBufferColor UnsignedInt8_8_8_8 size framebuffer
  return fptr
        
fromTexture :: Vec2 Int -> Texture2D RGBAFormat -> IO (R.Array R.F R.DIM3 Word8)
fromTexture size tex = R.fromForeignPtr (getShape size) <$> loadBuffer size framebuffer
  where
    framebuffer = renderTexture True tex (0:.0:.()) (1:.1:.())
    
    getShape :: Vec2 Int -> R.DIM3
    getShape (sx:.sy:.()) = R.ix3 sy sx 4 
      
toTexture :: R.Array R.F R.DIM3 Word8 -> IO (Texture2D RGBAFormat)
toTexture arr = withForeignPtr (R.toForeignPtr arr) $ \ptr -> newTexture UnsignedInt8_8_8_8 RGBA8 size [ptr]
  where
    size = let shs = reverse $ R.listOfShape $ R.extent arr in (shs !! 1) :. head shs :. ()

cacheTexture :: Vec2 Int -> Texture2D RGBAFormat -> IO (Texture2D RGBAFormat)
cacheTexture size tex = do
  let framebuffer = renderTexture True tex (0:.0:.()) (1:.1:.())
  ptr <- loadBuffer size framebuffer 
  withForeignPtr ptr $ \p -> newTexture UnsignedInt8_8_8_8 RGBA8 size [p]