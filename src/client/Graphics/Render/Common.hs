module Graphics.Render.Common(
      paintSolid
    , paintSolidAlpha
    , paintSolidDepth
    , paintSolidDepthAlpha
    , emptyFrameBuffer
    , emptyFrameBufferAlpha 
    , emptyFrameBufferDepth
    , emptyFrameBufferDepthAlpha
    , enlight
    , transform
    ) where
    
import Graphics.GPipe
import Data.Vec as Vec
    
paintSolid :: FragmentStream (Color RGBFormat (Fragment Float)) -> FrameBuffer RGBFormat () () -> FrameBuffer RGBFormat () ()
paintSolid = paintColor NoBlending (RGB $ vec True)

paintSolidAlpha :: FragmentStream (Color RGBAFormat (Fragment Float)) -> FrameBuffer RGBAFormat () () -> FrameBuffer RGBAFormat () ()
paintSolidAlpha = paintColor NoBlending (RGBA (vec True) True)

paintSolidDepth :: FragmentStream (Color RGBFormat (Fragment Float), FragmentDepth) -> FrameBuffer RGBFormat DepthFormat () -> FrameBuffer RGBFormat DepthFormat ()
paintSolidDepth = paintColorDepth Less True NoBlending (RGB $ vec True)

paintSolidDepthAlpha :: FragmentStream (Color RGBAFormat (Fragment Float), FragmentDepth) -> FrameBuffer RGBAFormat DepthFormat () -> FrameBuffer RGBAFormat DepthFormat ()
paintSolidDepthAlpha = paintColorDepth Less True blend (RGBA (vec True) True)
  where blend = Blend (FuncAdd, FuncAdd) ((SrcAlpha, OneMinusSrcAlpha), (SrcAlpha, OneMinusSrcAlpha)) (RGBA 0 0)
  
emptyFrameBuffer :: FrameBuffer RGBFormat () ()
emptyFrameBuffer = newFrameBufferColor (RGB 0)

emptyFrameBufferAlpha :: FrameBuffer RGBAFormat () () 
emptyFrameBufferAlpha = newFrameBufferColor (RGBA 0 0)

emptyFrameBufferDepth :: FrameBuffer RGBFormat DepthFormat ()
emptyFrameBufferDepth = newFrameBufferColorDepth (RGB 0) 100
  
emptyFrameBufferDepthAlpha :: FrameBuffer RGBAFormat DepthFormat ()
emptyFrameBufferDepthAlpha = newFrameBufferColorDepth (RGBA 0 0) 100
    
enlight :: Texture2D RGBAFormat -> (Vec3 (Fragment Float), Vec2 (Fragment Float), FragmentDepth) -> (Color RGBAFormat (Fragment Float), FragmentDepth)
enlight tex (normv, uv, depth) = (RGBA (c * vec (normv `dot` toGPU (normalize $ (-1):.1:.(-1):.()))) a, depth)
    where RGBA c a = sample (Sampler Point Wrap) tex uv
    
transform :: Float -> Vec2 Int -> (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float)) -> (Vec4 (Vertex Float), (Vec3 (Vertex Float), Vec2 (Vertex Float)))
transform angle (width:.height:.()) (pos, normv, uv) = (transformedPos, (transformedNorm, uv))
    where
    modelMat = rotationVec (normalize (1:.0.5:.0.3:.())) angle `multmm` translation (-0.5)
    viewMat  = translation (-(0:.0:.2:.()))
    projMat  = perspective 1 100 (pi/3) (fromIntegral width / fromIntegral height)
    viewProjMat = projMat `multmm` viewMat
    transformedPos  = toGPU (viewProjMat `multmm` modelMat) `multmv` (homPoint pos :: Vec4 (Vertex Float))
    transformedNorm = toGPU (mat4ToMat3 modelMat) `multmv` normv
    mat4ToMat3 = Vec.map (Vec.take n3) . Vec.take n3    