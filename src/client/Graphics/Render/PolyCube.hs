module Graphics.Render.PolyCube(
      cubeFrameBuffer
    , cube
    , cubeSidePosX
    , cubeSideNegX
    , cubeSidePosY
    , cubeSideNegY
    , cubeSidePosZ
    , cubeSideNegZ
    , transformedCube
    , rasterizedCube
    ) where
    
import Graphics.GPipe
import Graphics.Camera
import Graphics.Render.Camera
import Data.Monoid
import Math.Quaternion 

import Graphics.Render.Common 

cube :: PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
cube = mconcat [cubeSidePosX, cubeSideNegX, cubeSidePosY, cubeSideNegY, cubeSidePosZ, cubeSideNegZ]

cubeSidePosX :: PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
cubeSidePosX = toGPUStream TriangleStrip $ zip3 [1:.0:.0:.(), 1:.1:.0:.(), 1:.0:.1:.(), 1:.1:.1:.()] (repeat (1:.0:.0:.()))    cubeUvCoords

cubeSideNegX :: PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
cubeSideNegX = toGPUStream TriangleStrip $ zip3 [0:.0:.1:.(), 0:.1:.1:.(), 0:.0:.0:.(), 0:.1:.0:.()] (repeat ((-1):.0:.0:.())) cubeUvCoords

cubeSidePosY :: PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
cubeSidePosY = toGPUStream TriangleStrip $ zip3 [0:.1:.1:.(), 1:.1:.1:.(), 0:.1:.0:.(), 1:.1:.0:.()] (repeat (0:.1:.0:.()))    cubeUvCoords

cubeSideNegY :: PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
cubeSideNegY = toGPUStream TriangleStrip $ zip3 [0:.0:.0:.(), 1:.0:.0:.(), 0:.0:.1:.(), 1:.0:.1:.()] (repeat (0:.(-1):.0:.())) cubeUvCoords

cubeSidePosZ :: PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
cubeSidePosZ = toGPUStream TriangleStrip $ zip3 [1:.0:.1:.(), 1:.1:.1:.(), 0:.0:.1:.(), 0:.1:.1:.()] (repeat (0:.0:.1:.()))    cubeUvCoords

cubeSideNegZ :: PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
cubeSideNegZ = toGPUStream TriangleStrip $ zip3 [0:.0:.0:.(), 0:.1:.0:.(), 1:.0:.0:.(), 1:.1:.0:.()] (repeat (0:.0:.(-1):.())) cubeUvCoords

cubeUvCoords :: [Vec2 Float]
cubeUvCoords = [0:.0:.(), 0:.1:.(), 1:.0:.(), 1:.1:.()]    

cubeFrameBuffer :: Texture2D RGBAFormat -> Camera -> Vec2 Int -> FrameBuffer RGBAFormat DepthFormat ()
cubeFrameBuffer tex cam size = paintSolidDepthAlpha (litCube tex cam size) emptyFrameBufferDepthAlpha

transformedCube :: Camera -> Vec2 Int -> PrimitiveStream Triangle (Vec4 (Vertex Float), (Vec3 (Vertex Float), Vec2 (Vertex Float)))
transformedCube cam size = fmap (cameraTransform cam 0 unitQuaternion size (0.01:.100:.()) (pi/6)) cube
    
rasterizedCube :: Camera -> Vec2 Int -> FragmentStream (Vec3 (Fragment Float), Vec2 (Fragment Float), FragmentDepth)
rasterizedCube cam size = rasterizeFront $ fmap storeDepth $ transformedCube cam size
    where
        storeDepth (posv@(_:._:.depth:.w:.()), (normv, uv)) = (posv, (normv, uv ,depth/w))

litCube :: Texture2D RGBAFormat -> Camera -> Vec2 Int -> FragmentStream (Color RGBAFormat (Fragment Float), FragmentDepth)
litCube tex cam size = fmap (enlight tex) $ rasterizedCube cam size
    
