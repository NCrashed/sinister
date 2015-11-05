module Graphics.Render.Mesh(
    meshFrameBuffer
  , meshFragmentStream
  ) where 

import Graphics.GPipe

import Graphics.Render.Common 
import Graphics.Render.Camera
import Graphics.Camera 
import Graphics.Mesh 

import Math.Quaternion

-- | Renders textured mesh with specified camera
meshFrameBuffer :: Camera -- ^ View camera
  -> Texture2D RGBAFormat -- ^ Texture to map to the mesh
  -> Vec3 Float -- ^ Mesh world position
  -> Quaternion Float  -- ^ Mesh rotation
  -> Mesh -- ^ Mesh to render
  -> Vec2 Int -- ^ Viewport size
  -- | Buffer with alpha and depth info, no stencil buffer
  -> FrameBuffer RGBAFormat DepthFormat ()
meshFrameBuffer cam tex p r mesh size = paintSolidDepthAlpha litMesh emptyFrameBufferDepthAlpha
  where 
    litMesh = meshFragmentStream cam tex p r mesh size

-- | Fragment shader for one mesh
meshFragmentStream :: Camera -- ^ View camera
  -> Texture2D RGBAFormat -- ^ Texture to map to the mesh
  -> Vec3 Float -- ^ Mesh world position
  -> Quaternion Float  -- ^ Mesh rotation
  -> Mesh -- ^ Mesh to render
  -> Vec2 Int -- ^ Viewport size
  -- | Stream of fragments with color and depth info
  -> FragmentStream (Color RGBAFormat (Fragment Float), FragmentDepth) 
meshFragmentStream cam tex p r mesh size = litMesh
  where 
    -- | Fragment shader that enlights mesh
    litMesh :: FragmentStream (Color RGBAFormat (Fragment Float), FragmentDepth) 
    -- ^ Stream carries pairs of color and depth value
    litMesh = enlight tex <$> rasterizedMesh

    -- | Fragment shader that produces fragments from vector shader 
    rasterizedMesh :: FragmentStream (Vec3 (Fragment Float), Vec2 (Fragment Float), FragmentDepth) 
    -- ^ Stream carries vector of normal, uv coordinate vector and depth value
    rasterizedMesh = storeDepth <$> rasterizeFront transformedMesh
      where storeDepth (vn, uv) = (vn, uv, fragDepth)

    -- | Vector shader 
    transformedMesh :: PrimitiveStream Triangle (Vec4 (Vertex Float), (Vec3 (Vertex Float), Vec2 (Vertex Float))) 
    -- ^ Stream of verticies that forms triangles, normals and uv coordinates are packed to additional data of the stream
    transformedMesh = trans <$> mesh2stream mesh
      where 
      trans (v, n, uv) = let
        (v', n') = cameraTransform cam p r size v n 
        in (v', (n', uv))

