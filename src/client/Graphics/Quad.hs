module Graphics.Quad(
    remapCoords
  , transformedQuad
  , screenQuad
  ) where 

import Data.Vec 
import Graphics.GPipe

-- | Coordinates transformation from viewport system to specified local. This function maps points in particular
-- region into texture coordinates.
remapCoords :: Vec2 Float -> Vec2 Float -> Vec2 (Shader t Float) -> Vec2 (Shader t Float)
remapCoords (ox:.oy:.()) (sx:.sy:.()) (ux:.uy:.()) = ux':.uy':.()
  where
    ux' = (ux - toGPU ox) / toGPU sx
    uy' = (uy - toGPU oy) / toGPU sy

-- | Some trivial transformations for viewport quad.      
transformedQuad :: Float -> PrimitiveStream Triangle (Vec4 (Vertex Float), Vec2 (Vertex Float))
transformedQuad d = fmap homonize $ screenQuad d
  where 
    homonize :: (Vec3 (Vertex Float), Vec2 (Vertex Float)) -> (Vec4 (Vertex Float), Vec2 (Vertex Float))
    homonize (v,uv) = (homPoint v :: Vec4 (Vertex Float), uv)  

-- | Quad that covers all window        
screenQuad :: Float -> PrimitiveStream Triangle (Vec3 (Vertex Float), Vec2 (Vertex Float))
screenQuad d = toGPUStream TriangleList $ zip vecs uvs                                         
  where
    vecs = [(-1):.(-1):.d:.(), 1:.1:.d:.(), (-1):.1:.d:.(), (-1):.(-1):.d:.(), 1:.(-1):.d:.(), 1:.1:.d:.()]  
    uvs = [0:.1:.(), 1:.0:.(), 0:.0:.(), 0:.1:.(), 1:.1:.(), 1:.0:.()]                                          