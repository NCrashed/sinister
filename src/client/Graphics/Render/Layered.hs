module Graphics.Render.Layered(
    renderLayeredModel
  ) where 

import Prelude as P
import Graphics.Layered.Layer 
import Graphics.Layered.Model
import Graphics.GPipe
import Graphics.Camera2D
import Graphics.Light 
import Control.Applicative
import GHC.Float
import Data.Vec as Vec 

renderLayeredModel :: 
  -- | Model to render
     LayeredModel
  -- | Floor to render
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
renderLayeredModel m floorNum cam lights ambient size = 
  render (lmodelFloors m !! i) (double2Float $ fromIntegral i * lmodelFloorHeight m)
  where 
    i = max 0 $ min (P.length (lmodelFloors m) - 1) floorNum
    render l d = renderLayer l cam (Vec.map double2Float $ lmodelPos m) (double2Float $ lmodelRot m) 
      d lights ambient size 

renderLayer :: 
  -- | Layer to render
     Layer 
  -- | View camera of player
  -> Camera2D 
  -- | World translation of layer
  -> Vec3 Float 
  -- | World rotation of layer
  -> Float
  -- | Local Z depth of layer 
  -> Float
  -- | Dynamic lights
  -> [Light Float]
  -- | Ambient RGB color, the fourth component is intensity
  -> Vec4 Float 
  -- | Size of viewport
  -> Vec2 Int 
  -- | Fragment shader with depth info and alpha channel
  -> [FragmentStream (Color RGBAFormat (Fragment Float), FragmentDepth)]
renderLayer l c p r d ls a s = layerRenderer l c p r d ls a s : 
  (concat $ (\cl -> renderLayer cl c p r d ls a s) <$> P.reverse (layerChilds l)) 
