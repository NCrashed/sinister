module Graphics.Layered.DebugShip(
    debugShip
  , loadDebugShip
  ) where 

import Core 
import Prelude hiding (id, (.))
import FRP.Netwire

import Graphics.Layered.DSL
import Data.Vec as Vec 

loadDebugShip :: GameWire a (Event (Either Text LayeredModel))
loadDebugShip = loadLayered debugShip 

debugShip :: Layered
debugShip = do 
  name "Ship for debugging DSL"
  floorHeight 0.1

  floorDef "Roof" $ do 
    color orange
    renderer $ dynamicLight "media:ship.png" "media:normal.png"

    child "Decals1" $ do 
      color orange
      renderer $ dynamicLight "media:12.png" "media:normal.png"

    child "Decals2" $ do 
      color orange 
      renderer $ dynamicLight "media:13.png" "media:normal.png"

    child "Glow" $ do
      renderer $ noShading "media:glow.png"
  where 
    orange = Vec.map (/255) $ 47:.151:.0:.()