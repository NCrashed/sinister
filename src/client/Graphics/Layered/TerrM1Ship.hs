module Graphics.Layered.TerrM1Ship(
    terrM1Ship 
  , loadTerrM1Ship
  ) where 

import Core
import Prelude hiding (id, (.))
import FRP.Netwire
import Graphics.Layered.DSL 

loadTerrM1Ship :: GameWire a (Event (Either Text LayeredModel))
loadTerrM1Ship = loadLayered terrM1Ship 

terrM1Ship :: Layered 
terrM1Ship = do 
  name "Terran M1"
  floorHeight 0.5

  floorDef "Roof" $ do 
    renderer $ dynamicLight "ships:Terr_M1/Relikt_Re1.png" "ships:Terr_M1/Relikt_Normal.png"

    child "Decals1" $ do 
      renderer $ dynamicLight "ships:Terr_M1/Relikt_Re.png" "ships:Terr_M1/Relikt_Normal.png"

    child "Glow" $ do
      renderer $ noShading "ships:Terr_M1/Relikt_Glow.png"