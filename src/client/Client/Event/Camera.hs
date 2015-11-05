{-# LANGUAGE Arrows #-}
module Client.Event.Camera(
    mouseWorldPos
  , mouseWorldPosZ
  , mouseWorldClick
  , mouseWorldClickZ
  ) where 

import Core 
import Prelude hiding (id, (.))
import FRP.Netwire 
import Control.Wire.Unsafe.Event 

import Graphics.Camera
import Graphics.Render.Camera 
import Data.Vec as Vec 
import Util.Vec()
import Data.Maybe 
import GHC.Float

-- | Returns current position of mouse cursor in world coodinates for XY plane with zero Z
mouseWorldPos :: HasCamera a => GameWire a (Vec2 Double)
mouseWorldPos = mouseWorldPosZ . arr (\c -> (c, 0))

-- | Returns current position of mouse cursor in world coodinates for specified Z plane
mouseWorldPosZ :: HasCamera a => GameWire (a, Double) (Vec2 Double)
mouseWorldPosZ = loop $ proc ((c, z), p_) -> do 
  p  <- delay 0 -< p_
  size <- viewportSize -< ()
  ep <- centeredMouseMove -< ()
  let vp = Vec.map (Vec.map float2Double) $ vpMatrix (getCamera c) size 
      vpInv = fromMaybe Vec.identity . Vec.invert $ vp
      screenP = event p id ep
      clipZ = Vec.get n2 . project . (vp `multmv`) . homPoint $ 0:.0:.z:.()
      worldP = homTransform vpInv clipZ screenP
  forceNF -< (worldP, screenP)

homTransform :: Mat44 Double -> Double -> Vec2 Double -> Vec2 Double 
homTransform m z = Vec.take n2 . project . (m `multmv`) . homPoint . (`Vec.snoc` z)

-- | Returns position of mouse click in world coordinates for XY plane with zero Z
mouseWorldClick :: HasCamera a => MouseButton -> KeyState -> GameWire a (Event (Vec2 Double, Modifiers))
mouseWorldClick mb ks = mouseWorldClickZ mb ks . arr (\c -> (c, 0))

-- | Retyrbs position of mouse click in world coordinates for specifiex XY plane (Z value)
mouseWorldClickZ :: HasCamera a => MouseButton -> KeyState -> GameWire (a, Double) (Event (Vec2 Double, Modifiers))
mouseWorldClickZ mb ks = proc (c, z) -> do
  size <- viewportSize -< ()
  screenP <- centeredMouseClick mb ks -< ()
  let vp = Vec.map (Vec.map float2Double) $ vpMatrix (getCamera c) size 
      vpInv = fromMaybe Vec.identity . Vec.invert $ vp
      clipZ = Vec.get n2 . project . (vp `multmv`) . homPoint $ 0:.0:.z:.()
      worldP = first (homTransform vpInv clipZ) <$> screenP
  forceNF -< worldP
