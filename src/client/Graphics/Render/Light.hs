module Graphics.Render.Light(
    module L 
  , enlightNormal
  , enlightNormal'
  , enlightSimple
  ) where 

import Prelude as P hiding ((<*))
import Graphics.Light as L
import Graphics.GPipe
import Data.Vec as Vec
import Math.Vector 

-- | Transforms color with alpha-as-intensity to GPU color
liftVec4Color :: Vec4 Float -> Vec3 (Fragment Float)
liftVec4Color vcpu = Vec.take n3 vRGBA `smult` Vec.get n3 vRGBA
  where 
    vRGBA :: Vec4 (Fragment Float)
    vRGBA = toGPU vcpu 
    v `smult` s = Vec.map (*s) v 

enlightSimple ::
  -- | Diffuse texture
     Texture2D RGBAFormat
  -- | Ambient color, alpha is entensity
  -> Vec4 Float
  -- | Additional color modifier, the fourth component is intensity
  -> Vec4 Float
  -- | Fragment uv pos
  -> Vec2 (Fragment Float)
  -- | Resulting color
  -> Color RGBAFormat (Fragment Float)
enlightSimple tex ambientColor colorMod uv = RGBA fragColor fragColorA
  where 
    (RGBA diffuseColor diffuseColorA) = sample (Sampler Point Wrap) tex uv 
    ambient = liftVec4Color ambientColor 
    modifier = liftVec4Color colorMod
    itensity = ambient + 1

    fragColor :: Vec3 (Fragment Float)
    fragColor = (diffuseColor + modifier) * itensity
    fragColorA = diffuseColorA

-- | Performs dynamic lighting of fragments with given set of lights
enlightNormal :: Vec2 Int -- ^ Resolution of screen
  -- | Diffuse texture
  -> Texture2D RGBAFormat
  -- | Normal texture
  -> Texture2D RGBAFormat
  -- | Ambient color, alpha is entensity
  -> Vec4 Float
  -- | Additional color modifier, the fourth component is intensity
  -> Vec4 Float
  -- | Lights that are used to enlight
  -> [Light Float]
  -- | Inverse of VP matrix
  -> Mat44 Float 
  -- | Fragment uv pos
  -> Vec2 (Fragment Float)
  -- | Resulting color
  -> Color RGBAFormat (Fragment Float)
enlightNormal size tex ntex ambientColor colorMod lightsCPU vpInverse uv = 
  enlightNormal' size tex ntex ambientColor colorMod lightsCPU vpInverse uv uv

-- | More general function than enlightNormal, accepts different uvs for diffuse and normal textures
enlightNormal' :: Vec2 Int -- ^ Resolution of screen
  -- | Diffuse texture
  -> Texture2D RGBAFormat
  -- | Normal texture
  -> Texture2D RGBAFormat
  -- | Ambient color, alpha is entensity
  -> Vec4 Float
  -- | Additional color modifier, the fourth component is intensity
  -> Vec4 Float
  -- | Lights that are used to enlight
  -> [Light Float]
  -- | Inverse of VP matrix
  -> Mat44 Float 
  -- | Fragment uv pos for diffuse texture
  -> Vec2 (Fragment Float)
  -- | Fragment uv pos for normal texture
  -> Vec2 (Fragment Float)
  -- | Resulting color
  -> Color RGBAFormat (Fragment Float)
enlightNormal' size tex ntex ambientColor colorMod lightsCPU vpInverse uvDiff uvNorm = 
  P.foldl combineLights (RGBA 0 0) $ enlightLight <$> lightsCPU
  where 
    v `smult` s = Vec.map (*s) v

    combineLights (RGBA accRGB accAlpha) (RGBA lightRGB lightAlpha) =
      RGBA (accRGB + lightRGB)--(accRGB `smult` (1 - accAlpha) + lightRGB `smult` lightAlpha) 
           (accAlpha + (1 - accAlpha)*lightAlpha)

    enlightLight lightCPU = RGBA fragColor fragColorA
      where 
      (RGBA diffuseColor diffuseColorA) = sample (Sampler Point Wrap) tex uvDiff 
      (RGBA normalMap _) = sample (Sampler Point Wrap) ntex uvNorm 

      (xSize :. ySize :. ()) = toGPU $ Vec.map fromIntegral size 

      light :: Light (Fragment Float)
      light = toGPU lightCPU
      lightPos = lightPosition light 
      lColorRGB = lightColor light 
      power = lightPower light 
      powerLoss = lightPowerLoss light

      (fx:.fy:.fz:.fw:.()) = toGPU vpInverse `multmv` (
        (fragX/xSize * 2 - 1):.
        (fragY/ySize * 2 - 1):.
        (fragDepth * 2 - 1)  :. 
        1:.())

      fragmentPos :: Vec3 (Fragment Float)
      fragmentPos = ((fx/fw) :. (fy/fw) :. (fz/fw) :. ())

      lightDir = case light of 
        (DirectionalLight d _ _) -> d
        _ -> lightPos - fragmentPos

      dist = Vec.norm lightDir 
      n = Vec.normalize $ normalMap `smult` 2.0 - 1
      l = Vec.normalize lightDir

      diffuse :: Vec3 (Fragment Float)
      diffuse = (lColorRGB `smult` power) `smult` maxB (Vec.dot n l) 0.0

      ambient = liftVec4Color ambientColor 
      modifier = liftVec4Color colorMod

      attenuation = case light of 
        (ConeLight _ coneDir conAngle _ _ _) -> 
          let da = (negate l) `angleBetweenVecs` coneDir
              distAtten = 1.0 / (attentation powerLoss dist * (conAngle/2*pi))
          in ifB (da <* conAngle) distAtten
            -- Soft border, the formula is based on sigmoid function
             (let x = da - conAngle
                  p = 0.99
                  k = 100
                  in distAtten / (1 + exp (k*x + log ( (1-p)/p ))) )
        _ -> 1.0 / (attentation powerLoss dist)

      itensity = ambient + diffuse `smult` attenuation
      finalColor = (diffuseColor + modifier) * itensity

      fragColor :: Vec3 (Fragment Float)
      fragColor = finalColor
      fragColorA = diffuseColorA