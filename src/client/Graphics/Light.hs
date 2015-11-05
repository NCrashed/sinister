{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleInstances, TypeFamilies #-}
module Graphics.Light(
    Direction
  , Position
  , Power 
  , PowerLoss(..)
  , LightColor
  , ConeAngle 
  , ConeDirection
  , Light(..)
  , lightPosition
  , lightPower 
  , lightPowerLoss 
  , lightColor
  , lightDouble2Float
  , lightFloat2Double
  , transformLight
  , attentation
  ) where 

import Data.Vec as Vec
import Data.Typeable
import GHC.Generics (Generic)
import Control.DeepSeq 
import GHC.Float 
import Graphics.GPipe
import Util.Vec()

-- | Normalized vector that means direction
type Direction a = Vec3 a 
-- | Position of light source
type Position a = Vec3 a
-- | Power of light
type Power a = a
-- | Specific color of light (RGB)
type LightColor a = Vec3 a
-- | Angle of cone in radians
type ConeAngle a = a
-- | Direction of cone in radians
type ConeDirection a = Vec3 a 

-- | Loss of light power per distance unit, forming formula: 1 + k1*x + k2*x^2 + k3*x^3
data PowerLoss a = PowerLoss !a !a !a
  deriving (Typeable, Generic, Show, Eq)

instance NFData a => NFData (PowerLoss a)

instance Functor PowerLoss where 
  fmap f (PowerLoss k1 k2 k3) = PowerLoss (f k1) (f k2) (f k3)

instance Floating a => Num (PowerLoss a) where 
  (PowerLoss k11 k12 k13) + (PowerLoss k21 k22 k23) = PowerLoss (k11+k21) (k12+k22) (k13+k23)
  (PowerLoss k11 k12 k13) - (PowerLoss k21 k22 k23) = PowerLoss (k11-k21) (k12-k22) (k13-k23)
  (PowerLoss k11 k12 k13) * (PowerLoss k21 k22 k23) = PowerLoss (k11*k21) (k12*k22) (k13*k23)
  abs = fmap abs
  signum = fmap signum
  fromInteger i = PowerLoss f (sqrt f) (sqrt $ sqrt f)
    where f = fromInteger i 

instance GPU a => GPU (PowerLoss a) where 
  type CPU (PowerLoss a) = PowerLoss (CPU a)
  toGPU (PowerLoss k1 k2 k3) = PowerLoss (toGPU k1) (toGPU k2) (toGPU k3)

-- | Calclulates attentaion level of light for given distance
attentation :: Num a => PowerLoss a -> a -> a 
attentation (PowerLoss k1 k2 k3) x = 1 + k1*x + k2*x*x + k3*x*x*x  

data Light a =
  -- | Light that is very powerfull and located at infinite distance
  -- has only direction and constant power plus color
    DirectionalLight !(Direction a) !(Power a) !(LightColor a)
  -- | Small source of light that is located at specific point
  -- Contains position, initial power, power fall koefficient and color
  | PointLight !(Position a) !(Power a) !(PowerLoss a) !(LightColor a)
  -- | Source of light that lights in specific direction within specified angle
  -- Example: light from pocket light.
  | ConeLight !(Position a) !(ConeDirection a) !(ConeAngle a) !(Power a) !(PowerLoss a) !(LightColor a)
  deriving (Typeable, Generic, Show)

instance NFData a => NFData (Light a)

-- | Returns light position, for directional it is infinite
lightPosition :: Fractional a => Light a -> Position a
lightPosition (DirectionalLight d _ _) = Vec.map (* (1.0/0.0)) d
lightPosition (PointLight p _ _ _) = p 
lightPosition (ConeLight p _ _ _ _ _) = p 

-- | Returns inital power of light source
lightPower :: Light a -> Power a
lightPower (DirectionalLight _ p _) = p 
lightPower (PointLight _ p _ _) = p 
lightPower (ConeLight _ _ _ p _ _) = p 

-- | Returns light power decrease over each distance unit
lightPowerLoss :: Floating a => Light a -> PowerLoss a
lightPowerLoss (DirectionalLight _ _ _) = 0 
lightPowerLoss (PointLight _ _ pl _) =  pl 
lightPowerLoss (ConeLight _ _ _ _ pl _) = pl 

-- | Returns light color
lightColor :: Light a -> LightColor a
lightColor (DirectionalLight _ _ c) = c 
lightColor (PointLight _ _ _ c) = c 
lightColor (ConeLight _ _ _ _ _ c) = c

-- | Converting light inner precision
lightDouble2Float :: Light Double -> Light Float
lightDouble2Float = fmap double2Float 

-- | Converting light inner precision
lightFloat2Double :: Light Float -> Light Double 
lightFloat2Double = fmap float2Double 

instance Functor Light where 
  fmap f l = case l of 
    DirectionalLight d p c -> DirectionalLight (Vec.map f d) (f p) (Vec.map f c)
    PointLight p pw pwl c -> PointLight (Vec.map f p) (f pw) (fmap f pwl) (Vec.map f c)
    ConeLight p d a pw pwl c -> ConeLight (Vec.map f p) (Vec.map f d) (f a) (f pw) (fmap f pwl) (Vec.map f c)

instance GPU a => GPU (Light a) where 
  type CPU (Light a) = Light (CPU a)
  toGPU l = case l of 
    DirectionalLight d p c -> DirectionalLight (toGPU d) (toGPU p) (toGPU c)
    PointLight p pw pwl c -> PointLight (toGPU p) (toGPU pw) (toGPU pwl) (toGPU c)
    ConeLight p d a pw pwl c -> ConeLight (toGPU p) (toGPU d) (toGPU a) (toGPU pw) (toGPU pwl) (toGPU c)

-- | Applies transformation matrix to inner light vectors
-- Can be used to lift light position in proper view space
-- (ex. from model to NDC coordinates)
transformLight :: (Ord a, Floating a) =>
     Mat44 a -- ^ Transformation matrix
  -> Light a -- ^ Light before transformation
  -> Light a -- ^ Light after transformation
transformLight m l = case l of 
  DirectionalLight dir power col -> DirectionalLight 
    (applyNorm dir) power col
  PointLight pos power powerLoss col -> PointLight
    (apply pos) power powerLoss col
  ConeLight pos dir angle power powerLoss col -> ConeLight 
    (apply pos) (applyNorm dir) angle power powerLoss col
  where 
    apply = Vec.project . (m `multmv`) . Vec.homPoint
    applyNorm = Vec.normalize . applyNorm