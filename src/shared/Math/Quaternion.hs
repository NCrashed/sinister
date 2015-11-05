{-# LANGUAGE MultiWayIf, FlexibleContexts, DeriveGeneric #-}
module Math.Quaternion(
    Quaternion(..)
  , unitQuaternion
  , fromAxis
  , fromEuler
  , fromRotationMatrix
  , toRotationMatrix
  , quatLength
  , quatLength2
  , quatAxis
  , quatVec
  , quatAngle
  , quatScalar
  , quatConjugation
  , quatNormalize
  , quatInverse
  , rotateVec
  , slerp
  , scaleImag
  , scalarProduct
  ) where
  
import Data.Vec as Vec
import GHC.Generics (Generic)
import Control.DeepSeq
import TextShow
import TextShow.Generic 
import Util.Vec()
import Data.Serialize (Serialize)

-- | Math structure that describes rotation in R3 space.
-- Rotations can be composed by multiplying quaternions.
newtype Quaternion a = Quaternion (Vec4 a)
  deriving (Eq, Show, Generic)

instance NFData a => NFData (Quaternion a)
instance Serialize a => Serialize (Quaternion a)

instance TextShow a => TextShow (Quaternion a) where 
  showbPrec = genericShowbPrec

-- | Returns quaternion with no rotation
unitQuaternion :: Floating a => Quaternion a 
unitQuaternion = fromAxis (1:.0:.0:.()) 0 

-- | Creating quaternion that describes rotation around an axis by
-- specified angle.
fromAxis :: Floating a => Vec3 a -- ^ Axis, would be normalized 
  -> a -- ^ Rotation angle in radians 
  -> Quaternion a
fromAxis axis angle = Quaternion (x:.y:.z:.w:.())
  where
    sina = sin (angle / 2)
    x = ax * sina
    y = ay * sina
    z = az * sina
    w = cos (angle / 2)
    (ax:.ay:.az:.()) = normalize axis

-- | Creating quaternion that describes rotation by thre Euler angles.    
fromEuler :: Floating a => Vec3 a -- | Pitch yaw roll angles 
  -> Quaternion a
fromEuler (pitch:.yaw:.roll:.()) = Quaternion (x:.y:.z:.w:.())
  where
    cosz2 = cos (0.5*roll)
    cosy2 = cos (0.5*yaw)
    cosx2 = cos (0.5*pitch)
    sinz2 = sin (0.5*roll)
    siny2 = sin (0.5*yaw)
    sinx2 = sin (0.5*pitch)
    w = cosz2*cosy2*cosx2 + sinz2*siny2*sinx2
    x = cosz2*cosy2*sinx2 - sinz2*siny2*cosx2
    y = cosz2*siny2*cosx2 + sinz2*cosy2*sinx2
    z = sinz2*cosy2*cosx2 - cosz2*siny2*sinx2

-- | Creating quaternion that describes rotation in specified matrix. 
fromRotationMatrix :: (Floating a, Ord a) => Mat44 a -> Quaternion a
fromRotationMatrix mat = Quaternion (x:.y:.z:.w:.())
  where
    mget ir ic = get ic $ get ir mat   
    trace = mget n0 n0 + mget n1 n1 + mget n2 n2
    (x, y, z, w) = if
      | trace > 0 -> let t = 0.5 / sqrt (trace + 1) in
        ( (mget n1 n2 - mget n2 n1) * t
        , (mget n2 n0 - mget n0 n2) * t
        , (mget n0 n1 - mget n1 n0) * t
        , (trace + 1) * t )
      | mget n0 n0 > mget n1 n1 && mget n0 n0 > mget n2 n2 -> let t = 0.5 / sqrt (1 + mget n0 n0 - mget n1 n1 - mget n2 n2) in
        ( (1 + mget n0 n0 - mget n1 n1 - mget n2 n2) * t
        , (mget n1 n0 + mget n0 n1) * t
        , (mget n2 n0 + mget n0 n2) * t
        , (mget n1 n2 - mget n2 n1) * t)
      | mget n1 n1 > mget n2 n2 -> let t = 0.5 / sqrt (1 + mget n1 n1 - mget n0 n0 - mget n2 n2) in
        ( (mget n1 n0 + mget n0 n1) * t
        , (1 + mget n1 n1 - mget n0 n0 - mget n2 n2) * t
        , (mget n2 n1 + mget n1 n2) * t
        , (mget n2 n0 - mget n0 n2) * t)
      | otherwise -> let t = 0.5 / sqrt (1 + mget n2 n2 - mget n0 n0 - mget n1 n1) in
        ( (mget n2 n0 + mget n0 n2) * t
        , (mget n2 n1 + mget n1 n2) * t
        , (1 + mget n2 n2 - mget n0 n0 - mget n1 n1) * t
        , (mget n0 n1 - mget n1 n0) * t)

-- | Transforms quaternion rotation into rotation matrix.
-- This method is used by gpu pipeline as a way to get
-- final rotated vector.
toRotationMatrix :: Floating a => Quaternion a -> Mat44 a
toRotationMatrix q@(Quaternion (x:.y:.z:.w:.())) = 
  ( (m00:.m01:.m02:. 0 :.()) :.
    (m10:.m11:.m12:. 0 :.()) :.
    (m20:.m21:.m22:. 0 :.()) :.
    ( 0 :. 0 :. 0 :. 1 :.()) :. ())
  where
    s = 2 / quatLength q
    x2 = x * s
    y2 = y * s
    z2 = z * s
    xx = x * x2
    xy = x * y2
    xz = x * z2
    yy = y * y2
    yz = y * z2
    zz = z * z2
    wx = w * x2
    wy = w * y2
    wz = w * z2
    
    m00 = 1 - (yy + zz)
    m10 = xy - wz
    m20 = xz + wy
    
    m01 = xy + wz
    m11 = 1 - (xx + zz)
    m21 = yz - wx
    
    m02 = xz - wy
    m12 = yz + wx
    m22 = 1 - (xx + yy)      

-- | Retuns length of a quaternion.
quatLength :: Floating a => Quaternion a -> a
quatLength = sqrt . quatLength2

-- | Returns square of length of a quaternion.
-- More efficient then "quatLength" and should be
-- used where is possible. 
quatLength2 :: Floating a => Quaternion a -> a
quatLength2 (Quaternion (x:.y:.z:.w:.())) = x*x + y*y + z*z + w*w

-- | Returns rotation axis of a quaternion.
quatAxis :: Floating a => Quaternion a -> Vec3 a
quatAxis (Quaternion (x:.y:.z:.w:.())) = (x/t):.(y/t):.(z/t):.() 
  where t = sin (acos(w))

-- | Returns vector component of a quaternion. 
-- To get rotation axis use "quatAxis" function. 
quatVec :: Floating a => Quaternion a -> Vec3 a
quatVec (Quaternion v) = Vec.take n3 v

-- | Retuns rotation angle of a quaternion.
quatAngle :: Floating a => Quaternion a -> a
quatAngle (Quaternion (_:._:._:.w:.())) = 2*acos(w)

-- | Returns scalar component of a quaternion.
-- To get roation angle use "quatAngle" function.
quatScalar :: Floating a => Quaternion a -> a
quatScalar (Quaternion (_:._:._:.w:.())) = w

scaleImag :: Floating a => a -> Quaternion a -> Quaternion a
scaleImag val (Quaternion (x:.y:.z:.w:.())) = Quaternion ((x*val):.(y*val):.(z*val):.w:.())
 
scalarProduct :: Floating a => Quaternion a -> Quaternion a -> a
scalarProduct (Quaternion (a1:.b1:.c1:.d1:.())) (Quaternion (a2:.b2:.c2:.d2:.())) = a1*a2 + b1*b2 + c1*c2 + d1*d2 

vectorProduct :: Floating a => Quaternion a -> Quaternion a -> Quaternion a
vectorProduct (Quaternion (x1:.y1:.z1:.w1:.())) (Quaternion (x2:.y2:.z2:.w2:.())) = Quaternion (x:.y:.z:.w:.())
    where
      x = w1*x2 + x1*w2 + y1*z2 - z1*y2
      y = w1*y2 - x1*z2 + y1*w2 + z1*x2
      z = w1*z2 + x1*y2 - y1*x2 + z1*w2
      w = w1*w2 - x1*x2 - y1*y2 - z1*z2
      
instance Floating a => Num (Quaternion a) where
  (Quaternion va) + (Quaternion vb) = Quaternion (va + vb)
  (Quaternion va) - (Quaternion vb) = Quaternion (va - vb)
  (*) = vectorProduct
  abs (Quaternion v) = Quaternion (abs v)
  signum (Quaternion v) = Quaternion (signum v)
  fromInteger i = fromAxis (0:.0:.1:.()) (fromInteger i)

-- | Returns quaternion conjugation (imagenary part negated).  
quatConjugation :: Floating a => Quaternion a -> Quaternion a
quatConjugation (Quaternion (x:.y:.z:.w:.())) = Quaternion (x:.y:.z:.(-w):.())

-- | Returns normalized quaternion (vector part normalized).
quatNormalize :: Floating a => Quaternion a -> Quaternion a
quatNormalize (Quaternion (x:.y:.z:.w:.())) = Quaternion ((x/l):.(y/l):.(z/l):.w:.())
  where l = sqrt $ x*x + y*y + z*z + w*w

-- | Returns quaternion describing reverse rotation of a quaternion.   
quatInverse :: Floating a => Quaternion a -> Quaternion a
quatInverse = quatConjugation . quatNormalize

-- | Applies quaternion rotation for vector.
rotateVec :: Floating a => Quaternion a -> Vec3 a -> Vec3 a
rotateVec q (x:.y:.z:.()) = negate $ quatVec $ q * (Quaternion (x:.y:.z:.0:.())) * quatConjugation q

-- | Spherical linear interpolation. Helps to smoothly produce
-- rotation animation.
slerp :: (Floating a, Ord a) => a -- ^ Value between 0 and 1  
  -> Quaternion a -- ^ Source rotation 
  -> Quaternion a -- ^ Target rotation 
  -> Quaternion a
slerp t (Quaternion qa@(qax:.qay:.qaz:.qaw:.())) (Quaternion (qbx:.qby:.qbz:.qbw:.())) = Quaternion qv
  where
    scal = qax * qbx + qay * qby + qaz * qbz + qaw * qbw
    angle = acos scal
    sina = sqrt (1-scal*scal)
    eps = 0.001
    ratioA = sin ((1-t)*angle) / sina
    ratioB = sin (t*angle) / sina
    qv = if
      | abs scal >= 1 -> qa
      | abs sina < eps -> -- angle = 180 -> can rotate any way normal to qa or qb
          (qax * 0.5 + qbx * 0.5) :.
          (qay * 0.5 + qby * 0.5) :.
          (qaz * 0.5 + qbz * 0.5) :.
          (qaw * 0.5 + qbw * 0.5) :. ()
      | otherwise ->
          (qax * ratioA + qbx * ratioB) :.
          (qay * ratioA + qby * ratioB) :.
          (qaz * ratioA + qbz * ratioB) :.
          (qaw * ratioA + qbw * ratioB) :. ()