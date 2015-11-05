{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}
module Math.Vector(
    module Vec 
  , VectorUnitXY(..)
  , VectorUnitZ(..)
  , cosBetweenVecs
  , angleBetweenVecs
  , angleBetweenVecsSigned
  , pointInRange
  ) where 

import Data.Vec as Vec 

class VectorUnitXY v where 
  -- | Constant vector with X component of length 1
  xunit :: v 
  -- | Constant vector with Y component of length 1
  yunit :: v 

class VectorUnitZ v  where 
  -- | Constant vector with Z component of length 1
  zunit :: v 

instance Num a => VectorUnitXY (Vec3 a) where 
  xunit = 1 :. 0 :. 0 :. ()
  yunit = 0 :. 1 :. 0 :. ()

instance Num a => VectorUnitZ (Vec3 a) where 
  zunit = 0 :. 0 :. 1 :. ()

instance Num a => VectorUnitXY (Vec2 a) where 
  xunit = 1 :. 0 :. ()
  yunit = 0 :. 1 :. ()

-- | Returns cosinus between two vecs
cosBetweenVecs :: (Num v, Floating a, Fold v a, ZipWith a a a v v v) => v -> v -> a
cosBetweenVecs v1 v2 = (v1 `dot` v2) / (norm v1 * norm v2)

-- | Unsigned angle between vectors
angleBetweenVecs :: (Num v, Floating a, Fold v a, ZipWith a a a v v v) => v -> v -> a
angleBetweenVecs v1 v2 = acos $ cosBetweenVecs v1 v2 

-- | Signed angle between vectors
angleBetweenVecsSigned :: Floating a => Vec2 a -> Vec2 a -> a 
angleBetweenVecsSigned v1@(x1:.y1:.()) v2@(x2:.y2:.()) = signum cr * acos (cosBetweenVecs v1 v2)
  where cr = x1 * y2 - x2 * y1

-- | Returns true if given point in range of other point
pointInRange :: (Fold v a,  ZipWith a a a v v v, Num a, Num v, Ord a) => 
     v -- ^ Origin
  -> a -- ^ Maximum range to origin
  -> v -- ^ Point to test distance to origin
  -> Bool
pointInRange origin rad p = normSq (origin - p) <= rad*rad