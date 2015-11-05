{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Game.Boxed.Direction(
    Side(..)
  , sideDirection
  , directionSide
  , EachSide(..)
  , fromEachSide
  ) where 

import Data.Typeable
import Data.Vec as Vec
import Data.Serialize
import GHC.Generics (Generic)
import Control.DeepSeq

-- | Defines enum for direction
data Side = Upward | Downward | Forward | Backward | Leftward | Rightward | Unknownward
  deriving (Typeable, Eq, Ord, Enum, Show, Generic)

instance NFData Side 
instance Serialize Side 

-- | Defines normalized vector of direction
sideDirection :: Num a => Side -> Vec3 a
sideDirection Upward      = 0    :. 0    :. 1    :. ()
sideDirection Downward    = 0    :. 0    :. (-1) :. ()
sideDirection Forward     = 1    :. 0    :. 0    :. ()
sideDirection Backward    = (-1) :. 0    :. 0    :. ()
sideDirection Rightward   = 0    :. (-1) :. 0    :. ()
sideDirection Leftward    = 0    :. 1    :. 0    :. ()
sideDirection Unknownward = 0    :. 0    :. 0    :. ()

-- | Tries to guess whole direction from offset
directionSide :: (Ord a, Num a) => Vec3 a -> Side
directionSide (x:.y:.z:.()) 
  | x > y && x < (-y) && x > z && x > (-z) = Forward
  | x > y && x < (-y) && x < z = Upward 
  | x > y && x < (-y) && x < (-z) = Downward 

  | x < y && x > (-y) && x < z && x < (-z) = Backward
  | x < y && x > (-y) && x > z = Upward 
  | x < y && x > (-y) && x > (-z) = Downward 

  | x < y && x < (-y) && y > z && y > (-z) = Rightward
  | x < y && x < (-y) && y < z = Upward 
  | x < y && x < (-y) && y < (-z) = Downward 

  | x > y && x > (-y) && y < z && y < (-z) = Leftward
  | x > y && x > (-y) && y > z = Upward 
  | x > y && x > (-y) && y > (-z) = Downward 
directionSide _ = Unknownward

-- | Maps a value for each side
data EachSide a = EachSide {
  upward :: !a 
, downward :: !a 
, forward :: !a 
, backward :: !a 
, leftward :: !a 
, rightward :: !a 
} deriving (Typeable, Eq, Ord, Show, Generic)

-- | Getting from EachSide by Side value
fromEachSide :: Side -> EachSide a -> a 
fromEachSide Upward = upward  
fromEachSide Downward = downward
fromEachSide Forward = forward
fromEachSide Backward = backward 
fromEachSide Rightward = rightward
fromEachSide Leftward = leftward
fromEachSide Unknownward = upward -- We really doesn't matter, for totality only

instance NFData a => NFData (EachSide a)
instance Serialize a => Serialize (EachSide a)