{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}
module Game.Ship.Mask(
    ShipMask
  , newShipMask
  ) where 

import qualified Data.Vector.Unboxed as V

import GHC.Generics (Generic)
import Control.DeepSeq 
import Data.Vec as Vec
import Data.Bits
import Data.Word
import Util.Vec()

-- | Three dimenstional array that maps each tile coord to single bit (buildable/not buildable)
-- Used to restrict building interior out of ship hull
data ShipMask = ShipMask {
  maskData :: !(V.Vector Word8)
, maskOffset :: !(Vec3 Int)
, maskSize :: !(Vec3 Int)
} deriving (Generic)

instance NFData ShipMask

-- | Makes mask from size and offset
newShipMask :: Vec3 Int -> Vec3 Int -> ShipMask
newShipMask size@(x:.y:.z:.()) offset = ShipMask {
  maskData = V.replicate (ceiling (fromIntegral (x*y*z) / fromIntegral (finiteBitSize (0 :: Word8) ) :: Double)) 0
, maskOffset = offset 
, maskSize = size
}