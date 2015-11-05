{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Util.Texture(

  ) where 

import Control.DeepSeq
import Graphics.GPipe

instance NFData (Texture1D f) where
  rnf = (`seq` ())

instance NFData (Texture2D f) where
  rnf = (`seq` ())

instance NFData (Texture3D f) where
  rnf = (`seq` ())