{-# LANGUAGE TypeOperators, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Util.Vec(

  ) where 

import Control.DeepSeq
import Data.Hashable
import Data.Vec 

import TextShow
import Data.Monoid
import Data.Serialize as S 

instance (NFData a, NFData b) => NFData (a :. b) where 
  rnf (a :. b) = a `deepseq` b `deepseq` ()

instance (Hashable a, Hashable b) => Hashable (a :. b) where
  hashWithSalt salt (a :. b) = hashWithSalt salt a + hashWithSalt salt b

instance (TextShow a, TextShow b) => TextShow (a :. b) where 
  showb (a :. b) = showb a <> " :. " <> showb b

instance Serialize a => Serialize (Vec2 a) where 
  put (x:.y:.()) = S.put (x,y)
  get = do 
    (x,y) <- S.get 
    return $ x:.y:.()

instance Serialize a => Serialize (Vec3 a) where 
  put (x:.y:.z:.()) = S.put (x,y,z)
  get = do 
    (x,y,z) <- S.get 
    return $ x:.y:.z:.()

instance Serialize a => Serialize (Vec4 a) where 
  put (x:.y:.z:.w:.()) = S.put (x,y,z,w)
  get = do 
    (x,y,z,w) <- S.get 
    return $ x:.y:.z:.w:.()