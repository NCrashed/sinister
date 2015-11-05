{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Mesh(
    Mesh
  , MeshContainer(..)
  , mesh2stream
  ) where

import Prelude as P  
import Graphics.GPipe
import Data.Sequence as S
import Data.Foldable

import GHC.Generics (Generic)
import TextShow 
import TextShow.Generic
import Control.DeepSeq

type Mesh = MeshContainer (Vec3 Float, Vec3 Float, Vec2 Float)
data MeshContainer a = MeshContainer Triangle (Seq a)
  deriving (Generic)

instance TextShow Triangle where 
  showb TriangleList = fromText "TriangleList"
  showb TriangleFan = fromText "TriangleFan"
  showb TriangleStrip = fromText "TriangleStrip"

instance NFData Triangle where 
  rnf = (`seq` ())
  
instance TextShow a => TextShow (Seq a) where 
  showb = showb . toList

instance TextShow a => TextShow (MeshContainer a) where
  showbPrec = genericShowbPrec

instance NFData a => NFData (MeshContainer a)

-- | Converts mesh to GPU stream of verticies, normals and uvs
mesh2stream :: Mesh -> PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
mesh2stream (MeshContainer tr vecs) = toGPUStream tr $ list
  where list = toList vecs

instance Functor MeshContainer where
  fmap f (MeshContainer tr d) = MeshContainer tr (fmap f d)
  
instance Monoid Mesh where
  mempty = MeshContainer TriangleList empty
  mappend (MeshContainer ra av) (MeshContainer rb bv)
    | ra == rb  = MeshContainer ra (mappend av bv) 
    | otherwise = MeshContainer TriangleList (mappend (toTriangleList ra av) (toTriangleList rb bv))

-- | Transforms vertex representation to list of triangles
-- This function is needed when you use monoid instance of mesh, as
-- resulting mesh should have uniform representation of triangles.
toTriangleList :: Triangle -- ^ Representation to convert from
  -> Seq (Vec3 Float, Vec3 Float, Vec2 Float) -- ^ Input stream of vertecies
  -> Seq (Vec3 Float, Vec3 Float, Vec2 Float) -- ^ Output stream of vertecies
toTriangleList TriangleList  = id
toTriangleList TriangleStrip = fromList . P.concat . fromStrip 0 [] . toList
  where 
    fromStrip :: Int -> [[a]] -> [a] -> [[a]]
    fromStrip i acc (x:y:z:xs) 
      | i `mod` 2 == 0 = fromStrip (i+1) ([z,x,y]:acc) (y:z:xs)
      | otherwise      = fromStrip (i+1) ([z,y,x]:acc) (y:z:xs)
    fromStrip _ acc _ = acc
toTriangleList TriangleFan = \xs -> case toList xs of
  [] -> S.empty
  ls -> fromList $ P.concat $ fromFan (head ls) [] (tail ls)
  where
    fromFan :: a -> [[a]] -> [a] -> [[a]]
    fromFan c acc (x:y:xs) = fromFan c ([y,x,c]:acc) (y:xs)
    fromFan _ acc _ = acc