module Assets.ObjMesh.Converter(
    convertObj
  ) where 

import Assets.ObjMesh.Parser
import Data.Sequence as S
import Prelude as P  
import Data.Vec as Vec 
import Graphics.Mesh 
import Graphics.Model
import Graphics.GPipe
import Math.Quaternion 
import GHC.Float 
import Data.Maybe 
import Data.Foldable as F 
import qualified Data.HashMap.Strict as M 
import Control.Monad 

import Data.Text (Text)
import qualified Data.Text as T

-- | Converts obj model AST to model
convertObj :: Text -> [ObjAST] -> Model
convertObj name os = Model {
    modelName = name
  , modelMeshes = M.fromList $ fmap modelMeshName meshes `P.zip` meshes
  , modelPos = 0:.0:.0:.()
  , modelRot = unitQuaternion
  }
  
  where 
    meshes = fmap (uncurry oneMesh) . splitMeshes . rebaseSubmeshes $ os
    oneMesh mname mos = ModelMesh {
      modelMeshName = mname 
    , modelMesh = MeshContainer TriangleList . uncurry4 fromIndexedModel . collectVerticies $ mos
    , modelMeshTexture = Nothing -- TODO
    } 
    uncurry4 f (x,y,z,w) = f x y z w

-- | Indexes in original obj are global, need to convert to local
rebaseSubmeshes :: [ObjAST] -> [ObjAST]
rebaseSubmeshes objs = goUvs 0 0 $ goNorms 0 0 $ goVerts 0 0 objs
  where
  goVerts :: Int -> Int -> [ObjAST] -> [ObjAST]
  goVerts globalIndex localIndex os = case os of 
    (o@ObjObject{} : xs) -> o : goVerts (globalIndex+localIndex) 0 xs
    (o@ObjVertex{} : xs) -> o : goVerts globalIndex (localIndex+1) xs
    (ObjFace faces : xs) -> let incVert (i, u, n) = (i-globalIndex, u, n)
      in ObjFace (fmap incVert faces) : goVerts globalIndex localIndex xs
    (o : xs) -> o : goVerts globalIndex localIndex xs
    [] -> []

  goNorms :: Int -> Int -> [ObjAST] -> [ObjAST]
  goNorms globalIndex localIndex os = case os of 
    (o@ObjObject{} : xs) -> o : goNorms (globalIndex+localIndex) 0 xs
    (o@ObjNormal{} : xs) -> o : goNorms globalIndex (localIndex+1) xs
    (ObjFace faces : xs) -> let incNorm (i, u, n) = (i, u, fmap (\a->a-globalIndex) n)
      in ObjFace (fmap incNorm faces) : goNorms globalIndex localIndex xs
    (o : xs) -> o : goNorms globalIndex localIndex xs
    [] -> []

  goUvs :: Int -> Int -> [ObjAST] -> [ObjAST]
  goUvs globalIndex localIndex os = case os of 
    (o@ObjObject{} : xs) -> o : goUvs (globalIndex+localIndex) 0 xs
    (o@ObjUv{} : xs) -> o : goUvs globalIndex (localIndex+1) xs
    (ObjFace faces : xs) -> let incUvs (i, u, n) = (i, fmap (\a->a-globalIndex) u, n)
      in ObjFace (fmap incUvs faces) : goUvs globalIndex localIndex xs
    (o : xs) -> o : goUvs globalIndex localIndex xs
    [] -> []

-- | Splits obj ast by object tags
splitMeshes :: [ObjAST] -> [(Text, [ObjAST])]
splitMeshes = F.foldl go []
  where 
    go :: [(Text, [ObjAST])] -> ObjAST -> [(Text, [ObjAST])]
    go [] o = case o of 
      ObjObject s -> [(T.pack s, [])]
      _ -> [("default", [o])]
    go (a@(oname, as):ass) o = case o of
      ObjObject s -> (T.pack s, []):a:ass
      _ -> (oname, o:as):ass

-- | One triangle is thre indexes, one index carry numbers of vert, normal, uv
type IndexSeq = Seq (Vec3 (Vec3 Int))

-- | Goes over obj AST and collects verts, normals and uvs, also returns sequence of indexes (triangle faces)
collectVerticies :: [ObjAST] -> (Seq (Vec3 Float), Seq (Vec3 Float), Seq (Vec2 Float), IndexSeq)
collectVerticies = F.foldl go (S.empty, S.empty, S.empty, S.empty)
  where 
    go :: (Seq (Vec3 Float), Seq (Vec3 Float), Seq (Vec2 Float), IndexSeq) -- ^ acc
      -> ObjAST
      -> (Seq (Vec3 Float), Seq (Vec3 Float), Seq (Vec2 Float), IndexSeq) -- ^ acc
    go acc@(vs, ns, uvs, is) o = case o of 
      ObjVertex x y z -> (v3 x y z <| vs, ns, uvs, is)
      ObjNormal x y z -> (vs, v3 x y z <| ns, uvs, is)
      ObjUv x y       -> (vs, ns, v2f x y <| uvs, is)
      ObjFace [i1, i2, i3] -> (vs, ns, uvs, (v3i i1 :. v3i i2 :. v3i i3 :. ()) <| is)
      _ -> acc

    v3 x y z = Vec.map double2Float $ x:.y:.z:.()
    v2f x y = Vec.map double2Float $ x:.y:.()
    v3i (x, y, z) = x :. (fromMaybe 0 y) :. (fromMaybe 0 z) :. () 

-- | Unpacks indexed vertecies
fromIndexedModel :: Seq (Vec3 Float) -- ^ Vertecies
  -> Seq (Vec3 Float) -- ^ Normals
  -> Seq (Vec2 Float) -- ^ Uvs
  -> IndexSeq -- ^ Indexes
  -> Seq (Vec3 Float, Vec3 Float, Vec2 Float) -- ^ Resulting model
fromIndexedModel vs ns uvs = F.foldl go S.empty
  where 
    go :: Seq (Vec3 Float, Vec3 Float, Vec2 Float) -- ^ acc
      -> Vec3 (Vec3 Int) -- ^ Triangle
      -> Seq (Vec3 Float, Vec3 Float, Vec2 Float)
    go acc (i1:.i2:.i3:.()) = i i1 <| i i2 <| i i3 <| acc
      where
        -- | Triple index
        i :: Vec3 Int -> (Vec3 Float, Vec3 Float, Vec2 Float) 
        i (vi:.uvi:.ni:.()) = let
          a = fromMaybe 0 $ vs `indexS` (vi-1)
          b = fromMaybe (0:.1:.0:.()) $ ns `indexS` (ni-1)
          c = fromMaybe 0 $ uvs `indexS` (uvi-1)
          in (a, b, c)
            where 
            indexS q n = do 
              guard $ n >= 0 && n < S.length q
              return $ index q n 