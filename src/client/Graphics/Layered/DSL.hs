{-# LANGUAGE GeneralizedNewtypeDeriving, Arrows #-}
module Graphics.Layered.DSL(
  -- | Reexport for convinient
    module Layer 
  , module Model 
  -- | Monad types of DSL
  , LayeredM 
  , Layered
  , FloorM 
  -- | DSL Terms
  -- | General model properties
  , name 
  , floorHeight 
  , floorDef
  -- | Floor properties
  , offset 
  , rotation 
  , child
  , color
  -- | Renderers
  , renderer
  , dynamicLight
  , noShading
  -- | Getting model from DSL
  , loadLayered
  ) where 

import Core 
import Prelude as P hiding (id, (.))
import FRP.Netwire 

import Graphics.Layered.Layer as Layer 
import Graphics.Layered.Model as Model 
import Graphics.GPipe 
import Graphics.Render.Quad
import Assets.Texture 

import Control.Monad.State.Strict as S
import Data.Vec as Vec 
import GHC.Float
import Data.Traversable (sequenceA)

newtype LayeredM a = LayeredM (State ModelContext a)
  deriving (Functor, Applicative, Monad)

type Layered = LayeredM ()

newtype FloorM a = FloorM { unFloorM :: State FloorContext a }
  deriving (Functor, Applicative, Monad)

data ModelContext = ModelContext {
  modelCntxName :: Text 
, modelCntxFloorHeight :: Double 
, modelCntxFloors :: [FloorContext]
}

newModelCntx :: ModelContext
newModelCntx = ModelContext {
  modelCntxName = ""
, modelCntxFloorHeight = 0
, modelCntxFloors = []  
}

runModelCntx :: LayeredM a -> (a, ModelContext)
runModelCntx (LayeredM action) = runState action newModelCntx

getModelCntx :: LayeredM ModelContext
getModelCntx = LayeredM S.get 

putModelCntx :: ModelContext -> Layered
putModelCntx a = LayeredM $ S.put a 

updateModelCntx :: (ModelContext -> ModelContext) -> Layered 
updateModelCntx f = putModelCntx =<< f <$> getModelCntx

type RendererLoader = GameWire () (Event (Either Text LayerRenderer))

defaultRenderer :: LayerRenderer
defaultRenderer = mempty

defaultLoader :: RendererLoader 
defaultLoader = mapE (const . Right $ defaultRenderer) . now 

data FloorContext = FloorContext {
  floorCntxName :: Text 
, floorCntxOffset :: Vec2 Double 
, floorCntxRotation :: Double 
, floorCntxColor :: Vec4 Float 
, floorCntxChilds :: [FloorContext]
, floorCntxRenderer :: RendererLoader
}

newFloorCntx :: Text -> FloorContext
newFloorCntx fname = FloorContext {
  floorCntxName = fname 
, floorCntxOffset = 0 
, floorCntxRotation = 0
, floorCntxColor = 0
, floorCntxChilds = []
, floorCntxRenderer = defaultLoader
}

runFloorM :: FloorM a -> Text -> (a, FloorContext)
runFloorM (FloorM action) fname = runState action $ newFloorCntx fname 

getFloorCntx :: FloorM FloorContext
getFloorCntx = FloorM S.get 

putFloorCntx :: FloorContext -> FloorM () 
putFloorCntx a = FloorM $ S.put a 

updateFloorCntx :: (FloorContext -> FloorContext) -> FloorM () 
updateFloorCntx f = putFloorCntx =<< f <$> getFloorCntx

-- Oprations of DSL

name :: Text -> LayeredM ()
name s = updateModelCntx $ \cntx -> cntx { modelCntxName = s }

floorHeight :: Double -> LayeredM () 
floorHeight h = updateModelCntx $ \cntx -> cntx { modelCntxFloorHeight = h }

offset :: Vec2 Double -> FloorM ()
offset v = updateFloorCntx $ \cntx -> cntx { floorCntxOffset = v }

rotation :: Double -> FloorM () 
rotation r = updateFloorCntx $ \cntx -> cntx { floorCntxRotation = r }

color :: Vec3 Float -> FloorM ()
color clr = updateFloorCntx $ \cntx -> cntx { 
  floorCntxColor = Vec.snoc clr 1 }

floorDef :: Text -> FloorM a -> LayeredM a 
floorDef floorName action = do 
  let (a, floorCntx) = runFloorM action floorName
  updateModelCntx $ \cntx -> cntx { modelCntxFloors = floorCntx : modelCntxFloors cntx }
  return a 

child :: Text -> FloorM a -> FloorM a 
child childName action = do 
  let (a, floorCntx) = runFloorM action childName
  updateFloorCntx $ \cntx -> cntx { floorCntxChilds = floorCntx : floorCntxChilds cntx }
  return a 

renderer :: FloorM RendererLoader -> FloorM ()
renderer loaderM = do
  (loader, _) <- runState (unFloorM loaderM) <$> getFloorCntx
  updateFloorCntx $ \cntx -> cntx { floorCntxRenderer = loader }

-- | Helper that extracts common data from floor context and passes to your function
mkRenderer :: (Vec2 Float -> Float -> Vec4 Float -> RendererLoader) -> FloorM RendererLoader
mkRenderer action = do 
  cntx <- getFloorCntx
  let pos = Vec.map double2Float $ floorCntxOffset cntx
      rot = double2Float $ floorCntxRotation cntx
      clr = floorCntxColor cntx
  return $ action pos rot clr

dynamicLight :: Text -> Text -> FloorM RendererLoader 
dynamicLight diffuseTexName normalTexName = mkRenderer $ \pos rot clr -> 
   mapE (fmap $ \(dt, nt) -> dynamicLightRenderer dt nt pos rot clr) . loadResource2 
    diffuseTexName (Par2DRGBA RGBA8)
    normalTexName (Par2DRGBA RGBA8)
  where 
    dynamicLightRenderer :: 
         TextureResource (Texture2D RGBAFormat)
      -> TextureResource (Texture2D RGBAFormat)
      -> Vec2 Float -> Float -> Vec4 Float 
      -> LayerRenderer 
    dynamicLightRenderer 
      (TextureResource diffuseTex) (TextureResource normalTex) pos rot clr
      cam gpos grot depth lights ambient size 
      = quadWithNormal cam diffuseTex normalTex ambient clr
        lights (Vec.take n2 gpos + pos) (grot+rot) (Vec.get n2 gpos + depth) size 

noShading :: Text -> FloorM RendererLoader 
noShading diffuseTexName = mkRenderer $ \pos rot clr -> 
  mapE (fmap $ \dt -> noShadingRenderer dt pos rot clr)
  . loadResource diffuseTexName (Par2DRGBA RGBA8)
  where 
    noShadingRenderer :: TextureResource (Texture2D RGBAFormat)
      -> Vec2 Float -> Float -> Vec4 Float
      -> LayerRenderer
    noShadingRenderer (TextureResource diffuseTex) pos rot clr
      cam gpos grot depth _ ambient size
      = quadNoShading cam diffuseTex ambient clr (Vec.take n2 gpos + pos) (grot+rot) 
        (Vec.get n2 gpos + depth) size

-- Running DSL to model

loadLayered :: Layered -> GameWire a (Event (Either Text LayeredModel))
loadLayered dsl = 
  mapE (fmap mkModel) . loadFloors (modelCntxFloors cntx)
  where 
    (_, cntx) = runModelCntx dsl 

    mkModel layers = LayeredModel {
      lmodelName = modelCntxName cntx 
    , lmodelPos = 0
    , lmodelRot = 0
    , lmodelFloors = P.reverse layers
    , lmodelFloorHeight = modelCntxFloorHeight cntx
    }

loadFloors :: [FloorContext] -> GameWire a (Event (Either Text [Layer]))
loadFloors cntxs = allLoaded . loader
  where 
    loaders :: [GameWire a (Event (Either Text Layer))]
    loaders = fmap loadFloor cntxs

    loader :: GameWire a [Event (Either Text Layer)]
    loader = sequenceA loaders

allLoaded :: GameWire [Event (Either Text a)] (Event (Either Text [a]))
allLoaded = mapE sequence . waitAllEvents

loadFloor :: FloorContext -> GameWire a (Event (Either Text Layer))
loadFloor cntx = proc _ -> do 
  er <- floorCntxRenderer cntx -< ()
  echs <- loadFloors (floorCntxChilds cntx) -< ()
  mapE (uncurry $ liftM2 mkFloor) . waitEvents2 -< (er, echs)
  where 
    mkFloor r chs = Layer {
      layerName = floorCntxName cntx 
    , layerOffset = floorCntxOffset cntx 
    , layerRotation = floorCntxRotation cntx 
    , layerColor = floorCntxColor cntx
    , layerRenderer = r 
    , layerChilds = chs
    }