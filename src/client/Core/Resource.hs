{-# LANGUAGE Arrows #-}
module Core.Resource(
    Core.Resource.addNewFileSystemPack
  , Core.Resource.loadResource
  , loadResource2
  , loadResources
  ) where 

import Core.Context
import Core.CoreEvent
import Assets.Manager as Assets
import Assets.Resource as Assets
import Data.Dynamic
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM

import Prelude hiding ((.),id)
import FRP.Netwire
import Control.Wire.Core
import Control.Wire.Unsafe.Event 
import Control.Monad.Trans.Either
import qualified Data.HashMap.Strict as H 
import Control.DeepSeq

import Data.Text (Text)

-- | Adds filesystem resource pack to resource manager
addNewFileSystemPack :: Text -- ^ Name of resource pack
  -> FilePath -- ^ Path to root folder of resource pack
  -> GameMonad ()
addNewFileSystemPack name path = 
  enqueuResourceLoaderSync $ \mng -> 
    Assets.addNewFileSystemPack mng name path 

-- | Creates listener that fires when resource is ready for use
-- Event carries either error message either resource
loadResource :: (NFData a, Resource a) => Text -- ^ Full resource name 'pack:name'
  -> ResourceParams a -- ^ Specific resource params that defines overload
  -> GameWire b (Event (Either Text a))
loadResource name params = postQuery
  where 
    -- | First post loader to resource manager core
    postQuery = mkGen $ \_ _ -> do
      i <- getNextResourceMngCounter
      -- | Callback that loads resource
      enqueuResourceLoader $ \mng tq -> do 
        mres <- runEitherT $ Assets.getResource mng name params 
        atomically $ writeTQueue tq $ case mres of 
          Left s -> (i, Left s, mng)
          Right (a, newInner) -> a `deepseq` (i, Right $ toDyn a, newInner)
      return (Right NoEvent, waitRespond i)

    -- | Wait a respond from resource manager core
    waitRespond i = mkGen $ \_ _ -> do 
      mng <- getResourceMng
      case i `H.lookup` clientResMngFinished mng of 
        Nothing -> return (Right NoEvent, waitRespond i)
        Just mres -> case mres of 
          Left s -> return (Right (Event (Left s)), never) 
          Right res -> case fromDynamic res of 
            Nothing -> return (Right (Event (Left "cast error")), never)
            Just a -> return (Right (Event (Right a)), never)

liftEither :: (Either e a, Either e b) -> Either e (a, b)
liftEither (ma, mb) = (,) <$> ma <*> mb

-- | Loads two resources in parallel manner and waits for
-- both of them are loaded.
loadResource2 :: (NFData a, Resource a, NFData b, Resource b) => 
     Text -> ResourceParams a 
  -> Text -> ResourceParams b 
  -> GameWire c (Event (Either Text (a, b)))
loadResource2 na pa nb pb = proc _ -> do 
  ae <- Core.Resource.loadResource na pa -< ()
  ab <- Core.Resource.loadResource nb pb -< ()
  e <- waitEvents2 -< (ae, ab)
  forceNF -< liftEither <$> e 

loadResources :: (NFData a, Resource a) => 
  [(Text, ResourceParams a)] 
  -> GameWire c (Event (Either Text [a]))
loadResources ps = mapE sequence . waitAllEvents . sequenceA (uncurry Core.Resource.loadResource <$> ps)