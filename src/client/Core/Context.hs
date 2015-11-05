{-# LANGUAGE StandaloneDeriving, DeriveGeneric, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Core.Context(
    GameSession
  , GameWire
  , GameWireI
  , GameMonad
  , GameActor
  , GameContext
  , EventManager(..)
  , InputEvent(..)
  , ClientContext(..)
  , newEventManager
  , getEventMng
  , putEventMng
  -- | Resource management (background loading)
  , ClientResourceManager(..)
  , ResourceId
  , ResourceTQueue
  , getResourceMng
  , putResourceMng
  , updateResourceMng
  , enqueuResourceLoader
  , enqueuResourceLoaderSync
  , runResourceLoaders
  , readResourceResults
  , getNextResourceMngCounter
  -- | Viewport API
  , viewportSizeM
  , viewportSize
  ) where

import Core.Monad
import Core.Indexed
import Core.Input

import Assets.Manager
import GHC.Generics (Generic)
import Control.Monad.State.Strict
import Control.DeepSeq

import Data.Dynamic
import Control.Concurrent.STM.TQueue
import qualified Data.HashMap.Strict as H
import System.IO.Unsafe (unsafePerformIO)
import Util.Concurrent
import qualified Data.Foldable as F 
import Data.Vec(Vec2)
import Util.Vec()

import Network.Protocol.Message 

import Data.Text (Text)

type GameContext = GameContextG ClientContext
type GameMonad a = GameMonadG ClientContext a
type GameWire a b = GameWireG ClientContext a b
type GameWireI a b = GameWireGI ClientContext a b 
type GameActor i a b = GameActorG ClientContext i a b 

instance GameContextClass ClientContext where 
  newCustomContext = newClientContext

-- | Context specific for client
data ClientContext = ClientContext {
  -- | Current asset manager
    clientResMng :: !ClientResourceManager
  -- | Current event manager
  , clientEventMng :: !EventManager
  -- | Current size of vieport
  , clientViewportSize :: !(Vec2 Int)
  -- | Is cursor locked?
  , clientCursorLock :: Bool
} deriving (Generic)

instance NFData ClientContext

newClientContext :: ClientContext
newClientContext = ClientContext newClientResourceManager newEventManager 0 False

-- ===========================================================================
-- Resource management (background loading)

-- | Id of resource
-- Each resource acquires an unique id to find it
-- when loaded.
type ResourceId = Int 
type ResourceTQueue = TQueue (ResourceId, Either Text Dynamic, ResourceManager)

instance NFData ResourceTQueue where 
  rnf = (`seq` ())

data ClientResourceManager = ClientResourceManager {
  -- | Current resource manager
    clientResMngInner :: !ResourceManager
  -- | Client channel where resource loaders loads resources to and
  -- game engine gets resulted resources from.
  , clientResMngTQueue :: !ResourceTQueue
  -- | Stack of queued actions to load resources
  -- All actions are executed in separate thread and should 
  -- emit resulted resource into TQueue
  , clientResMngStack :: ![ResourceManager -> ResourceTQueue -> IO ()]
  -- | Stack of queued actions to modify resource manager (e.x. register resource pack)
  -- all actions in this queue should perform before ones from clientResMngStack
  , clientResMngStackSynch :: ![ResourceManager -> IO ResourceManager]
  -- | Stack of finished resources
  , clientResMngFinished :: !(H.HashMap ResourceId (Either Text Dynamic))
  -- | Counter that holds next id off resource
  , clientResMngCounter :: ResourceId
} deriving (Generic)

instance NFData ClientResourceManager

-- | Creates semantically empty client resource manager
newClientResourceManager :: ClientResourceManager 
newClientResourceManager = ClientResourceManager { 
  clientResMngInner = emptyResourceManager
, clientResMngTQueue = unsafePerformIO newTQueueIO
, clientResMngStack = []
, clientResMngStackSynch = []
, clientResMngFinished = H.empty 
, clientResMngCounter = 0
}

-- | Returns current client resource manager
getResourceMng :: GameMonad ClientResourceManager 
getResourceMng = fmap (clientResMng . gameCustomContext) get 

-- | Sets current client resource manager
putResourceMng :: ClientResourceManager -> GameMonad () 
putResourceMng rmng = do 
  cntx <- get 
  put $ cntx { gameCustomContext = (gameCustomContext cntx) {
      clientResMng = rmng
    } 
  }

-- | Helper function that updates current client resource manager with f
updateResourceMng :: (ClientResourceManager -> ClientResourceManager) -> GameMonad ()
updateResourceMng f = putResourceMng =<< (f <$> getResourceMng)

-- | Places resource loading action into queue 
-- The loader should load desired resource and place it into 
-- concurrent channel-queue passed as first parameter.
enqueuResourceLoader :: (ResourceManager -> ResourceTQueue -> IO ()) -> GameMonad ()
enqueuResourceLoader loader = do 
  updateResourceMng $ \mng -> mng {
    clientResMngStack = loader : (clientResMngStack mng)
  }

-- | Place resource manager updating action into queue
-- The loader should update resource manager (e.x. register new resource pack)
enqueuResourceLoaderSync :: (ResourceManager -> IO ResourceManager) -> GameMonad ()
enqueuResourceLoaderSync loader = do 
  updateResourceMng $ \mng -> mng {
    clientResMngStackSynch = loader : (clientResMngStackSynch mng)
  }

-- | Executes all enqueued resource loaders
-- Note: list of actions are cleaned after call of this function
runResourceLoaders :: ClientResourceManager -> IO ClientResourceManager
runResourceLoaders mng = do
  imng' <- F.foldrM ($) (clientResMngInner mng) (clientResMngStackSynch mng)
  forM_ (clientResMngStack mng) $ 
    \loader -> loader imng' (clientResMngTQueue mng) -- Cannot use forkIO due OpenGL resource loading bug
  return $ mng { 
      clientResMngInner =  imng'
    , clientResMngStack = []
    , clientResMngStackSynch = [] 
  }

-- | Checks results of resource loading
-- Note: old results are overwritten with new, as events should capture result
-- as soon as it appears in result set.
readResourceResults :: ClientResourceManager -> IO ClientResourceManager
readResourceResults mng = do 
  res <- readTQueueWhileCould $ clientResMngTQueue mng
  let (pairs, mngs) = foldl (\(acc1, acc2) (a, b, c) -> ((a,b):acc1, c:acc2)) ([], []) res  
  let newInner = mconcat (clientResMngInner mng : mngs)
  return $ mng { 
    clientResMngInner = newInner
  , clientResMngFinished = H.fromList pairs 
  }

-- | Returns next index and saves update counter at context
getNextResourceMngCounter :: GameMonad ResourceId 
getNextResourceMngCounter = do 
  mng <- getResourceMng
  putResourceMng $ mng { clientResMngCounter = clientResMngCounter mng + 1 }
  return $ clientResMngCounter mng 

-- ====================================================================
-- Event manager definition

-- | TODO: OutputEvent for manual disconnects

data EventManager = EventManager {
  -- | Holds network messages from server
    eventMngNetMessages :: ![NetworkMessage]
  -- | Holds other input events for simulation
  , eventMngInputEvents :: ![InputEvent]
  -- | Holds messages that should be sent to client side
  -- Note: first message in list - the last message in time
  , eventMngMessages2Send :: ![NetworkMessage]
} deriving (Generic)

instance NFData EventManager

newEventManager :: EventManager
newEventManager = EventManager [] [] []

getEventMng :: GameMonad EventManager 
getEventMng = fmap (clientEventMng . gameCustomContext) get 

putEventMng :: EventManager -> GameMonad () 
putEventMng emng = do 
  cntx <- get 
  put $ cntx { gameCustomContext = (gameCustomContext cntx) {
      clientEventMng = emng
    } 
  }

-- Viewport API
-- ============================================================

-- | Returns current viewport size
viewportSizeM :: GameMonad (Vec2 Int)
viewportSizeM = fmap (clientViewportSize . gameCustomContext) get

-- | Returns current viewport size
viewportSize :: GameWire a (Vec2 Int)
viewportSize = liftGameMonad viewportSizeM