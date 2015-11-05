{-# LANGUAGE Arrows #-}
module Core.Log(
  -- | Logging in game monad
    logInfo
  , logWarn
  , logError
  -- | Logging within game wire
  , logInfoA
  , logWarnA
  , logErrorA
  -- | Logging discrete events
  , logInfoE
  , logWarnE
  , logErrorE
  -- | Tracing events (debug)
  , traceEvent
  , traceEventShow
  ) where 

import Core.Monad

import Control.Wire.Unsafe.Event 
import Control.Monad.State.Strict
import qualified Data.Sequence as S

import Prelude hiding (id, (.))
import FRP.Netwire

import Data.Text (Text)
import TextShow 

-- | Putting message into console
logInfo, logWarn, logError, rawLog :: Text -> GameMonadG e ()
logInfo = rawLog . ("Info: " <>)
logWarn = rawLog . ("Warn: " <>)
logError = rawLog . ("Error: " <>)
rawLog s = do
  cntx <- get 
  put $ cntx { gameLogMessages = s S.<| gameLogMessages cntx }

-- | Putting message into console, arrow version
logInfoA, logWarnA, logErrorA :: GameWireG e Text ()
logInfoA = liftGameMonad1 logInfo 
logWarnA = liftGameMonad1 logWarn 
logErrorA = liftGameMonad1 logError 

-- | Putting message int console when an event fires
logInfoE, logWarnE, logErrorE :: GameWireG e (Event Text) ()
logInfoE = logRawE logInfoA 
logWarnE = logRawE logWarnA 
logErrorE = logRawE logErrorA 

-- | Transforms logger that spams every step to event logger
logRawE :: GameWireG e Text () -> GameWireG e (Event Text) ()
logRawE logger = proc ev -> case ev of 
  NoEvent -> forceNF -< ()
  Event msg -> logger -< msg 

-- | Prints occurences of event to log
traceEventShow :: TextShow a => GameWireG e (Event a) (Event a)
traceEventShow = traceEvent showt

-- | Prints occurences of event to log using specific function to form message
traceEvent :: (a -> Text) -> GameWireG e (Event a) (Event a)
traceEvent f = proc ev -> do
  logInfoE . arr (fmap f) -< ev 
  returnA -< ev