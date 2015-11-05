{-# LANGUAGE Arrows #-}
module Core.CoreEvent(
    mapE
  , liftEvent
  -- | Awaiting multiple events
  , waitEvents2
  , waitEvents3
  , waitAllEvents
  -- | Filtering
  , filterEitherE
  , filterEitherE'
  , filterJustE
  -- | Holding
  , holdJust
  ) where 

import Prelude hiding (id, (.))
import FRP.Netwire 
import Core.Monad 
import Core.Log
import Control.Wire.Unsafe.Event 
import Control.Wire.Core 

import Data.Text (Text)
import TextShow

-- | Shortcart for transforming values inside a event
mapE :: (a -> b) -> GameWireG cntx (Event a) (Event b)
mapE f = arr (fmap f)
 
-- | Lifts a game wire into discrete signal world
liftEvent :: GameWireG cntx a b -> GameWireG cntx (Event a) (Event b)
liftEvent w = proc ev -> case ev of 
  NoEvent -> returnA -< NoEvent 
  Event a -> fmap Event w -< a

-- | Waits for occurences of both events and returns their first occurences
-- after that never occurs.
waitEvents2 :: GameWireG cntx (Event a, Event b) (Event (a,b))
waitEvents2 = go Nothing Nothing
  where 
    go Nothing Nothing = mkGen $ \_ i -> case i of 
      (NoEvent, NoEvent) -> return (Right NoEvent, go Nothing Nothing)
      (Event a, NoEvent) -> return (Right NoEvent, go (Just a) Nothing)
      (NoEvent, Event b) -> return (Right NoEvent, go Nothing (Just b))
      (Event a, Event b) -> return (Right $ Event (a, b), never)

    go (Just a) Nothing = mkGen $ \_ (_, eb) -> case eb of 
      NoEvent -> return (Right NoEvent, go (Just a) Nothing)
      Event b -> return (Right $ Event (a, b), never)

    go Nothing (Just b) = mkGen $ \_ (ea, _) -> case ea of 
      NoEvent -> return (Right NoEvent, go Nothing (Just b))
      Event a -> return (Right $ Event (a, b), never)

    go (Just a) (Just b) = mkGen $ \_ _ -> return (Right $ Event (a, b), never)

-- | Waits for occurences of events and returns their first occurences
-- after that never occurs.
waitEvents3 :: GameWireG cntx (Event a, Event b, Event c) (Event (a,b,c))
waitEvents3 = go Nothing Nothing Nothing
  where 
    go Nothing Nothing Nothing = mkGen $ \_ i -> case i of 
      (NoEvent, NoEvent, NoEvent) -> return (Right NoEvent, go Nothing Nothing Nothing)
      (Event a, NoEvent, NoEvent) -> return (Right NoEvent, go (Just a) Nothing Nothing)
      (NoEvent, Event b, NoEvent) -> return (Right NoEvent, go Nothing (Just b) Nothing)
      (Event a, Event b, NoEvent) -> return (Right NoEvent, go (Just a) (Just b) Nothing)
      (NoEvent, NoEvent, Event c) -> return (Right NoEvent, go Nothing Nothing (Just c))
      (Event a, NoEvent, Event c) -> return (Right NoEvent, go (Just a) Nothing (Just c))
      (NoEvent, Event b, Event c) -> return (Right NoEvent, go Nothing (Just b) (Just c))
      (Event a, Event b, Event c) -> return (Right $ Event (a, b, c), never)

    go (Just a) Nothing Nothing = mkGen $ \_ (_, eb, ec) -> case (eb, ec) of 
      (NoEvent, NoEvent) -> return (Right NoEvent, go (Just a) Nothing Nothing)
      (Event b, NoEvent) -> return (Right NoEvent, go (Just a) (Just b) Nothing)
      (NoEvent, Event c) -> return (Right NoEvent, go (Just a) Nothing (Just c))
      (Event b, Event c) -> return (Right $ Event (a, b, c), never)

    go Nothing (Just b) Nothing = mkGen $ \_ (ea, _, ec) -> case (ea, ec) of 
      (NoEvent, NoEvent) -> return (Right NoEvent, go Nothing (Just b) Nothing)
      (Event a, NoEvent) -> return (Right NoEvent, go (Just a) (Just b) Nothing)
      (NoEvent, Event c) -> return (Right NoEvent, go Nothing (Just b) (Just c))
      (Event a, Event c) -> return (Right $ Event (a, b, c), never)

    go Nothing Nothing (Just c) = mkGen $ \_ (ea, eb, _) -> case (ea, eb) of 
      (NoEvent, NoEvent) -> return (Right NoEvent, go Nothing Nothing (Just c))
      (Event a, NoEvent) -> return (Right NoEvent, go (Just a) Nothing (Just c))
      (NoEvent, Event b) -> return (Right NoEvent, go Nothing (Just b) (Just c))
      (Event a, Event b) -> return (Right $ Event (a, b, c), never)

    go (Just a) (Just b) Nothing = mkGen $ \_ (_, _, ec) -> case ec of 
      NoEvent -> return (Right NoEvent, go (Just a) (Just b) Nothing)
      Event c -> return (Right $ Event (a, b, c), never)

    go (Just a) Nothing (Just c) = mkGen $ \_ (_, eb, _) -> case eb of 
      NoEvent -> return (Right NoEvent, go (Just a) Nothing (Just c))
      Event b -> return (Right $ Event (a, b, c), never)

    go Nothing (Just b) (Just c) = mkGen $ \_ (ea, _, _) -> case ea of 
      NoEvent -> return (Right NoEvent, go Nothing (Just b) (Just c))
      Event a -> return (Right $ Event (a, b, c), never)

    go (Just a) (Just b) (Just c) = mkGen $ \_ _ -> return (Right $ Event (a, b, c), never)
    
-- | Waits for occurence of each event in list after that occurs once
-- with first values of events. 
--
-- Note: the function expects that number of events in list is constant
-- over time.
--
-- TODO: test case when events occurs not in order they passed 
waitAllEvents :: GameWireG cntx [Event a] (Event [a])
waitAllEvents = initLoaders
  where 
    initLoaders = proc es -> do 
      e <- mapE makeLoaders . now -< length es
      rSwitch (pure NoEvent) -< (es, e)

    makeLoaders :: Int -> GameWireG cntx [Event a] (Event [a])
    makeLoaders n = pure NoEvent >-- onceSwitch (merger $ replicate n hold)

    merger :: [GameWireG cntx (Event a) a] -> GameWireG cntx [Event a] (Event [a])
    merger loaders = fmap Event $ zipArrows loaders

-- | Logs lefts to console, pass through rights
filterEitherE :: TextShow e => GameWireG cntx (Event (Either e a)) (Event a)
filterEitherE = proc ev -> case ev of 
  NoEvent -> returnA -< NoEvent
  Event (Left s) -> do 
    logWarnE -< Event $ showt s 
    returnA -< NoEvent
  Event (Right a) -> returnA -< Event a

-- | Logs lefts to console, pass through rights
-- Special version for Texts (don't quote them)
filterEitherE' :: GameWireG cntx (Event (Either Text a)) (Event a)
filterEitherE' = proc ev -> case ev of 
  NoEvent -> returnA -< NoEvent
  Event (Left s) -> do 
    logWarnE -< Event s 
    returnA -< NoEvent
  Event (Right a) -> returnA -< Event a

-- | Passes through Justs, and doesn't pass Nothings
filterJustE :: GameWireG cntx (Event (Maybe a)) (Event a)
filterJustE = proc ev -> case ev of 
  NoEvent -> returnA -< NoEvent 
  Event Nothing -> returnA -< NoEvent 
  Event (Just a) -> returnA -< Event a 

-- | Until first event occurs returns Nothing, then relfecting its latest value with Just a
holdJust :: GameWireG cntx (Event a) (Maybe a)
holdJust = (hold <|> pure Nothing) . mapE Just