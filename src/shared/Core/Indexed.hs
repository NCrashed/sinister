{-# LANGUAGE Arrows, TupleSections #-}
module Core.Indexed(
    GameWireGI(..)
  , updateWireCntrl
  , GameActorG
  , makeIndexed
  , makeFixedIndexed
  , runIndexed
  , runIndexed'
  ) where 

import Core.Monad
import Core.CoreEvent
import Core.Message
import Prelude hiding (id, (.))
import FRP.Netwire

-- | Game wire that is marked by it id type
data GameWireGI e id a b = GameWireGI {
    wireId :: !id
  , wireCntrl :: !(GameWireG e a b)
}

instance Eq id => Eq (GameWireGI e id a b) where 
  (GameWireGI i1 _) == (GameWireGI i2 _) = i1 == i2 

updateWireCntrl :: (GameWireG e a b -> GameWireG e a b) -> GameWireGI e id a b -> GameWireGI e id a b
updateWireCntrl f (GameWireGI i c) = GameWireGI i (f c)

-- | Common pattern in game
-- Usually wires that are actors need context to register themselfes in core.
-- Major part of wire functions operates with such wrapped indexed arrows thats
-- why the convinient type synonym is exists.
type GameActorG cntx i a b = GameMonadG cntx (GameWireGI cntx i a b)

-- | Registers new index for wire and makes an indexed wire
makeIndexed :: Messagable id => 
     (id -> GameWireG cntx a b) -- ^ Your handler, usual wire wich gets index
  -> GameActorG cntx id a b -- ^ Operation that makes the indexed wire
makeIndexed wire = do 
  i <- registerObject
  return $! GameWireGI i (wire i)

-- | Makes indexed wire from given index and handler (index could be retrieved by e.x. network message)
-- Monadic action is used to unify API
makeFixedIndexed :: Messagable id => 
     id -- ^ Wire index
  -> (id -> GameWireG cntx a b) -- ^ Handler
  -> GameActorG cntx id a b
makeFixedIndexed i wire = return $! GameWireGI i (wire i)

-- | If need no dynamic switching, you can use the function to embed index wire just at time
runIndexed :: GameActorG cntx id a b -- ^ Operation that creates indexed wire
  -> GameWireG cntx a (b, id) -- ^ Usual wire that also returns id of inner indexed wire
runIndexed mwire = switch makeWire 
  where 
  -- | Switches immidieatly to created wire, thats why error is used for
  -- value that should be returned in case where there is no event.
  makeWire = proc _ -> do 
    e <- mapE (\iw -> arr (, wireId iw) . wireCntrl iw) . now . liftGameMonadOnce mwire -< ()
    returnA -< (error "runIndexed: impossible", e)

-- | Same as runIndexed but doesn't return id of actor
runIndexed' :: GameActorG cntx id a b -- ^ Operation that creates indexed wire
  -> GameWireG cntx a b -- ^ Usual wire that returns output of actor
runIndexed' mwire = arr fst . runIndexed mwire