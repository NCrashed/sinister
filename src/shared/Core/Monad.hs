{-# LANGUAGE StandaloneDeriving, DeriveGeneric, Arrows #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Core.Monad(
  -- | Core types
    GameSession
  , GameWireG
  , GameMonadG
  , GameContextG(..)
  , GameContextClass(..)
  , newGameContext
  -- | Lifting game monad to game arrow
  , liftGameMonad
  , liftGameMonad1
  , liftGameMonad2
  , liftGameMonadOnce
  , liftGameMonad1Once
  , liftGameMonad2Once
  -- | Generic message management between wires
  , MessageManager(..)
  , getMsgMng
  , putMsgMng
  -- | Block manager
  , BlockManager(..)
  , getBlockMng
  , putBlockMng
  -- | Arrow utils
  , zipArrows
  , onceSwitch
  ) where

import Prelude hiding (id, (.))
import FRP.Netwire
import Control.Wire (NominalDiffTime)
import Control.Wire.Core (mkGen_, mkGen, stepWire)

import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Dynamic

import GHC.Generics (Generic)
import Control.Monad.State.Strict
import Control.DeepSeq
import qualified Data.Sequence as S
import Game.Boxed.Block 

import Data.Text (Text)

-- | Game session that is used to calculate delta time for simulation
type GameSession = Session IO (Timed NominalDiffTime ())
-- | Main type of arrows in game, client and server parametries it with it's own context
type GameWireG cntx a b = Wire (Timed NominalDiffTime ()) () (GameMonadG cntx) a b
-- | Underlying monad of game wire, carries generic context and custom context
type GameMonadG cntx = State (GameContextG cntx)

-- | Takes game monad and wraps it into game wire
-- Note: Result of wire is calclulated each frame.
liftGameMonad :: GameMonadG cntx b -> GameWireG cntx a b
liftGameMonad action = mkGen_ $ \ _ -> do 
  val <- action 
  return $ Right val

-- | Takes game monad and wraps it into game wire
-- Note: Result of wire is calclulated each frame.
-- Version for game monad with 1 parameter.
liftGameMonad1 :: (a -> GameMonadG cntx b) -> GameWireG cntx a b
liftGameMonad1 action = mkGen_ $ \ a -> do 
  val <- action a
  return $ Right val

-- | Takes game monad and wraps it into game wire
-- Note: Result of wire is calclulated each frame.
-- Version for game monad with 2 parameters.
liftGameMonad2 :: (a -> b -> GameMonadG cntx c) -> GameWireG cntx (a, b) c
liftGameMonad2 action = mkGen_ $ \(a, b) -> do 
  val <- action a b
  return $ Right val

-- Feel fre to add liftGameMonad3 and further if needed 

-- | Takes game monad and wraps it into game wire
-- Note: Result of wire is calculated ONCE and next execution returns cached value
--
-- Very usefull for creation of actors, the function is used heavily in actor API
liftGameMonadOnce :: GameMonadG cntx b -> GameWireG cntx a b 
liftGameMonadOnce action = mkGen $ \_ _ -> do 
  val <- action 
  return (Right val, pure val)

-- | Takes game monad and wraps it into game wire
-- Note: Result of wire is calculated ONCE and next execution returns cached value
--
-- Very usefull for creation of actors, the function is used heavily in actor API
-- Version for game monad with 1 parameter.
liftGameMonad1Once :: (a -> GameMonadG cntx b) -> GameWireG cntx a b
liftGameMonad1Once action = mkGen $ \_ a -> do 
  val <- action a
  return (Right val, pure val)

-- | Takes game monad and wraps it into game wire
-- Note: Result of wire is calculated ONCE and next execution returns cached value
--
-- Very usefull for creation of actors, the function is used heavily in actor API
-- Version for game monad with 2 parameters.
liftGameMonad2Once :: (a -> b -> GameMonadG cntx c) -> GameWireG cntx (a,b) c 
liftGameMonad2Once action = mkGen $ \_ (a,b) -> do 
  val <- action a b
  return (Right val, pure val)

-- Feel fre to add liftGameMonad3Once and further if needed 

-- | Generic game context that is extendable via cntx parameter
data GameContextG cntx  = GameContextG {
  -- | Generic message manager for handling message passing API between arrows
    gameMessageManager :: !MessageManager
  -- | Custon context (client or server sprecific stuff)
  , gameCustomContext :: !cntx
  -- | Generic queue of log messages
  , gameLogMessages :: !(S.Seq Text)
  -- | Generic block manager, where client and server holds known blocks
  , gameBlockManager :: !BlockManager
} deriving (Generic)

-- | Forced when calculated next game state
instance NFData e => NFData (GameContextG e)

-- | Operations that custom context of client or server should support
class GameContextClass cntx where 
  -- | Creation new instance of custom context
  newCustomContext :: cntx

-- | Helper for creating new instance of generic context
newGameContext :: GameContextClass cntx => GameContextG cntx
newGameContext = GameContextG newMessageManager newCustomContext S.empty newBlockManager

-- | Retrievies message manager from game context
getMsgMng :: GameMonadG cntx MessageManager 
getMsgMng = fmap gameMessageManager get 

-- | Writes down message manager to game context
putMsgMng :: MessageManager -> GameMonadG cntx ()
putMsgMng mng = do 
  cntx <- get 
  put $ cntx { gameMessageManager = mng }

-- | Generic message manager for handling message passing between wires
-- To pass message from one wire to another you should now id of desination mailbox.
-- Also to read message from own mailbox you should now your own id.
--
-- Mailbox and id is a bijection. When you registers new id, new mailbox is created.
data MessageManager = MessageManager {
  -- | Next available id, incremented each time you register a mailbox
    nextIndex :: !Int
  -- | Mapping between id and mailboxes. Each mailbox is simple list of messages.
  -- Message has type of Dynamic as message manager doesn't know anything about message types.
  -- We don't need to serialization protocol due passing via memory. Type safety is forced 
  -- with Messagable type class with type family (see Core.Message module).
  , objectBoxes :: !(HashMap Int [Dynamic])
} deriving (Generic)

instance NFData Dynamic where 
  rnf = (`seq` ())

instance NFData MessageManager

-- | Creates empty message manager
newMessageManager :: MessageManager 
newMessageManager = MessageManager 1 HM.empty

-- | Holds all known blocks, in future mods will use the API to add it ownt blocks
data BlockManager = BlockManager {
  -- | Mapping from name to block, block name is unique
  blockMngBlocks :: HashMap Text Block
} deriving (Generic)

instance NFData BlockManager 

-- | Makes empty block manager
newBlockManager :: BlockManager 
newBlockManager = BlockManager (HM.fromList $ fmap blockName standartBlocks `zip` standartBlocks)

-- | Low-level API for getting block manager
getBlockMng :: GameMonadG cntx BlockManager 
getBlockMng = fmap gameBlockManager get 

-- | Low-level API for saving block manager
putBlockMng :: BlockManager -> GameMonadG cntx ()
putBlockMng bmng = do 
  cntx <- get 
  put $ cntx { gameBlockManager = bmng }

-- | Pass through first occurence and then forget about event producer
-- Note: netwire once combinator still holds it event producer when event
-- is produced.
onceSwitch :: GameWireG cntx a (Event b) -> GameWireG cntx a (Event b)
onceSwitch w = proc a -> do 
  e <- w -< a 
  drSwitch id -< (e, fmap (const never) e)

-- | Passes input values two predefined set of wires
-- If one of wire inhibits, all wire will inhibit
zipArrows :: [GameWireG cntx a b] -> GameWireG cntx [a] [b]
zipArrows ws = mkGen $ \s as -> do 
  (ebs, ws') <- fmap unzip $ zipWithM (\w a -> stepWire w s (Right a)) ws as
  return $ length ebs `seq` length ws' `seq` (sequence ebs, zipArrows ws')