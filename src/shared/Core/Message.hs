{-# LANGUAGE TypeFamilies, Arrows, FlexibleContexts #-}
module Core.Message(
    Messagable(..)
  , registerObject
  , removeObject
  , getMessages
  , processMessages
  , processMessagesM
  , putMessage
  , putMessages
  , putMessageE
  , putMessagesE
  , putMessage'
  , putMessages'
  ) where

import Prelude hiding ((.), id)
import FRP.Netwire 
import Control.Wire.Unsafe.Event (onEventM)

import Core.Monad
import Data.Typeable
import Data.Dynamic
import Data.Maybe
import Data.HashMap.Strict (insert, delete, adjust)
import qualified Data.HashMap.Strict as HM

import Control.DeepSeq
import Control.Monad 

-- | The typeclass separates message API's of different type of actors
--  
-- In general you don't want to have one global type to handle all possible types of messages,
-- it will break modularity. Thats why you creates (with newtype) seperate types of ids for
-- each actor and statically binds message type (usually algebraic type) to the id.
--
-- The class implies that your id is some integer type, but it could be not. Just provide way
-- to stable convertion of you id to integer and vice-versa.
class Messagable objectId where 
  -- | Binded message type, mailbox with id type of objectId would accept only this message type
  type MessageType objectId :: * 
  -- | Convertion from global counter. Don't use it in client code as it could break type safety.
  fromCounter :: Int -> objectId
  -- | Convertion to global counter. Don't use it in client code as it could break type safety.
  toCounter :: objectId -> Int

-- | Creates new mailbox in message manager
-- After aquiring objectId you can send and retrieve message from the mailbox with
-- putMessages/putMessageE/.. and processMessages/processMessagesM functions.
registerObject :: Messagable objectId => GameMonadG e objectId
registerObject = do
  mng <- getMsgMng 
  let i = nextIndex mng
      boxes = objectBoxes mng 
  putMsgMng $ mng { nextIndex = i+1, objectBoxes = insert i [] boxes }  
  return $ fromCounter i

-- | Unregisters mailbox from message manager. You should call that when destroying
-- actors. 
--
-- TODO: add high-level destruction of actors
removeObject :: Messagable objectId => objectId -> GameMonadG e ()
removeObject oid = do 
  mng <- getMsgMng 
  putMsgMng $ mng { objectBoxes = delete (toCounter oid) $ objectBoxes mng }

-- | Getting all messages from mailbox for specified id.
-- If there was type safety failure, you would get only messages with correct types. 
--
-- Note: the function is expected to be used for one reader-actor, reading messages for one id
-- from different places is wreid idea.
-- 
-- Note: mailbox is purged after the operation
getMessages' :: (Messagable objectId, Typeable (MessageType objectId)) => objectId -> GameMonadG e [MessageType objectId]
getMessages' oid = do 
  mng <- getMsgMng 
  let 
    msgs :: [Dynamic]
    msgs = fromMaybe [] (toCounter oid `HM.lookup` objectBoxes mng)
  putMsgMng $ mng { objectBoxes = adjust (const []) (toCounter oid) $ objectBoxes mng }
  return.catMaybes $ fromDynamic <$> msgs

-- | Putting message in mailbox
-- You can put only messages that correspond to id type. After the operation one can read the message
-- from mailbox in same frame of simulation.
putMessage' :: (Messagable objectId, Typeable (MessageType objectId), NFData (MessageType objectId)) 
  => objectId -- ^ id of mailbox (id of actor)
  -> MessageType objectId  -- ^ Message type corresponding to objectId
  -> GameMonadG e () -- ^ Action that puts the message into the mailbox
putMessage' oid msg = msg `deepseq` do 
  mng <- getMsgMng 
  let boxes = objectBoxes mng 
  putMsgMng $ mng { objectBoxes = adjust (toDyn msg :) (toCounter oid) boxes }

-- | Putting messages in mailbox
-- See putMessage'
putMessages' :: (Messagable objectId, Typeable (MessageType objectId), NFData (MessageType objectId)) 
  => [(objectId, MessageType objectId)] -> GameMonadG e ()
putMessages' msgs = mapM_ (uncurry putMessage') msgs 

-- | Arrow that takes id of mailbox and returns list of messages of corresponding types
-- Sedd getMessages'
getMessages :: (Messagable objectId, Typeable (MessageType objectId)) => GameWireG e objectId [MessageType objectId]
getMessages = liftGameMonad1 getMessages'  

-- | High-level function (aka fold over messages) that applies handler to messages and updates accumulator (typically object state)
-- If you need monadic update, use processMessagesM
-- 
-- Common usage (updating inner state of actor):
-- @@
-- someActor initialState = makeIndexed $ \i -> loop $ (_, innerState) -> do 
--   innerState' <- processMessages handler i . delay initialState <- innerState
--   -- ...
-- @@
processMessages :: (Messagable objectId, Typeable (MessageType objectId)) => 
     (a -> MessageType objectId -> a) -- ^ Updater, takes object state and message and performs updating
  -> objectId  -- ^ Id of mailbox to take messages from
  -> GameWireG e a a -- ^ Resulting wire that transforms object state with messages
processMessages f oid = proc m -> do 
      msgs <- getMessages -< oid 
      returnA -< foldl f m msgs

-- | Variation of processMessages function that uses monadic context for updating
-- See processMessages
processMessagesM :: (Messagable objectId, Typeable (MessageType objectId)) => 
  (a -> MessageType objectId -> GameMonadG e a) -- ^ Updater that takes object state and message and performs monadic update
  -> objectId -- ^ Id of mailbox to take messages from
  -> GameWireG e a a  -- ^ Resulting wire that transforms object state with messages
processMessagesM f oid = proc m -> do 
      msgs <- getMessages -< oid 
      liftGameMonad2 (\m msgs -> foldM f m msgs) -< (m, msgs)

-- | Putting message of type that corresponds to id of mailbox
-- See putMessage'
--
-- Note: The messages are put to mailbox at each frame, for discrete semantic see putMessageE
putMessage :: (Messagable objectId, Typeable (MessageType objectId), NFData (MessageType objectId)) 
  => GameWireG e (objectId, MessageType objectId) ()
putMessage = liftGameMonad2 putMessage'

-- | Discrete semantic variation of putMessage
-- Puts message of corresponding type to mailbox by event. As most messages are wrapped with event,
-- the variation is most used across the game.
--
-- You can put only messages that correspond to id type. After the operation one can read the message
-- from mailbox in same frame of simulation.
--
-- Example (putting message once at creation of actor):
-- @@
-- myActor = makeIndexed $ \i -> proc otherId -> do 
--   putMessageE . now -< (otherId, msg)
-- @@
putMessageE :: (Messagable objectId, Typeable (MessageType objectId), NFData (MessageType objectId)) 
  => GameWireG e (Event (objectId, MessageType objectId)) (Event ())
putMessageE  = onEventM $ uncurry putMessage'

-- | Variation of putMessage for many messages
-- See putMessage
putMessages :: (Messagable objectId, Typeable (MessageType objectId), NFData (MessageType objectId)) 
  => GameWireG e [(objectId, MessageType objectId)] ()
putMessages = liftGameMonad1 putMessages'

-- | Variation of putMessageE for many messages 
-- See putMessageE
putMessagesE :: (Messagable objectId, Typeable (MessageType objectId), NFData (MessageType objectId)) 
  => GameWireG e (Event [(objectId, MessageType objectId)]) (Event ())
putMessagesE = onEventM putMessages'