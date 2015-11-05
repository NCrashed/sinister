module Util.Concurrent(
    readTChanWhileCould
  , readTChanWhileCould'
  , readTQueueWhileCould
  , readTQueueWhileCould'
  ) where 

import Control.Concurrent.STM.TChan 
import Control.Concurrent.STM.TQueue 
import Control.Monad.STM 
import Control.DeepSeq

-- | Tries to extract from TChan while it is full
readTChanWhileCould :: TChan a -> IO [a]
readTChanWhileCould ch = do 
  as <- go []
  return $! reverse as
  where 
    go acc = atomically $ do
      empty <- isEmptyTChan ch 
      case empty of 
        True -> return acc 
        False -> do 
          a <- readTChan ch 
          return $! a : acc 

-- | Tries to extract from TChan while it is full
readTChanWhileCould' :: NFData a => TChan a -> IO [a]
readTChanWhileCould' ch = do 
  as <- go []
  return $! as `deepseq` reverse as
  where 
    go acc = atomically $ do
      empty <- isEmptyTChan ch 
      case empty of 
        True -> return acc 
        False -> do 
          a <- readTChan ch 
          return $! a : acc 


-- | Tries to extract from TQueue while it is full
readTQueueWhileCould :: TQueue a -> IO [a]
readTQueueWhileCould ch = do 
  as <- go []
  return $! reverse as
  where 
    go acc = atomically $ do
      empty <- isEmptyTQueue ch 
      case empty of 
        True -> return acc 
        False -> do 
          a <- readTQueue ch 
          return $! a : acc 

-- | Tries to extract from TQueue while it is full
readTQueueWhileCould' :: NFData a => TQueue a -> IO [a]
readTQueueWhileCould' ch = do 
  as <- go []
  return $! as `deepseq` reverse as
  where 
    go acc = atomically $ do
      empty <- isEmptyTQueue ch 
      case empty of 
        True -> return acc 
        False -> do 
          a <- readTQueue ch 
          return $! a : acc 