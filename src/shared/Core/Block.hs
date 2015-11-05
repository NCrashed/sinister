module Core.Block(
  -- | Getting blocks
    knownBlocks
  , knownBlocksNames
  , knownBlocksList
  , getKnownBlockM
  -- | Registering blocks
  , registerBlockM 
  , registerBlocksM
  , rewriteBlockM
  , rewriteBlocksM
  , unregisterBlockM
  , unregisterBlocksM
  -- | Arrow API
  , getKnownBlock
  , registerBlock
  , registerBlocks 
  , rewriteBlock 
  , rewriteBlocks
  , unregisterBlock
  , unregisterBlocks
  -- | Arrow event API
  , registerBlockE
  , registerBlocksE
  , rewriteBlockE
  , rewriteBlocksE
  , unregisterBlockE
  , unregisterBlocksE
  ) where 

import Core.Monad
import qualified Data.HashMap.Strict as M
import qualified Data.Foldable as F
import Game.Boxed.Block 
import Control.DeepSeq 
import Control.Wire.Unsafe.Event 

import Data.Text (Text)

-- | Returns all registered blocks
knownBlocks :: GameMonadG cntx (M.HashMap Text Block)
knownBlocks = fmap blockMngBlocks getBlockMng

-- | Returns list of all known names of blocks
knownBlocksNames :: GameMonadG cntx [Text]
knownBlocksNames = fmap M.keys knownBlocks

-- | Simplified version of @knownBlocks@ that retuns list of blocks
knownBlocksList :: GameMonadG cntx [Block]
knownBlocksList = fmap M.elems knownBlocks

-- | Find block by name
getKnownBlockM :: Text -> GameMonadG cntx (Maybe Block)
getKnownBlockM bName = fmap (M.lookup bName) knownBlocks

-- | Tries to register block, if there is already presented block, returns False
registerBlockM :: Block -> GameMonadG cntx Bool 
registerBlockM b = do
  mng <- getBlockMng 
  case M.lookup (blockName b) (blockMngBlocks mng) of 
    Just _ -> return False 
    Nothing -> do 
      putBlockMng $!! mng { blockMngBlocks = M.insert (blockName b) b $ blockMngBlocks mng }
      return True 

-- | Tries to register collection of blocks, if there is already presented blocks doesn't rewrite them
registerBlocksM :: F.Foldable f => f Block -> GameMonadG cntx ()
registerBlocksM = F.mapM_ registerBlockM

-- | Registers block in block manager, if block is exists, rewrites it
rewriteBlockM :: Block -> GameMonadG cntx () 
rewriteBlockM b = do 
  mng <- getBlockMng 
  putBlockMng $!! mng { blockMngBlocks = M.insert (blockName b) b $ blockMngBlocks mng }

-- | Registers collection of blocks, if some blocks are already exist, rewrites it
rewriteBlocksM :: F.Foldable f => f Block -> GameMonadG cntx ()
rewriteBlocksM = F.mapM_ rewriteBlockM

-- | Removes block from block manager, returns True if block is actually removed
unregisterBlockM :: Block -> GameMonadG cntx Bool 
unregisterBlockM b = do 
  mng <- getBlockMng
  case M.lookup (blockName b) (blockMngBlocks mng) of 
    Just _ -> return False 
    Nothing -> do 
      putBlockMng $!! mng { blockMngBlocks = M.delete (blockName b) $ blockMngBlocks mng }
      return True 

-- | Unregisters all blocks in given collection
unregisterBlocksM :: F.Foldable f => f Block -> GameMonadG cntx ()
unregisterBlocksM = F.mapM_ unregisterBlockM

-- | Returns block by name if found
getKnownBlock :: GameWireG cntx Text (Maybe Block)
getKnownBlock = liftGameMonad1 getKnownBlockM 

-- | Registers block on each frame, returns True if registered
registerBlock :: GameWireG cntx Block Bool
registerBlock = liftGameMonad1 registerBlockM 

-- | Registers collection of blocks on each frame, doesn't overwrite blocks
registerBlocks :: F.Foldable f => GameWireG cntx (f Block) ()
registerBlocks = liftGameMonad1 registerBlocksM

-- | Registers block on each frame, does overwriting
rewriteBlock :: GameWireG cntx Block ()
rewriteBlock = liftGameMonad1 rewriteBlockM

-- | Register collection of blocks on each frame, overwrites blocks
rewriteBlocks :: F.Foldable f => GameWireG cntx (f Block) ()
rewriteBlocks = liftGameMonad1 rewriteBlocksM

-- | Removes block from block manager on each frame
unregisterBlock :: GameWireG cntx Block Bool 
unregisterBlock = liftGameMonad1 unregisterBlockM

-- | Removes collection of blocks on each frame
unregisterBlocks :: F.Foldable f => GameWireG cntx (f Block) ()
unregisterBlocks = liftGameMonad1 unregisterBlocksM

-- | On event registers block, returns True if block was registered
registerBlockE :: GameWireG cntx (Event Block) (Event Bool)
registerBlockE = onEventM registerBlockM

-- | On event registers collection of blocks, doesn't overwrite existing blocks
registerBlocksE :: F.Foldable f => GameWireG cntx (Event (f Block)) (Event ())
registerBlocksE = onEventM registerBlocksM 

-- | On event register blocks, overwrites existing blocks
rewriteBlockE :: GameWireG cntx (Event Block) (Event ())
rewriteBlockE = onEventM rewriteBlockM 

-- | On event register collection of blocks, overwrites existing blocks
rewriteBlocksE :: F.Foldable f => GameWireG cntx (Event (f Block)) (Event ())
rewriteBlocksE = onEventM rewriteBlocksM 

-- | On event unregister block from block manager, returns True if block is actually unregistered
unregisterBlockE :: GameWireG cntx (Event Block) (Event Bool)
unregisterBlockE = onEventM unregisterBlockM

-- | On event unregister collection of blocks
unregisterBlocksE :: F.Foldable f => GameWireG cntx (Event (f Block)) (Event ())
unregisterBlocksE = onEventM unregisterBlocksM