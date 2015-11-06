{-# LANGUAGE Arrows, TupleSections #-}
module Server.Simulation(
    mainWire
  ) where 

import Control.DeepSeq 
import Core 
import Data.Text (Text)
import FRP.Netwire 
import Prelude hiding (id, (.))
import Server.Game.Player 
import Server.Game.World

mainWire :: GameMonad (GameWire () ()) 
mainWire = do
  logInfo "Starting default world..."
  return $ proc _ -> do 
    -- Running predefined world
    (w, wid) <- runIndexed (world "default") -< ()
    -- When player connects, add to default world
    cplayers <- liftEvent spawnPlayers . playersConnected -< ()
    putMessagesE . mapE (fmap $ second SpawnPlayer) -< fmap (wid,) <$> cplayers 
    -- When player disconnect, remove from world
    dplayers <- playersDisconnected -< ()
    putMessagesE . mapE (fmap $ second DespawnPlayer) -< fmap (wid,) <$> dplayers

    forceNF -< w `deepseq` ()

    where 
      spawnPlayers :: GameWire [Text] [Player]
      spawnPlayers = liftGameMonad1 $ mapM $ \name -> do
        i <- registerObject
        return $! Player i name