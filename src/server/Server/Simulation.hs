{-# LANGUAGE Arrows, TupleSections #-}
module Server.Simulation(
    mainWire
  ) where 

import Core 
import Control.DeepSeq 

import Prelude hiding (id, (.))
import FRP.Netwire 

import Server.Game.World
import Server.Game.Player 

import Data.Text (Text)
import Math.Vector

-- | TODO: make config to handle this (and ips, names and others)
debugViewDistance :: Double 
debugViewDistance = 100 

mainWire :: GameMonad (GameWire () ()) 
mainWire = do
  logInfo "Starting default world..."
  return $ proc _ -> do 
    -- Running predefined world
    (w, wid) <- runIndexed (world "default" debugViewDistance) -< ()
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
        return $! Player i name 0 xunit yunit