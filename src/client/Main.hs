module Main where

import Graphics.Core
import Graphics.GPipe

import Control.Concurrent.MVar
import System.Environment
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Client
import Simulation

import qualified Data.Text as T 
import qualified Data.Text.IO as T 
import Data.Monoid

main :: IO ()
main = do
  [host, port, serverAddr, username] <- fmap T.pack <$> getArgs
  messageBusOutput <- newTChanIO
  messageBusInput <- newTChanIO
  connectBus <- newTChanIO
  disconnectBus <- newTChanIO
  connErrorBus <- newTChanIO
  userInputBus <- newTVarIO (Nothing, [])
  var <- newEmptyMVar -- when filled client is exited
  holdVar <- newEmptyMVar -- when filled connection is initialized (only transport)

  _ <- forkIO $ do
    res <- client host port serverAddr username var holdVar messageBusOutput messageBusInput connectBus disconnectBus
    case res of 
      Nothing -> return ()
      Just err -> do
        atomically $ writeTChan connErrorBus err
        T.putStrLn $ "Error connecting " <> err
        putMVar holdVar ()

  T.putStrLn "Awaiting connection to server..."
  _ <- readMVar holdVar -- ^ Wait for transport initialize
  drawLoop "Epsylon" (100:.100:.()) (800:.600:.()) (
      cubeRenderer messageBusOutput messageBusInput connectBus disconnectBus connErrorBus userInputBus username) userInputBus $ do
    putMVar var True -- true forces client to close transport