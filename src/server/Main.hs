module Main where

import Network.Transport
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Network.Socket.Internal (withSocketsDo)
import Control.Concurrent
import Control.Concurrent.STM.TQueue
import Control.Exception
import System.Environment

import Core 
import Network.Protocol.Message
import Server.Simulation 

import qualified Data.Text as T 
import qualified Data.Text.IO as T 
import Data.Text (Text)

onCtrlC :: IO a -> IO () -> IO a
p `onCtrlC` q = catchJust isUserInterrupt p (const $ q >> p `onCtrlC` q)
  where
    isUserInterrupt :: AsyncException -> Maybe ()
    isUserInterrupt UserInterrupt = Just ()
    isUserInterrupt _             = Nothing

main :: IO ()
main = withSocketsDo $ do
  [host, port]    <- getArgs
  serverDone      <- newEmptyMVar
  mtransport <- createTransport host port defaultTCPParameters
  case mtransport of 
    Left err -> 
      T.putStrLn $ T.unwords ["Failed to start transport:", T.pack $ show err]
    Right transport -> do 
      mendpoint <- newEndPoint transport
      case mendpoint of 
        Left err ->
          T.putStrLn $ T.unwords ["Failed to create endpoint:", T.pack $ show err]
        Right endpoint -> do
          messageBus <- newTQueueIO
          connChan <- newTQueueIO
          disconnChan <- newTQueueIO
          _ <- forkIO $ coreServer endpoint serverDone messageBus connChan disconnChan
          _ <- forkIO $ simulation messageBus connChan disconnChan
          T.putStrLn $ T.unwords ["Aerospace server started at", T.pack $ show (address endpoint)]
          readMVar serverDone `onCtrlC` closeTransport transport

-- | Thread that runs simulation steps
simulation :: TQueue (Text, NetworkMessage) -- ^ Channel with received network messages from server core
  -> TQueue Text -- ^ Channel with names of connected players
  -> TQueue Text -- ^ Channel with names of disconnected players
  -> IO ()
simulation messageBus connChan disconnChan = go $ initialGameState mainWire
  where go s = stepGame messageBus connChan disconnChan s >>= go 