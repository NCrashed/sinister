module Client(
    client
  ) where

import Network.Transport
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Network.Socket.Internal (withSocketsDo)

import Network.Protocol.Message
import Data.Serialize
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Control.Monad 

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T 
import qualified Data.Text.IO as T
import Data.Text (Text)
import TextShow 

client :: Text -- ^ Client host
  -> Text -- ^ Client port
  -> Text -- ^ Server address of form "ip:port:node" (node is usually 0)
  -> Text -- ^ Username
  -> MVar Bool -- ^ When the var is filled, client stops listening
  -> MVar () -- ^ Client fills that mvar when transport is initalized
  -> TChan NetworkMessage -- ^ Channel with messages to send to server
  -> TChan NetworkMessage -- ^ Channel with messages to process from server
  -> TChan () -- ^ Channel that is filled when player is connected
  -> TChan Text -- ^ Channel that is filled when player is disconnected (with disconnection reason)
  -> IO (Maybe Text) -- ^ Noting is ok, otherwise carry connection error msg
client host port serverAddr username whenClose initalizeVar messageBusOutput messageBusInput connectBus disconnectBus = withSocketsDo $ do 
  mt <- createTransport (T.unpack host) (T.unpack port) defaultTCPParameters
  case mt of 
    Left err -> return $! Just $! showt err 
    Right transport -> do 
      me <- newEndPoint transport 
      case me of 
        Left err -> return . Just . T.pack . show $! err 
        Right endpoint -> do 
          let addr = EndPointAddress (BS.pack $ T.unpack serverAddr)
          mc <- connect endpoint addr ReliableOrdered $ defaultConnectHints { connectTimeout = Just 100000 }
          case mc of 
            Left err -> return . Just . T.pack . show $! err 
            Right conn -> do 
              putMVar initalizeVar ()
              -- Start listening close event
              _ <- forkIO $ do 
                needCloseTransport <- readMVar whenClose
                when needCloseTransport $ do
                  close conn
                  closeTransport transport
                  atomically $ writeTChan disconnectBus "Closed by user"

              -- Handshake and loggin protocol
              receive endpoint >>= T.putStrLn . T.pack . show -- connect event

              _ <- send conn [encode handshakeMsg]
              receive endpoint >>= T.putStrLn . T.pack . show -- handshake ack
              
              _ <- send conn [encode $ LoginMsg username ""]
              receive endpoint >>= T.putStrLn . T.pack . show -- login ack

              -- Start listening messages to send
              _ <- forkIO $ forever $ do 
                msg <- atomically $ readTChan messageBusOutput
                res <- send conn [encode msg]
                case res of 
                  Left err -> T.putStrLn $ T.unwords ["Failed to send msg ", showt msg, ", due: ", T.pack $ show err]
                  Right () -> return ()

              -- Allow client draw thread to initalize GLUT (if it happens before server couldn't connect, don't know why)
              atomically $ writeTChan connectBus () -- loggin event 

              -- Start listening
              go conn transport endpoint
              putMVar whenClose False -- stop thread which checks exit var
              return $! Nothing
  where 
    go conn transport endpoint = do 
      event <- receive endpoint
      case event of 
        Received _ payload -> do
          case decode $ BS.concat payload of 
            Left err -> T.putStrLn $ T.unwords ["Failed to deserialize msg", showt payload, ", with error: ", T.pack err]
            Right msg -> atomically $ writeTChan messageBusInput msg 
          go conn transport endpoint
        ConnectionClosed _ -> do 
          putStrLn "Connection to server is closed"
          close conn
          closeTransport transport
          atomically $ writeTChan disconnectBus "Closed by server"
        ErrorEvent (TransportError _ msg) -> do 
          putStrLn $ "Error on transport layer: " ++ msg
          close conn
          closeTransport transport
          atomically $ writeTChan disconnectBus $ T.pack msg
        ev -> do 
          T.putStrLn $ T.unwords ["Unknown transport event:", T.pack $ show ev]
          go conn transport endpoint