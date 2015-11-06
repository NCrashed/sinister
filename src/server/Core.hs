module Core(
    coreServer
  , module X
  ) where 

import Core.Context as X 
import Core.Monad as X 
import Core.Event as X 
import Core.Message as X 
import Core.State as X 
import Core.Log as X
import Core.Indexed as X
import Core.CoreEvent as X 

import Network.Transport
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TQueue
import Data.Map as M
import Data.Maybe 

import Data.Serialize
import Network.Protocol.Message
import qualified Data.ByteString as BS
import Control.Monad 
import Control.DeepSeq 

import Authentication 

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import TextShow 
import Data.Monoid 

-- | Main server loop. Handle connections, handshake and authentification
coreServer :: EndPoint -- ^ Local end point (this server)
  -> MVar () -- ^ Flag to let main thread exit
  -> TQueue (Text, NetworkMessage) -- ^ Channel to send/receive messages from simulation thread
  -> TQueue Text -- ^ Channel to inform about player login
  -> TQueue Text -- ^ Channel to inform about player logout (disconnect)
  -> IO ()
coreServer endpoint serverDone messageBus connectChan disconnectChan = do
  name2connVar <- newMVar empty
  _ <- forkIO $ forever $ readMessage name2connVar
  go empty empty name2connVar
  where
    readMessage :: MVar (Map Text Connection) -> IO ()
    readMessage name2connVar = do 
      (name, msg) <- atomically $ readTQueue messageBus
      name2conn <- readMVar name2connVar
      case M.lookup name name2conn of 
        Nothing -> T.putStrLn $ T.unwords ["Failed to find conn with name", name]
        Just conn -> void $ send conn [encode msg]

    go :: Map ConnectionId (MVar (Either Connection AuthConnection)) -- ^ Carry list of authorised connections and not authorised ones
      -> Map EndPointAddress [ConnectionId] -- ^ Carry info about connections for each endpoint (to close them all properly)
      -> MVar (Map Text Connection) -- ^ Mapping from player name to connection
      -> IO ()
    go cs endPointIds name2connVar = do
      -- block on awaiting input events
      event <- receive endpoint
      case event of
        ConnectionOpened cid rel addr -> do
          T.putStrLn $ T.unwords ["  New connection: ID ", showt cid, ", reliability: ", T.pack $ show rel, ", address: ", T.pack $ show addr]
          connMVar <- newEmptyMVar
          _ <- forkIO $ do
            mconn <- connect endpoint addr rel defaultConnectHints
            case mconn of 
              Right conn -> putMVar connMVar $! Left conn
              Left err -> T.putStrLn $ T.unwords ["Failed to establish connection with"
                , showt cid, ":", T.pack $ show err]
          go (insert cid connMVar cs) 
             (alter (\ids -> Just $ maybe [cid] (cid :) ids) addr endPointIds) 
             name2connVar
        -- Got message from connection
        Received cid payload -> do
          _ <- forkIO $ do
            -- print $ payload -- DEBUG
            let mvar = cs ! cid
            conn <- readMVar mvar
            case conn of 
              -- Connection that didn't handshake
              Left c -> case decode $ BS.concat payload of 
                Right m -> if checkHandshake m 
                  then do
                    T.putStrLn $ "Got handshake message from " <> showt cid
                    _ <- send c [encode handshakeMsg]
                    void $ swapMVar mvar (Right $! AuthConnection c Nothing)
                  else do 
                    print m
                    _ <- send c [encode $ TerminationMsg "Protocol version missmatch"]
                    close c 
                _ -> do
                  T.putStrLn $ "Failed to deserialize message for " <> showt cid
                  close c  
              -- Connection that did handshake
              Right c -> case decode $ BS.concat payload of 
                Right m -> if isAuthed c 
                  -- If authed then send message to simulation thread
                  then do 
                    T.putStrLn $ T.unwords ["Putting into bus", showt (fromJust (authName c), m)] -- DEBUG
                    atomically $ writeTQueue messageBus (fromJust (authName c), m)
                    yield
                  -- We can only check if it is an authorization message
                  else case m of 
                    LoginMsg nameStr _ -> do
                      T.putStrLn $ "Got login message from " <> showt cid <> " with name " <> showt nameStr 
                      -- TODO: password check
                      -- Disconnect logged with that name
                      name2conn <- readMVar name2connVar
                      case M.lookup nameStr name2conn of 
                        Just _ -> do 
                          _ <- send (authConn c) [encode $ TerminationMsg "Logged from another client"] 
                          close (authConn c)
                        _ -> do 
                          -- Send ACK that peer is authorised
                          _ <- send (authConn c) [encode LoginAccepted]
                          -- Update connection, set to authorized (has name)
                          void $ swapMVar mvar (Right $! c { authName = Just nameStr })
                          -- Update name2conn mapping
                          void $ swapMVar name2connVar $! M.insert nameStr (authConn c) name2conn
                          -- Inform simulation thread about connection
                          atomically $ nameStr `deepseq` writeTQueue connectChan nameStr
                          yield
                    _ -> do 
                      _ <- send (authConn c) [encode $ TerminationMsg "Authentication needed"]
                      close (authConn c)
                _ -> do
                  T.putStrLn $ "Failed to deserialize message for " <> showt cid
                  close $ authConn c 
            return ()
          go cs endPointIds name2connVar
        ConnectionClosed cid -> do
          T.putStrLn $ "    Closed connection: ID " <> showt cid
          -- Close connection
          _ <- forkIO $ do
            conn <- readMVar (cs ! cid)
            case conn of 
              Left c -> close c 
              Right c -> close $ authConn c
          -- Update mapping
          let cs' = delete cid cs 
              endPointIds' = fmap (Prelude.filter (/=cid)) endPointIds
          name2conn  <- readMVar name2connVar
          void $ swapMVar name2connVar =<< deleteFromNameMapping cid name2conn
          go cs' endPointIds' name2connVar
        EndPointClosed -> do
          T.putStrLn "Echo server exiting"
          putMVar serverDone ()
        -- | All connections from specific endpoint are broken
        ErrorEvent (TransportError (EventConnectionLost addr) _) -> do
          case M.lookup addr endPointIds of 
            Nothing -> do
              T.putStrLn $ T.unwords ["Connection with (not initialized, it is not normal)", T.pack $ show addr, "is lost!"]
              go cs endPointIds name2connVar
            Just ids -> do  
              T.putStrLn $ T.unwords ["Connection with", T.pack $ show addr, "is lost!"]
              let cs' = Prelude.foldr delete cs ids
                  endPointIds' = delete addr endPointIds
              name2conn <- readMVar name2connVar
              void $ swapMVar name2connVar =<< foldM (flip deleteFromNameMapping) name2conn ids
              go cs' endPointIds' name2connVar
        ErrorEvent err -> do 
          print err 
          putMVar serverDone () 
        ReceivedMulticast _ _ -> go cs endPointIds name2connVar
      where 
        deleteFromNameMapping :: ConnectionId -> Map Text Connection -> IO (Map Text Connection)
        deleteFromNameMapping cid n2c =
          case M.lookup cid cs of 
            Just mvar -> do 
              conn <- readMVar mvar 
              case conn of 
                (Right aconn) -> case authName aconn of 
                  Just s -> do
                    -- Inform simulation thread about disconnection
                    atomically $ s `deepseq` writeTQueue disconnectChan s
                    yield
                    -- Update mapping
                    return $ M.delete s n2c
                  Nothing -> return n2c
                _ -> return n2c
            Nothing -> return n2c 