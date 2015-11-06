{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module Network.Protocol.Message where 

import Control.DeepSeq
import Data.DeriveTH
import Data.Int
import Data.Serialize
import Data.Serialize.Text()
import Data.Text (Text)
import GHC.Generics (Generic)
import TextShow
import TextShow.Generic
import Util.Vec()

protocolVersion :: Int32
protocolVersion = 100000

data NetworkMessage = 
    HandshakeMsg !Int32  -- ^ Carry protocol version
  | LoginMsg !Text !Text -- ^ Carry login and password hash
  | LoginAccepted -- ^ Message that ackes login
  | TerminationMsg !Text -- ^ Carry description of error
  | DebugMsg !Text -- ^ Debug info message
  | PlayerRequestWorld -- ^ Request world id and name from client
  | PlayerRequestEnviroment -- ^ Request player properties and list of boxed models around 
  | PlayerWorld !Int !Text -- ^ Server responds with that to PlayerRequestWorld to inform about world id
  | PlayerData !Int -- ^ Server sends that to update client player info (currently only id) respond to PlayerRequestEnviroment
  deriving (Generic, Show)

instance Serialize NetworkMessage
instance NFData NetworkMessage
instance TextShow NetworkMessage where 
  showbPrec = genericShowbPrec

$(derive makeIs ''NetworkMessage)

-- | Makes handshake message
handshakeMsg :: NetworkMessage
handshakeMsg = HandshakeMsg protocolVersion

-- | Returns true if the message is handshake and carry right version
checkHandshake :: NetworkMessage -> Bool
checkHandshake (HandshakeMsg v) = v == protocolVersion
checkHandshake _ = False