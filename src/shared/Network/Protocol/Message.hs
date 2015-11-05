{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module Network.Protocol.Message where 

import Data.Int
import qualified Data.ByteString as BS
import Data.Serialize
import GHC.Generics (Generic)
import Control.DeepSeq
import Data.DeriveTH
import Game.Boxed.Block 
import Data.Vec as Vec 
import Util.Vec()

import Data.Serialize.Text()
import Data.Text (Text)
import TextShow
import TextShow.Generic

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
  | PlayerData !Int !(Vec3 Double) !(Vec3 Double) !(Vec3 Double) -- ^ Server sends that to update client player pos, face and up vectors, respond to PlayerRequestEnviroment
  | BoxedModelsAround ![Int] -- ^ List of boxed model ids around of player
  | RequestBoxedModelData !Int -- ^ Player request data for specific boxed model
  | BoxedModelChunk !Int !(Int, Int, Int) !BS.ByteString -- ^ Server responds with all chunk data for specific boxed model, chunk data is gzip'ed
  | BoxedModelMapping !Int ![(Int32, Text)] -- ^ Server sends boxed model block<->name mapping
  | BoxedModelBlockUpdate !Int !(Int, Int, Int) !BlockId  -- ^ Server sends the message to client when a single block is updated
  | BoxedModelBlockMetaUpdate !Int !(Int, Int, Int) !BlockMeta  -- ^ Server sends the message to client when a single block is updated
  | BoxedModelBlockAndMetaUpdate !Int !(Int, Int, Int) !BlockId !BlockMeta  -- ^ Server sends the message to client when a single block is updated
  | PlayerDebugPlaceBlock !Int !(Int, Int, Int) !Text !BlockMeta -- ^ Debug msg that allows players modify generic boxed model
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