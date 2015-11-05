{-# LANGUAGE DeriveDataTypeable, TypeFamilies, DeriveGeneric #-}
module Assets.ObjMesh(
    ObjResource(..)
  , ResourceParams(..)
  ) where 

import Assets.ObjMesh.Parser
import Assets.ObjMesh.Converter
import Assets.Resource 
import Graphics.Model
import Control.Monad.Trans.Either
import qualified Data.ByteString.Lazy.UTF8 as BS
import Data.Typeable 
import GHC.Generics (Generic)
import Control.DeepSeq

import Data.Text (Text)
import qualified Data.Text as T 

newtype ObjResource = ObjResource Model 
  deriving (Typeable, Generic)

instance NFData ObjResource

instance Resource ObjResource where 
  data ResourceParams ObjResource = ObjResourceName Text

  loadResource path (ObjResourceName name) bs = hoistEither $ case parseObj (T.unpack path) (BS.toString bs) of 
    Left err -> Left . T.pack $! show err 
    Right obj -> Right . ObjResource $! convertObj name obj

  saveResource _ _ = error "Saving obj models isn't implemented"