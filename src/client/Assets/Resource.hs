{-# LANGUAGE TypeFamilies #-}
module Assets.Resource(
    Resource(..)
  , loadResource'
  , saveResource'
  ) where

import Data.Typeable
import Control.Monad.Trans.Either
import Data.ByteString.Lazy

import Data.Text (Text)
import Data.Monoid

addResName :: Text -> EitherT Text IO a -> EitherT Text IO a
addResName name = bimapEitherT (\s -> "Resource name: " <> name <> ". " <> s) id

-- | Safe version of loadResource that adds resource name to the error string.
loadResource' :: (Resource a) => Text -> ResourceParams a -> ByteString -> EitherT Text IO a
loadResource' name p = addResName name . loadResource name p

-- | Safe version of saveResource that adds resource name to the error string.
saveResource' :: (Resource a) => Text -> ResourceParams a -> a -> EitherT Text IO ByteString
saveResource' name p = addResName name . saveResource p

class Typeable a => Resource a where
  data ResourceParams a :: *
  loadResource :: Text -> ResourceParams a -> ByteString -> EitherT Text IO a
  saveResource :: ResourceParams a -> a -> EitherT Text IO ByteString