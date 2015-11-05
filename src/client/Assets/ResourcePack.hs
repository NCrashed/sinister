{-# LANGUAGE DeriveDataTypeable #-}
module Assets.ResourcePack(
    ResourcePack()
  , resourcePackName
  , resourcePackPath
  , newResourcePack
  , getResource
  , setResource
  , finalizePack
  ) where
  
import Prelude hiding (lookup)  
import Data.HashMap
import Data.Dynamic
import Assets.Resource
import Assets.Archive
import Control.Monad.Trans.Either

import qualified Data.Text as T 
import Data.Text (Text)

data ResourcePack a = ResourcePack Text a ResourceCache
  deriving (Typeable)
  
type ResourceCache = Map FilePath Dynamic 

resourcePackName :: ResourcePack a -> Text
resourcePackName (ResourcePack name _ _) = name

resourcePackPath :: (Archive a) => ResourcePack a -> FilePath
resourcePackPath (ResourcePack _ archive _) = archivePath archive

newResourcePack :: (Archive a) => Text -> a -> ResourcePack a
newResourcePack name archive = ResourcePack name archive empty

getResource :: (Archive a, Resource b) => ResourcePack a -> FilePath -> ResourceParams b -> EitherT Text IO (b, ResourcePack a) 
getResource pack@(ResourcePack name archive cache) path params =
  case lookup path cache of
    Nothing -> newres
    Just cached -> 
      case fromDynamic cached of
        Nothing -> newres
        Just res -> right (res, pack)
  where
    newres = do
      res <- loadResource' (T.pack path) params =<< readArchiveFile archive path
      right (res, ResourcePack name archive (insert path (toDyn res) cache))

setResource :: (Archive a, Resource b) => ResourcePack a -> FilePath -> ResourceParams b -> b -> EitherT Text IO (ResourcePack a)
setResource (ResourcePack name archive cache) path params res = do
  writeArchiveFile archive path =<< saveResource' (T.pack path) params res
  right $ ResourcePack name archive $ insert path (toDyn res) cache
  
finalizePack :: (Archive a) => ResourcePack a -> IO ()
finalizePack (ResourcePack _ archive _) = closeArchive archive  