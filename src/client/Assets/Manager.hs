{-# LANGUAGE ExistentialQuantification, ViewPatterns #-}
module Assets.Manager(
    ResourceManager()
  , emptyResourceManager
  , addNewFileSystemPack
  , getResource
  ) where
  
import Prelude hiding (lookup)
import Data.HashMap
import qualified Assets.ResourcePack as Pack 
import Assets.Archive
import Assets.FileSystem
import Assets.Resource
import Control.Monad.Trans.Either

import qualified Data.Text as T 
import Data.Text (Text)
import Data.Monoid
import Control.DeepSeq

type ResourceManager = Map Text SomePack

data SomePack = forall a . (Archive a) => SomePack (Pack.ResourcePack a)

instance NFData SomePack where 
  rnf = (`seq` ())

emptyResourceManager :: ResourceManager
emptyResourceManager = empty

getResourcePack :: ResourceManager -> Text -> Maybe SomePack
getResourcePack mng name = name `lookup` mng

setResourcePack :: ResourceManager -> Text -> SomePack -> ResourceManager
setResourcePack mng name pack = insert name pack mng

addNewFileSystemPack :: ResourceManager -> Text -> FilePath -> IO ResourceManager
addNewFileSystemPack mng name path = do
  case getResourcePack mng name of
    Just (SomePack oldPack) -> Pack.finalizePack oldPack 
    Nothing -> return ()
  eitherT (\msg -> fail (T.unpack msg)) (\pack -> return $ insert name (SomePack pack) mng) $ Pack.newResourcePack name <$> newFileSystemArchive path
  
getResource :: (Resource a) => ResourceManager -> Text -> ResourceParams a -> EitherT Text IO (a, ResourceManager)
getResource mng fullName params = case getResourcePack mng packName of
  Nothing -> left $ "Cannot find resource pack with name \"" <> packName <> "\"!"
  Just (SomePack pack) -> do
    (res, newpack) <- Pack.getResource pack (T.unpack resName) params
    return (res, setResourcePack mng packName $ SomePack newpack)
  where
    (packName, T.drop 1 -> resName) = T.break (== ':') fullName