{-# LANGUAGE DoAndIfThenElse, DeriveDataTypeable #-}
module Assets.FileSystem(
    FileSystemArchive
  , newFileSystemArchive
  ) where

import Prelude hiding (readFile, writeFile)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Either
import Data.Typeable
import Data.ByteString.Lazy (readFile, writeFile)
import System.Directory
import System.FilePath
import Assets.Archive
import Util.Monad (liftExceptions)

import Data.Text (Text)

newtype FileSystemArchive = FileSystemArchive FilePath
  deriving (Typeable)
  
instance Archive FileSystemArchive where
  openArchive path = do 
    ex <- liftIO $ doesDirectoryExist path 
    if ex then do
      p <- liftIO $ getPermissions path
      if readable p && writable p
      then right $ FileSystemArchive path
      else left "Doesn't have permissions"
    else left "Directory doesn't exist" 
    
  closeArchive _ = return ()
  
  listArchive (FileSystemArchive path) = getDirectoryContents path
  readArchiveFile (FileSystemArchive path) file = liftExceptions $ liftIO $ readFile $ path </> file
  writeArchiveFile (FileSystemArchive path) file ds = liftExceptions $ liftIO $ writeFile (path </> file) ds
  archivePath (FileSystemArchive path) = path
  
newFileSystemArchive :: FilePath -> EitherT Text IO FileSystemArchive
newFileSystemArchive path = do
  liftExceptions $ liftIO $ createDirectoryIfMissing True path
  openArchive path