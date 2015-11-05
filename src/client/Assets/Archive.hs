module Assets.Archive(
    Archive(..)
  ) where
  
import Data.ByteString.Lazy (ByteString)
import Control.Monad.Trans.Either

import Data.Text (Text)

class Archive a where
  openArchive  :: FilePath -> EitherT Text IO a
  closeArchive :: a -> IO ()
  listArchive  :: a -> IO [FilePath]
  readArchiveFile  :: a -> FilePath -> EitherT Text IO ByteString
  writeArchiveFile :: a -> FilePath -> ByteString -> EitherT Text IO ()
  archivePath  :: a -> FilePath