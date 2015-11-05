module Authentication(
    AuthConnection(..)
  , isAuthed
  ) where 

import Network.Transport
import Data.Maybe (isJust)

import Data.Text (Text)

data AuthConnection = AuthConnection {
    authConn :: !Connection 
  , authName :: !(Maybe Text)
  } 

isAuthed :: AuthConnection -> Bool 
isAuthed = isJust . authName
