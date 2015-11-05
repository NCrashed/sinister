{-# LANGUAGE ScopedTypeVariables #-}
module Util.Monad(
    liftExceptions
  , whenM 
  , unlessM
  , whenMaybe
  , whenMaybeM
  ) where
  
import Control.Exception
import Control.Monad.Trans (liftIO)  
import Control.Monad.Trans.Either
import Control.Monad 

import Data.Text (Text)
import TextShow

-- | Catches all synchronous exceptions all transforms them into EitherT Text
-- | monad transformer. 
liftExceptions :: forall a . IO a -> EitherT Text IO a
liftExceptions action = do
  res <- liftIO (try action :: IO (Either SomeException a))
  eitherT (left.showt) right $ hoistEither res

-- | Monadic condition version of @when@
whenM :: Monad m => m Bool -> m () -> m ()
whenM cond f = do 
  v <- cond 
  when v f

-- | Monadic condition version of @unless@
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM cond f = do 
  v <- cond 
  unless v f

-- | If given value is just then pass it to handler
whenMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenMaybe Nothing _ = return ()
whenMaybe (Just a) f = f a 

whenMaybeM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenMaybeM cond f = do 
  v <- cond 
  whenMaybe v f