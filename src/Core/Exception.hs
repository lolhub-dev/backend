module Core.Exception where

import           Control.Exception (SomeException(..))
import           Control.Concurrent.MonadIO

anyException :: (MonadIO io) => SomeException -> io (Maybe m)
anyException = return $ return Nothing



