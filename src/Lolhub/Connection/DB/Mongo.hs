{-# LANGUAGE OverloadedStrings #-}

module Lolhub.Connection.DB.Mongo (run, mapAction) where

import           Database.MongoDB (access, master, Action, Pipe, Document)
import           Control.Concurrent.MonadIO
import           Database.MongoDB.Query (Database)
import           Data.Bson.Mapping
import           Data.Data (Typeable)
import           GHC.Generics
import           Control.Monad.Trans.Reader (mapReaderT)

-- | returns the DB name 
dbName :: Database
dbName = "lolhub"

run :: MonadIO m => Action m a -> Pipe -> m a
run action pipe = access pipe master dbName action

maybeFromBson :: (Bson a, Monad m, MonadIO io) => io (m Document) -> io (m a)
maybeFromBson a = ((\b -> return (fromBson =<< b)) =<< a)

mapAction
  :: (Bson a, Monad m, MonadIO io) => Action io (m Document) -> Action io (m a)
mapAction = mapReaderT maybeFromBson