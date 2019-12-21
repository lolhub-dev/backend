{-# LANGUAGE OverloadedStrings #-}

module Lolhub.Connection.DB.Mongo (run) where

import           Database.MongoDB (access, master, Action, Pipe)
import           Control.Concurrent.MonadIO
import           Database.MongoDB.Query (Database)

-- | returns the DB name 
dbName :: Database
dbName = "lolhub"

run :: MonadIO m => Action m a -> Pipe -> m a
run action pipe = access pipe master dbName action