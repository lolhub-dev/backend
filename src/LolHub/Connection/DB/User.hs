{-# LANGUAGE OverloadedStrings #-}

module LolHub.Connection.DB.User (getUserByName, insertUser, User(..)) where

import           Database.MongoDB (Action, Pipe, Failure, Collection, Document
                                 , Value, access, close, connect, delete
                                 , exclude, find, findOne, host, insert
                                 , insertMany, master, project, rest, select
                                 , sort, hint, (=:))
import           Control.Monad.Trans (liftIO)
import           Core.DB.MongoUtil (run, mapAction)
import           Control.Concurrent.MonadIO
import           GHC.Generics
import           LolHub.Domain.User
import           Data.Bson.Mapping
import           Control.Exception

col :: Collection
col = "user"

getUserByName :: String -> Action IO (Maybe User)
getUserByName username = mapAction query
  where
    query = findOne (select ["username" =: username] col)

insertUser :: User -> IO (Either Failure (Action IO Value))
insertUser user = try (return (insert col (toBson user)))