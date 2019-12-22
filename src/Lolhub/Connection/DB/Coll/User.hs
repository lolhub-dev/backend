{-# LANGUAGE OverloadedStrings #-}

module Lolhub.Connection.DB.Coll.User (getUser, insertUser, User(..)) where

import           Database.MongoDB (Action, Pipe, Collection, Document, Value
                                 , access, close, connect, delete, exclude, find
                                 , findOne, host, insert, insertMany, master
                                 , project, rest, select, sort, (=:))
import           Lolhub.Connection.DB.Mongo (run, mapAction)
import           Lolhub.Domain.User
import           Data.Bson.Mapping

collection :: Collection
collection = "user"

getUser :: String -> Action IO (Maybe User)
getUser username = mapAction query
  where
    query = findOne (select ["username" =: username] collection)
      :: Action IO (Maybe Document)

insertUser :: User -> Action IO Value
insertUser user = insert collection (toBson user)