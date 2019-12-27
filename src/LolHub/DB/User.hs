{-# LANGUAGE OverloadedStrings #-}

module LolHub.DB.User
    ( getUserByName
    , insertUser
    , loginUser
    , UserE(..)
    , SessionE(..)
    , Action) where

import           Database.MongoDB (Action, Pipe, Failure, Collection, Document
                                 , Value, access, close, connect, delete
                                 , exclude, find, findOne, host, insert
                                 , insertMany, master, project, rest, select
                                 , sort, hint, (=:))
import           Control.Monad.Trans (liftIO)
import           Core.DB.MongoUtil (run, encodeAction, parseAction)
import           Control.Concurrent.MonadIO
import           GHC.Generics
import           LolHub.Domain.User
import           Data.Bson.Mapping
import Data.Text

col :: Collection
col = "user"

getUserByName :: Text -> Action IO (Maybe UserE)
getUserByName username = parseAction query
  where
    query = findOne (select ["username" =: username] col)

loginUser :: Text -> Text -> Action IO (Maybe UserE)
loginUser username password = parseAction query
  where
    query = findOne
      (select ["username" =: username, "password" =: password] col)

insertUser :: UserE -> Action IO (Maybe Value)
insertUser user = Just <$> query
  where
    query = insert col (toBson user)