{-# LANGUAGE OverloadedStrings #-}

module LolHub.DB.User
    ( getUserByName
    , insertUser
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

col :: Collection
col = "user"

getUserByName :: String -> Action IO (Maybe UserE)
getUserByName username = parseAction query
  where
    query = findOne (select ["username" =: username] col)

insertUser :: UserE -> Action IO (Maybe Value)
insertUser user = Just <$> query
  where
    query = insert col (toBson user)