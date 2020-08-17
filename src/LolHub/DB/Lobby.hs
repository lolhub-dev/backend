{-# LANGUAGE OverloadedStrings #-}

module LolHub.DB.Lobby (insertLobby, findLobby, updateLobby) where

import           Core.DB.MongoUtil (run, parseAction)
import           LolHub.Domain.Lobby
import           Database.MongoDB (Action, ObjectId, Pipe, Failure, Collection
                                 , Document, Value, access, close, connect
                                 , delete, exclude, find, findOne, host, insert
                                 , upsert, insertMany, master, project, rest
                                 , save, replace, select, sort, hint, (=:))
import           Data.Bson.Mapping (toBson, fromBson)
import           Data.Text

col :: Collection
col = "lobby"

updateLobby :: LobbyE -> Action IO (Maybe ())
updateLobby lobby = Just <$> query
  where
    query = save col $ toBson lobby

insertLobby :: LobbyE -> Action IO (Maybe Value)
insertLobby lobby = Just <$> query
  where
    query = insert col (toBson lobby)

findLobby :: ObjectId -> Action IO (Maybe LobbyE)
findLobby id = parseAction query
  where
    query = findOne (select ["_id" =: id] col)