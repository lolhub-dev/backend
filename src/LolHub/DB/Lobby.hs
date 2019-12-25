{-# LANGUAGE OverloadedStrings #-}

module LolHub.DB.Lobby (insertLobby) where

import           Core.DB.MongoUtil (run, mapAction)
import           LolHub.Domain.Lobby
import           Database.MongoDB (Action, Pipe, Failure, Collection, Document
                                 , Value, access, close, connect, delete
                                 , exclude, find, findOne, host, insert
                                 , insertMany, master, project, rest, select
                                 , sort, hint, (=:))
import           Data.Bson.Mapping (toBson, fromBson)

col :: Collection
col = "lobby"

insertLobby :: LobbyE -> Action IO Value
insertLobby lobby = insert col (toBson lobby)