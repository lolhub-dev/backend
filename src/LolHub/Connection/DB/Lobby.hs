{-# LANGUAGE OverloadedStrings #-}

module LolHub.Connection.DB.Lobby (insertLobby) where

import           Database.MongoDB (Action, Pipe, Failure, Collection, Document
                                 , Value, access, close, connect, delete
                                 , exclude, find, findOne, host, insert
                                 , insertMany, master, project, rest, select
                                 , sort, hint, (=:))
import           Core.DB.MongoUtil (run, mapAction)
import           LolHub.Domain.Lobby
import           Data.Bson.Mapping

col :: Collection
col = "lobby"

insertLobby :: Lobby -> Action IO Value
insertUser user = return (insert col (toBson user))