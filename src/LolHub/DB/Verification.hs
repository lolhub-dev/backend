{-# LANGUAGE OverloadedStrings #-}

module LolHub.DB.Verification (insertVerification) where

import           Core.DB.MongoUtil (run, parseAction)
import           LolHub.Domain.VerifyToken
import           Database.MongoDB (Action, Pipe, Failure, Collection, Document
                                 , Value, access, close, connect, delete
                                 , exclude, find, findOne, host, insert
                                 , insertMany, master, project, rest, select
                                 , sort, hint, (=:))
import           Data.Bson.Mapping (toBson, fromBson)

col :: Collection
col = "verification"

insertVerificationToken :: VerificationE -> ActionE IO (Maybe Value)
insertVerificationToken verification = Just <$> verification
    where query = insert col (toBson query)


getTokenForName :: Text -> Action IO (Maybe Value)
