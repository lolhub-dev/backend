{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LolHub.Domain.Lobby where

import           LolHub.Connection.Graphql.Api

data Lobby = Lobby { _id :: ObjectId
                   , state :: LobbyState
                   , kind :: LobbyType
                   , creator :: User
                   , teams :: Teams
                   }
    deriving (Generic, Typeable, Show, Read, Eq, Ord)

$(deriveBson ''Lobby)
