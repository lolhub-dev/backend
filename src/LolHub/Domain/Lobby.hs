{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LolHub.Domain.Lobby where

import           LolHub.Graphql.Types
import           Data.Text
import           Database.MongoDB (ObjectId)
import           Data.Data (Typeable)
import           GHC.Generics
import           Data.Bson.Mapping

type UserId = String

type TeamE = [UserId]

data TeamsE = TeamsE { blueTeam :: TeamE, redTeam :: TeamE }
  deriving (Generic, Typeable, Show, Read, Eq, Ord)

$(deriveBson ''TeamsE)

data LobbyStateE = WAITING
                 | FULL
                 | CLOSED
                 | OPEN
  deriving (Generic, Typeable, Show, Read, Eq, Ord)

$(deriveBson ''LobbyStateE)

data LobbyKindE = PRIVATE
                | PUBLIC
                | HIDDEN
  deriving (Generic, Typeable, Show, Read, Eq, Ord)

$(deriveBson ''LobbyKindE)

data LobbyE = LobbyE { _id :: ObjectId
                     , state :: LobbyStateE
                     , kind :: LobbyKindE
                     , creator :: UserId
                     , teams :: TeamsE
                     }
  deriving (Generic, Typeable, Show, Read, Eq, Ord)

$(deriveBson ''LobbyE)
