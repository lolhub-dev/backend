{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LolHub.Domain.Lobby where

import qualified LolHub.Domain.User as User
import           Data.Text
import           Database.MongoDB (ObjectId)
import           Data.Data (Typeable)
import           GHC.Generics
import           Data.Bson.Mapping

type TeamE = [ObjectId]

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
                     , creator :: ObjectId
                     , teams :: TeamsE
                     }
  deriving (Generic, Typeable, Show, Read, Eq, Ord)

createLobby :: LobbyKindE -> Maybe User.UserE -> ObjectId -> Maybe LobbyE
createLobby kind maybeCreator oid = case maybeCreator of
  Nothing      -> Nothing
  Just creator -> Just
    $ LobbyE { _id = oid
             , state = WAITING
             , kind = kind
             , creator = User._id creator
             , teams = TeamsE { blueTeam = [], redTeam = [] }
             }

$(deriveBson ''LobbyE)
