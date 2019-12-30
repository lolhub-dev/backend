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
import           Control.Lens

type Username = Text

type TeamE = [Username]

data TeamsE = TeamsE { _blueTeam :: TeamE, _redTeam :: TeamE }
  deriving (Generic, Typeable, Show, Read, Eq, Ord)

$(deriveBson ''TeamsE)

$(makeLenses ''TeamsE)

data LobbyStateE = WAITING
                 | FULL
                 | CLOSED
                 | OPEN
  deriving (Generic, Typeable, Show, Read, Eq, Ord)

$(deriveBson ''LobbyStateE)

$(makeLenses ''LobbyStateE)

data LobbyKindE = PRIVATE
                | PUBLIC
                | HIDDEN
  deriving (Generic, Typeable, Show, Read, Eq, Ord)

$(deriveBson ''LobbyKindE)

$(makeLenses ''LobbyKindE)

data LobbyE = LobbyE { _id :: ObjectId
                     , _state :: LobbyStateE
                     , _kind :: LobbyKindE
                     , _creator :: ObjectId
                     , _teams :: TeamsE
                     }
  deriving (Generic, Typeable, Show, Read, Eq, Ord)

$(deriveBson ''LobbyE)

$(makeLenses ''LobbyE)

createLobby :: LobbyKindE -> ObjectId -> User.UserE -> Maybe LobbyE
createLobby kind oid creator = do
  return
    LobbyE { _id = oid
           , _state = WAITING
           , _kind = kind
           , _creator = User._id creator
           , _teams = TeamsE { _blueTeam = [], _redTeam = [] }
           }

joinLobby :: LobbyE -> User.UserE -> LobbyE
joinLobby
  lobby
  user = lobby { _teams = newTeams } -- //TODO: refactor...lenses ?
  where
    newTeams = (_teams lobby) { _blueTeam = (User.username user):oldBlueTeam }

    oldBlueTeam = _blueTeam $ _teams $ lobby
