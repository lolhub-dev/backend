{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LolHub.Domain.Lobby where

import qualified LolHub.Domain.User            as User
import           Data.Text
import           Database.MongoDB               ( ObjectId )
import           Data.Data                      ( Typeable )
import           GHC.Generics
import           Data.Bson.Mapping
import           Control.Lens

type Username = Text

type TeamE = [Username]

data TeamColorE = BLUE
                | RED
  deriving (Generic, Typeable, Show, Read, Eq, Ord)

$(deriveBson ''TeamColorE)

$(makeLenses ''TeamColorE)

opponent RED  = BLUE
opponent BLUE = RED

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
                     , _creator :: Text
                     , _teams :: TeamsE
                     }
  deriving (Generic, Typeable, Show, Read, Eq, Ord)

$(deriveBson ''LobbyE)

$(makeLenses ''LobbyE)

createLobby :: LobbyKindE -> ObjectId -> Text -> Maybe LobbyE
createLobby kind oid creator = return LobbyE
        { _id      = oid
        , _state   = WAITING
        , _kind    = kind
        , _creator = creator
        , _teams   = TeamsE { _blueTeam = [], _redTeam = [] }
        }

joinLobby :: LobbyE -> User.UserE -> TeamColorE -> LobbyE
joinLobby lobby user dreamTeam
        | isInTeam dreamTeam = lobby
        | isInTeam $ opponent dreamTeam = ( join dreamTeam
                                          . leave (opponent dreamTeam)
                                          )
                lobby
        | otherwise = join dreamTeam lobby
    where
        color tc = if tc == BLUE then blueTeam else redTeam

        isInTeam :: TeamColorE -> Bool
        isInTeam tc =
                anyOf (teams . color tc) (elem $ User._username user) lobby

        join tc = over (teams . color tc) (User._username user :)

        leave tc = over
                (teams . color tc)
                (toListOf
                        (folded . ifiltered (\_ x -> x /= User._username user))
                )

