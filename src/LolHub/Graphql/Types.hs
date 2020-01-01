{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module LolHub.Graphql.Types where

import qualified LolHub.Domain.User as User
import qualified LolHub.Domain.Lobby as Lobby
import           Data.Morpheus.Document (importGQLDocumentWithNamespace)
import           Data.Morpheus.Types (Event(..), IOMutRes, IORes, constRes
                                    , Resolver, WithOperation)
import           Data.Morpheus.Types.Internal.AST
import           Database.MongoDB (ObjectId)
import           Data.Text
import           Control.Concurrent.MonadIO

importGQLDocumentWithNamespace "src/LolHub/Graphql/Api.gql"

type Leaf (o :: OperationType) a = Resolver o () a

type Graphql (o :: OperationType) a = Resolver o () (a (Resolver o () IO))

data Channel = USER
  deriving (Show, Eq, Ord)

newtype Content = Content { contentID :: Int }

type USEREVENT = (Event Channel Content)

toLobbyKindE :: LobbyKind -> Lobby.LobbyKindE
toLobbyKindE lobbyKind = case lobbyKind of
  PUBLIC  -> Lobby.PUBLIC
  PRIVATE -> Lobby.PRIVATE
  HIDDEN  -> Lobby.HIDDEN

fromLobbyKindE :: Lobby.LobbyKindE -> LobbyKind
fromLobbyKindE lobbyKindE = case lobbyKindE of
  Lobby.PUBLIC  -> PUBLIC
  Lobby.PRIVATE -> PRIVATE
  Lobby.HIDDEN  -> HIDDEN

toTeamColorE :: TeamColor -> Lobby.TeamColorE
toTeamColorE tc = case tc of
  RED  -> Lobby.RED
  BLUE -> Lobby.BLUE

fromTeamColorE :: Lobby.TeamColorE -> TeamColor
fromTeamColorE tcE = case tcE of
  Lobby.RED  -> RED
  Lobby.BLUE -> BLUE

resolveUser
  :: (WithOperation o)
  => User.UserE
  -> Graphql o User -- //TODO: parse different user types: so far we always return UnverifiedUser!

resolveUser user = UserUnverifiedUser
  $ UnverifiedUser { unverifiedUserUsername = constRes $ User._username user
                   , unverifiedUserFirstname = constRes $ User._firstname user
                   , unverifiedUserLastname = constRes $ User._lastname user
                   , unverifiedUserEmail = constRes $ User._email user
                   , unverifiedUserToken = constRes $ User._token user
                   }

resolveTeam :: Lobby.TeamE -> Team (IOMutRes USEREVENT)
resolveTeam teamE = Team { teamMembers = constRes m }
  where
    m = fmap (pack . show) teamE :: [Text]

resolveTeams :: Lobby.TeamsE -> Teams (IOMutRes USEREVENT)
resolveTeams teamsE =
  Teams { teamsBlueTeam = constRes $ resolveTeam $ Lobby._blueTeam teamsE
        , teamsRedTeam = constRes $ resolveTeam $ Lobby._redTeam teamsE
        }

resolveLobby :: Lobby.LobbyE -> User.UserE -> Lobby (IOMutRes USEREVENT)
resolveLobby lobbyE userE =
  Lobby { lobby_id = lid
        , lobbyState = ls
        , lobbyCreator = lc
        , lobbyTeams = lt
        , lobbyKind = lk
        }
  where
    lid = constRes $ pack $ show $ Lobby._id $ lobbyE

    ls = constRes
      $ case Lobby._state lobbyE of
        Lobby.OPEN    -> OPEN
        Lobby.CLOSED  -> CLOSED
        Lobby.FULL    -> FULL
        Lobby.WAITING -> WAITING

    lc = constRes $ resolveUser $ userE

    lt = constRes $ resolveTeams $ Lobby._teams lobbyE

    lk = constRes $ fromLobbyKindE $ Lobby._kind lobbyE