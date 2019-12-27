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

module LolHub.Graphql.Types where

import qualified LolHub.Domain.User as User
import qualified LolHub.Domain.Lobby as Lobby
import           Data.Morpheus.Document (importGQLDocumentWithNamespace)
import           Data.Morpheus.Types (Event(..), IOMutRes, IORes, constRes)
import           Database.MongoDB (ObjectId)
import           Data.Text
import           Control.Concurrent.MonadIO

importGQLDocumentWithNamespace "src/LolHub/Graphql/Api.gql"

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
    Lobby.HIDDEN -> HIDDEN

resolveUser :: User.UserE -> User (IOMutRes USEREVENT)
resolveUser user = UserUnverifiedUser
  $ UnverifiedUser { unverifiedUserUsername = constRes $ User.username user
                   , unverifiedUserFirstname = constRes $ User.firstname user
                   , unverifiedUserLastname = constRes $ User.lastname user
                   , unverifiedUserEmail = constRes $ User.email user
                   , unverifiedUserToken = constRes $ User.token user
                   }

resolveTeam :: Lobby.TeamE -> Team (IOMutRes USEREVENT)
resolveTeam teamE = Team { teamMembers = constRes m }
  where
    m = fmap (pack . show) teamE :: [Text]

resolveTeams :: Lobby.TeamsE -> Teams (IOMutRes USEREVENT)
resolveTeams teamsE =
  Teams { teamsBlueTeam = constRes $ resolveTeam $ Lobby.blueTeam teamsE
        , teamsRedTeam = constRes $ resolveTeam $ Lobby.redTeam teamsE
        }

resolveLobby :: Lobby.LobbyE -> User.UserE -> Lobby (IOMutRes USEREVENT)
resolveLobby lobbyE userE =
  Lobby { lobby_id = lid, lobbyState = ls, lobbyCreator = lc, lobbyTeams = lt, lobbyKind = lk }
  where
    lid = constRes $ pack $ show $ Lobby._id $ lobbyE

    ls = constRes
      $ case Lobby.state lobbyE of
        Lobby.OPEN   -> OPEN
        Lobby.CLOSED -> CLOSED
        Lobby.FULL   -> FULL
        Lobby.WAITING -> WAITING

    lc = constRes $ resolveUser $ userE

    lt = constRes $ resolveTeams $ Lobby.teams lobbyE
    lk = constRes $ fromLobbyKindE $ Lobby.kind lobbyE