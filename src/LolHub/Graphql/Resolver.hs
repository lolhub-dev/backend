{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module LolHub.Graphql.Resolver where

import           LolHub.Graphql.Types
import qualified LolHub.Domain.Lobby as Lobby
import qualified LolHub.Domain.User as User
import           Data.Text
import           Data.Morpheus.Types (Event(..), IOMutRes, IORes, Resolver
                                    , WithOperation)

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
  :: User.UserE
  -> User (IOMutRes USEREVENT)-- //TODO: parse different user types: so far we always return UnverifiedUser!

resolveUser user = UserUnverifiedUser
  $ UnverifiedUser { unverifiedUserUsername = return $ User._username user
                   , unverifiedUserFirstname = return $ User._firstname user
                   , unverifiedUserLastname = return $ User._lastname user
                   , unverifiedUserEmail = return $ User._email user
                   , unverifiedUserToken = return $ User._token user
                   }

resolveTeam :: Lobby.TeamE -> Team (IOMutRes USEREVENT)
resolveTeam teamE = Team { teamMembers = return m }
  where
    m = fmap (pack . show) teamE :: [Text]

resolveTeams :: Lobby.TeamsE -> Teams (IOMutRes USEREVENT)
resolveTeams teamsE =
  Teams { teamsBlueTeam = return $ resolveTeam $ Lobby._blueTeam teamsE
        , teamsRedTeam = return $ resolveTeam $ Lobby._redTeam teamsE
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
    lid = return $ pack $ show $ Lobby._id $ lobbyE

    ls = return
      $ case Lobby._state lobbyE of
        Lobby.OPEN    -> OPEN
        Lobby.CLOSED  -> CLOSED
        Lobby.FULL    -> FULL
        Lobby.WAITING -> WAITING

    lc = return $ resolveUser $ userE

    lt = return $ resolveTeams $ Lobby._teams lobbyE

    lk = return $ fromLobbyKindE $ Lobby._kind lobbyE