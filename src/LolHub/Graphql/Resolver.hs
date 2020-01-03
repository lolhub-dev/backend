{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module LolHub.Graphql.Resolver where

import           Data.Morpheus.Types  (Event (..), IOMutRes, IORes, Resolver,
                                       WithOperation)
import           Data.Text
import qualified LolHub.Domain.Lobby  as Lobby
import qualified LolHub.Domain.User   as User
import           LolHub.Graphql.Types

toLobbyKindE :: LobbyKind -> Lobby.LobbyKindE
toLobbyKindE lobbyKind =
  case lobbyKind of
    PUBLIC  -> Lobby.PUBLIC
    PRIVATE -> Lobby.PRIVATE
    HIDDEN  -> Lobby.HIDDEN

fromLobbyKindE :: Lobby.LobbyKindE -> LobbyKind
fromLobbyKindE lobbyKindE =
  case lobbyKindE of
    Lobby.PUBLIC  -> PUBLIC
    Lobby.PRIVATE -> PRIVATE
    Lobby.HIDDEN  -> HIDDEN

toTeamColorE :: TeamColor -> Lobby.TeamColorE
toTeamColorE tc =
  case tc of
    RED  -> Lobby.RED
    BLUE -> Lobby.BLUE

fromTeamColorE :: Lobby.TeamColorE -> TeamColor
fromTeamColorE tcE =
  case tcE of
    Lobby.RED  -> RED
    Lobby.BLUE -> BLUE

resolveUser ::
     (WithOperation o)
  => User.UserE
  -> Object o USEREVENT User -- //TODO: parse different user types: so far we always return UnverifiedUser!
resolveUser user =
  UserUnverifiedUser $
  UnverifiedUser
    { unverifiedUserUsername = pure $ User._username user
    , unverifiedUserFirstname = pure $ User._firstname user
    , unverifiedUserLastname = pure $ User._lastname user
    , unverifiedUserEmail = pure $ User._email user
    , unverifiedUserToken = pure $ User._token user
    }

resolveTeam :: (WithOperation o) => Lobby.TeamE -> Object o USEREVENT Team
resolveTeam teamE = Team {teamMembers = pure m}
  where
    m = fmap (pack . show) teamE :: [Text]

resolveTeams :: (WithOperation o) => Lobby.TeamsE -> Object o USEREVENT Teams
resolveTeams teamsE =
  Teams
    { teamsBlueTeam = pure $ resolveTeam $ Lobby._blueTeam teamsE
    , teamsRedTeam = pure $ resolveTeam $ Lobby._redTeam teamsE
    }

resolveLobby ::
     (WithOperation o) => Lobby.LobbyE -> User.UserE -> Object o USEREVENT Lobby
resolveLobby lobbyE userE =
  Lobby
    { lobby_id = lid
    , lobbyState = ls
    , lobbyCreator = lc
    , lobbyTeams = lt
    , lobbyKind = lk
    }
  where
    lid = pure $ pack $ show $ Lobby._id $ lobbyE
    ls =
      pure $
      case Lobby._state lobbyE of
        Lobby.OPEN    -> OPEN
        Lobby.CLOSED  -> CLOSED
        Lobby.FULL    -> FULL
        Lobby.WAITING -> WAITING
    lc = pure $ resolveUser $ userE
    lt = pure $ resolveTeams $ Lobby._teams lobbyE
    lk = pure $ fromLobbyKindE $ Lobby._kind lobbyE

resolveSummonerToken ::
     (WithOperation o)
  => User.SummonerTokenE
  -> Object o USEREVENT SummonerToken
resolveSummonerToken summtok =
  SummonerToken
    { summonerTokenToken = pure $ User._vtoken summtok
    , summonerTokenName = pure $ User._sname summtok
    }
