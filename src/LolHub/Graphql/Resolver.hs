{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module LolHub.Graphql.Resolver where

import           Data.Morpheus.Types            ( Event(..)
                                                , IOMutRes
                                                , IORes
                                                , Resolver
                                                , WithOperation
                                                )
import           Data.Text
import qualified LolHub.Domain.Lobby           as Lobby
import qualified LolHub.Domain.User            as User
import           LolHub.Graphql.Types

toVerificationStatusE :: VerificationStatus -> User.VerificationStatusE
toVerificationStatusE UNVERIFIED        = User.UNVERIFIED
toVerificationStatusE VERIFIED          = User.VERIFIED
toVerificationStatusE SUMMONER_VERIFIED = User.SUMMONER_VERIFIED

toVerificationStatus :: User.VerificationStatusE -> VerificationStatus
toVerificationStatus User.UNVERIFIED        = UNVERIFIED
toVerificationStatus User.VERIFIED          = VERIFIED
toVerificationStatus User.SUMMONER_VERIFIED = SUMMONER_VERIFIED

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

resolveUser :: (WithOperation o) => User.UserE -> Object o USEREVENT User
resolveUser user = User
        { username  = pure $ User._username user
        , firstname = pure $ User._firstname user
        , lastname  = pure $ User._lastname user
        , email     = pure $ User._email user
        , token     = pure $ User._token user
        , verified  = pure $ toVerificationStatus $ User._verified user
        }

resolveUserInfo
        :: (WithOperation o) => User.UserE -> Object o USEREVENT UserInfo
resolveUserInfo user = UserInfo { username  = pure $ User._username user
                                , firstname = pure $ User._firstname user
                                , lastname  = pure $ User._lastname user
                                }

resolveTeam :: (WithOperation o) => Lobby.TeamE -> Object o USEREVENT Team
resolveTeam teamE = Team { members = pure m }
        where m = fmap (pack . show) teamE :: [Text]

resolveTeams :: (WithOperation o) => Lobby.TeamsE -> Object o USEREVENT Teams
resolveTeams teamsE = Teams
        { blueTeam = pure $ resolveTeam $ Lobby._blueTeam teamsE
        , redTeam  = pure $ resolveTeam $ Lobby._redTeam teamsE
        }

resolveLobby
        :: (WithOperation o)
        => Lobby.LobbyE
        -> User.UserE
        -> Object o USEREVENT Lobby
resolveLobby lobbyE userE = Lobby { _id     = lid
                                  , state   = ls
                                  , creator = lc
                                  , teams   = lt
                                  , kind    = lk
                                  }
    where
        lid = pure $ pack $ show $ Lobby._id lobbyE

        ls  = pure $ case Lobby._state lobbyE of
                Lobby.OPEN    -> OPEN
                Lobby.CLOSED  -> CLOSED
                Lobby.FULL    -> FULL
                Lobby.WAITING -> WAITING

        lc = pure $ resolveUserInfo userE

        lt = pure $ resolveTeams $ Lobby._teams lobbyE

        lk = pure $ fromLobbyKindE $ Lobby._kind lobbyE

