{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module LolHub.Graphql.Api
        ( api
        , subApi
        , gqlRoot
        , UserEvent
        )
where

import           Control.Exception              ( catch )
import           Core.DB.MongoUtil              ( run
                                                , (<<-)
                                                )
import           Core.Exception
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Either.Extra
import           Data.Morpheus                  ( interpreter )
import           Data.Morpheus.Document         ( importGQLDocumentWithNamespace
                                                )
import           Data.Morpheus.Types            ( Event(..)
                                                , Input
                                                , Stream
                                                , RootResolver(..)
                                                , MUTATION
                                                , QUERY
                                                , ResolverM
                                                , ResolverS
                                                , SUBSCRIPTION
                                                , constRes
                                                , lift
                                                , liftEither
                                                , publish
                                                , subscribe
                                                , SubscriptionField
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.Time.Clock.POSIX          ( getPOSIXTime )
import qualified Database.MongoDB              as Mongo
                                                ( Action
                                                , Failure
                                                , ObjectId
                                                , Pipe
                                                , Value
                                                , genObjectId
                                                )
import qualified LolHub.DB.Lobby               as LobbyAction
import qualified LolHub.DB.User                as UserAction
import qualified LolHub.Domain.Lobby           as Lobby
import qualified LolHub.Domain.User            as User
import           LolHub.Graphql.Resolver
import           LolHub.Graphql.Types
import           Text.Read                      ( readMaybe )

importGQLDocumentWithNamespace "src/LolHub/Graphql/Api.gql"

api :: Mongo.Pipe -> Maybe User.SessionE -> ByteString -> IO ByteString
api pipe session = interpreter $ gqlRoot pipe session

subApi
        :: Mongo.Pipe
        -> Maybe User.SessionE
        -> Input api
        -> Stream api UserEvent IO
subApi pipe session = interpreter $ gqlRoot pipe session

gqlRoot
        :: Mongo.Pipe
        -> Maybe User.SessionE
        -> RootResolver IO UserEvent Query Mutation Subscription
gqlRoot pipe session = RootResolver { queryResolver
                                    , mutationResolver
                                    , subscriptionResolver
                                    }
    where
        queryResolver    = Query { queryHelloWorld = resolveHelloWorld }
        mutationResolver = Mutation
                { mutationRegister = resolveRegisterUser pipe
                , mutationCreate   = resolveCreateLobby session pipe
                , mutationJoin     = resolveJoinLobby session pipe
                , mutationLogin    = resolveLoginUser pipe
                }
        subscriptionResolver = Subscription
                { subscriptionJoined = resolveJoinedLobby session pipe
                }
----- QUERY RESOLVERS -----
resolveHelloWorld :: Value QUERY Text UserEvent
resolveHelloWorld = pure "helloWorld" -- //TODO: remove this, when there are other queries

----- MUTATION RESOLVERS -----
resolveLoginUser
        :: Mongo.Pipe -> MutationLoginArgs -> ResolverM UserEvent IO User
resolveLoginUser pipe MutationLoginArgs { mutationLoginArgsUsername, mutationLoginArgsPassword }
        = liftEither $ resolveLoginUser' pipe
                                         mutationLoginArgsUsername
                                         mutationLoginArgsPassword
    where
        resolveLoginUser'
                :: Mongo.Pipe
                -> Text
                -> Text
                -> IO (EitherObject MUTATION UserEvent String User)
        resolveLoginUser' pipe uname pword = do
                user <- run (UserAction.loginUser uname pword) pipe
                return
                        $   maybeToEither "Wrong Credentials"
                        $   resolveUser
                        <$> user

resolveRegisterUser
        :: Mongo.Pipe -> MutationRegisterArgs -> ResolverM UserEvent IO User
resolveRegisterUser pipe args = lift (resolveRegisterUser' pipe args)
    where
        resolveRegisterUser'
                :: Mongo.Pipe
                -> MutationRegisterArgs
                -> IO (Object MUTATION UserEvent User)
        resolveRegisterUser' pipe MutationRegisterArgs { mutationRegisterArgsUsername, mutationRegisterArgsFirstname, mutationRegisterArgsLastname, mutationRegisterArgsEmail, mutationRegisterArgsPassword }
                = do
                        oid      <- Mongo.genObjectId
                        currTime <- getPOSIXTime
                        let token = User.encodeSession $ User.createSession
                                    mutationRegisterArgsUsername
                                    currTime
                        let
                                userE = User.UserE
                                        { _id        = oid
                                        , _username  =
                                                mutationRegisterArgsUsername
                                        , _email     = mutationRegisterArgsEmail
                                        , _firstname =
                                                mutationRegisterArgsFirstname
                                        , _lastname  =
                                                mutationRegisterArgsLastname
                                        , _password  =
                                                mutationRegisterArgsPassword
                                        , _token     = token
                                        , _verified  = User.UNVERIFIED
                                        }
                        result <-
                                run (UserAction.insertUser userE) pipe
                                        `catch` anyException
                        let user = resolveUser userE
                        return user
        -- maybeToEither "Username already taken"
        -- $ result >> (Just user)

resolveCreateLobby
        :: Maybe User.SessionE
        -> Mongo.Pipe
        -> MutationCreateArgs
        -> ResolverM UserEvent IO Lobby
resolveCreateLobby session pipe args = liftEither
        (resolveCreateLobby' session pipe args)
    where
        resolveCreateLobby'
                :: Maybe User.SessionE
                -> Mongo.Pipe
                -> MutationCreateArgs
                -> IO (EitherObject MUTATION UserEvent String Lobby)
        resolveCreateLobby' session pipe args = do
                oid <- Mongo.genObjectId
                let creator = User._uname <$> session
                let lobby   = Lobby.createLobby lobbyKind oid =<< creator
                run (LobbyAction.insertLobby <<- lobby) pipe
                return
                        (   maybeToEither "Invalid Session"
                        $   resolveLobby
                        <$> lobby
                        <*> creator
                        )
        lobbyKind =
                toLobbyKindE $ mutationCreateArgsKind args :: Lobby.LobbyKindE

resolveJoinLobby
        :: Maybe User.SessionE
        -> Mongo.Pipe
        -> MutationJoinArgs
        -> ResolverM UserEvent IO Lobby
resolveJoinLobby session pipe MutationJoinArgs { mutationJoinArgsLobby, mutationJoinArgsTeam }
        = do
                publish
                        [ Event
                                  { channels = [UserChannel]
                                  , content  = Content { lobbyId  = "test1234"
                                                       , username = "test"
                                                       }
                                  }
                        ]
                liftEither
                        (resolveJoinLobby' session
                                           pipe
                                           mutationJoinArgsLobby
                                           mutationJoinArgsTeam
                        )
    where
        resolveJoinLobby'
                :: Maybe User.SessionE
                -> Mongo.Pipe
                -> Text
                -> TeamColor
                -> IO (EitherObject MUTATION UserEvent String Lobby)
        resolveJoinLobby' session pipe lobbyId tc = do
                let uname = User._uname <$> session
                user <- run (UserAction.getUserByName <<- uname) pipe
                let lid = readMaybe $ unpack lobbyId :: Maybe Mongo.ObjectId
                lobby <- run (LobbyAction.findLobby <<- lid) pipe
                let
                        lobby' = Lobby.joinLobby <$> lobby <*> user <*> return
                                (toTeamColorE tc)
                result <- run (LobbyAction.updateLobby <<- lobby') pipe -- //TODO: magically worked, after a few commits, why...keep an eye on that !!!
                return
                        (   maybeToEither "Invalid Session"
                        $   resolveLobby
                        <$> lobby'
                        <*> (User._username <$> user)
                        )

---- SUBSCRIPTIONS -----
--
resolveJoinedLobby
        :: Maybe User.SessionE
        -> Mongo.Pipe
        -> SubscriptionJoinedArgs
        -> SubscriptionField (ResolverS UserEvent IO UserJoined)
resolveJoinedLobby session pipe args = subscribe UserChannel
        $ pure resolveJoinedLobby'
    where
        resolveJoinedLobby'
                :: Event Channel Content -> ResolverS UserEvent IO UserJoined
        resolveJoinedLobby' _ = pure UserJoined { username = return "test" }
