
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
        , gqlRoot
        , USEREVENT
        )
where

import           Control.Exception              ( catch )
import           Core.DB.MongoUtil              ( run
                                                , (<<-)
                                                )
import           Core.Exception
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Either.Utils
import           Data.Morpheus                  ( interpreter )
import           Data.Morpheus.Document         ( importGQLDocumentWithNamespace
                                                )
import           Data.Morpheus.Types            ( Event(..)
                                                , GQLRootResolver(..)
                                                , IOMutRes
                                                , IORes
                                                , MUTATION
                                                , QUERY
                                                , ResolveM
                                                , ResolveQ
                                                , ResolveS
                                                , Resolver(..)
                                                , SUBSCRIPTION
                                                , Undefined(..)
                                                , constRes
                                                , lift
                                                , liftEither
                                                , publish
                                                , subscribe
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

gqlRoot
        :: Mongo.Pipe
        -> Maybe User.SessionE
        -> GQLRootResolver IO USEREVENT Undefined Mutation Subscription
gqlRoot pipe session = GQLRootResolver { queryResolver
                                       , mutationResolver
                                       , subscriptionResolver
                                       }
    where
        queryResolver    = Undefined
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
resolveHelloWorld :: Value QUERY Text USEREVENT
resolveHelloWorld = pure "helloWorld" -- //TODO: remove this, when there are other queries

----- MUTATION RESOLVERS -----
resolveLoginUser
        :: Mongo.Pipe -> MutationLoginArgs -> ResolveM USEREVENT IO User
resolveLoginUser pipe MutationLoginArgs { mutationLoginArgsUsername, mutationLoginArgsPassword }
        = liftEither $ resolveLoginUser' pipe
                                         mutationLoginArgsUsername
                                         mutationLoginArgsPassword
    where
        resolveLoginUser'
                :: Mongo.Pipe
                -> Text
                -> Text
                -> IO (EitherObject MUTATION USEREVENT String User)
        resolveLoginUser' pipe uname pword = do
                user <- run (UserAction.loginUser uname pword) pipe
                return
                        $   maybeToEither "Wrong Credentials"
                        $   resolveUser
                        <$> user

resolveRegisterUser
        :: Mongo.Pipe -> MutationRegisterArgs -> ResolveM USEREVENT IO User
resolveRegisterUser pipe args = lift (resolveRegisterUser' pipe args)
    where
        resolveRegisterUser'
                :: Mongo.Pipe
                -> MutationRegisterArgs
                -> IO (Object MUTATION USEREVENT User)
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
        -> ResolveM USEREVENT IO Lobby
resolveCreateLobby session pipe args = liftEither
        (resolveCreateLobby' session pipe args)
    where
        resolveCreateLobby'
                :: Maybe User.SessionE
                -> Mongo.Pipe
                -> MutationCreateArgs
                -> IO (EitherObject MUTATION USEREVENT String Lobby)
        resolveCreateLobby' session pipe args = do
                oid <- Mongo.genObjectId
                let uname = User._uname <$> session
                creator <- run (UserAction.getUserByName <<- uname) pipe
                let lobby = Lobby.createLobby lobbyKind oid =<< creator
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
        -> ResolveM USEREVENT IO Lobby
resolveJoinLobby session pipe MutationJoinArgs { mutationJoinArgsLobby, mutationJoinArgsTeam }
        = do
                publish
                        [ Event { channels = [USER]
                                , content  = Content { contentID = 12 }
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
                -> IO (EitherObject MUTATION USEREVENT String Lobby)
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
                        <*> user
                        )

---- SUBSCRIPTIONS -----
--
resolveJoinedLobby
        :: Maybe User.SessionE
        -> Mongo.Pipe
        -> SubscriptionJoinedArgs
        -> ResolveS USEREVENT IO UserJoined
resolveJoinedLobby session pipe args = subscribe [USER] $ pure subResolver

    where
        subResolver (Event [USER] content) = lift (resolveJoinedLobby' content)
        resolveJoinedLobby' :: Content -> IO (Object QUERY USEREVENT UserJoined)
        resolveJoinedLobby' content = return UserJoined
                { username = return $ pack $ show $ contentID content
                }
