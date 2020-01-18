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

module LolHub.Graphql.Api.LobbyApi
        ( lobbyApi
        , lobbyGqlRoot
        , USEREVENT
        )
where

import           Control.Monad.Trans     (lift)
import           Core.DB.MongoUtil       (run, (<<-))
import           Data.ByteString.Lazy    (ByteString)
import           Data.Either.Utils
import           Data.Morpheus           (interpreter)
import           Data.Morpheus.Document  (importGQLDocumentWithNamespace)
import           Data.Morpheus.Types     (Event (..), GQLRootResolver (..),
                                          IOMutRes, IORes, IOSubRes, MUTATION,
                                          QUERY, ResolveM, ResolveQ, ResolveS,
                                          Resolver (..), SUBSCRIPTION,
                                          Undefined (..), liftEither)
import           Data.Text               (Text, pack, unpack)
import           Database.MongoDB        (ObjectId, Pipe, genObjectId)
import qualified LolHub.DB.Actions       as Actions
import qualified LolHub.Domain.Lobby     as Lobby
import qualified LolHub.Domain.User      as User
import           LolHub.Graphql.Resolver
import           LolHub.Graphql.Types
import           Text.Read               (readMaybe)

importGQLDocumentWithNamespace "src/LolHub/Graphql/Query/Lobby.gql"

----- API ------
lobbyApi :: Pipe -> Maybe User.SessionE -> ByteString -> IO ByteString
lobbyApi pipe session = interpreter $ lobbyGqlRoot pipe session

lobbyGqlRoot
        :: Pipe
        -> Maybe User.SessionE
        -> GQLRootResolver IO USEREVENT Query Mutation Subscription
lobbyGqlRoot pipe session = GQLRootResolver { queryResolver
                                            , mutationResolver
                                            , subscriptionResolver
                                            }
    where
        queryResolver    = Query { queryHelloWorld = resolveHelloWorld }
        mutationResolver = Mutation
                { mutationCreate = resolveCreateLobby session pipe
                , mutationJoin   = resolveJoinLobby session pipe
                }
        subscriptionResolver = Subscription
                { subscriptionJoined = resolveJoinedLobby session pipe
                }

----- QUERY RESOLVERS -----
resolveHelloWorld :: Value QUERY Text USEREVENT
resolveHelloWorld = return $ pack "helloWorld" -- //TODO: remove this, when there are other queries

----- MUTATION RESOLVERS -----
resolveCreateLobby
        :: Maybe User.SessionE
        -> Pipe
        -> MutationCreateArgs
        -> ResolveM USEREVENT IO Lobby
resolveCreateLobby session pipe args = liftEither
        (resolveCreateLobby' session pipe args)
    where
        resolveCreateLobby'
                :: Maybe User.SessionE
                -> Pipe
                -> MutationCreateArgs
                -> IO (EitherObject MUTATION USEREVENT String Lobby)
        resolveCreateLobby' session pipe args = do
                oid <- genObjectId
                let uname = User._uname <$> session
                creator <- run (Actions.getUserByName <<- uname) pipe
                let lobby = Lobby.createLobby lobbyKind oid =<< creator
                run (Actions.insertLobby <<- lobby) pipe
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
        -> Pipe
        -> MutationJoinArgs
        -> ResolveM USEREVENT IO Lobby
resolveJoinLobby session pipe MutationJoinArgs { mutationJoinArgsLobby, mutationJoinArgsTeam }
        = do
                value <- liftEither
                        (resolveJoinLobby' session
                                           pipe
                                           mutationJoinArgsLobby
                                           mutationJoinArgsTeam
                        )
                MutResolver
                        $ return
                                  ( [Event [USER] (Content { contentID = 12 })]
                                  , value
                                  )
    where
        resolveJoinLobby'
                :: Maybe User.SessionE
                -> Pipe
                -> Text
                -> TeamColor
                -> IO (EitherObject MUTATION USEREVENT String Lobby)
        resolveJoinLobby' session pipe lobbyId tc = do
                let uname = User._uname <$> session
                user <- run (Actions.getUserByName <<- uname) pipe
                let lid = readMaybe $ unpack lobbyId :: Maybe ObjectId
                lobby <- run (Actions.findLobby <<- lid) pipe
                let
                        lobby' = Lobby.joinLobby <$> lobby <*> user <*> return
                                (toTeamColorE tc)
                result <- run (Actions.updateLobby <<- lobby') pipe -- //TODO: magically worked, after a few commits, why...keep an eye on that !!!
                return
                        (   maybeToEither "Invalid Session"
                        $   resolveLobby
                        <$> lobby'
                        <*> user
                        )

resolveJoinedLobby
        :: Maybe User.SessionE
        -> Pipe
        -> SubscriptionJoinedArgs
        -> ResolveS USEREVENT IO UserJoined
resolveJoinedLobby session pipe args = SubResolver { subChannels = [USER]
                                                   , subResolver = subResolver
                                                   }
    where
        subResolver (Event [USER] content) = lift (resolveJoinedLobby' content)
        resolveJoinedLobby' :: Content -> IO (Object QUERY USEREVENT UserJoined)
        resolveJoinedLobby' content = return UserJoined
                { username = return $ pack $ show $ contentID content
                }
