{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module LolHub.Connection.Graphql.Resolver.Lobby (lobbyApi, USEREVENT, Lobby, LobbyState, LobbyType) where

import           Control.Monad.Trans (lift)
import qualified Data.ByteString.Lazy.Char8 as B
import           Database.MongoDB (Pipe, Failure, genObjectId)
import           Data.Text
import           Data.Morpheus (interpreter)
import           Data.Morpheus.Document (importGQLDocumentWithNamespace)
import           Data.Morpheus.Types (Event(..), GQLRootResolver(..), IOMutRes
                                    , IORes, ResolveM, ResolveQ, ResolveS
                                    , Undefined(..), constRes, liftEither)
import           Data.Text (Text)
import qualified LolHub.Domain.User as UserE
import           Core.DB.MongoUtil (run)
import qualified LolHub.Connection.DB.Lobby as Lobby
import           Control.Monad.IO.Class (liftIO)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Web.JWT

importGQLDocumentWithNamespace "src/LolHub/Connection/Graphql/Api.gql"

data Channel = USER
             | ADDRESS
  deriving (Show, Eq, Ord)

newtype Content = Content { contentID :: Int }

type USEREVENT = (Event Channel Content)

userApi :: Pipe -> B.ByteString -> IO B.ByteString
userApi pipe = interpreter $ userGqlRoot pipe

userGqlRoot :: Pipe -> GQLRootResolver IO USEREVENT Query Mutation Undefined
userGqlRoot pipe =
  GQLRootResolver { queryResolver, mutationResolver, subscriptionResolver }
  where
    queryResolver = Query { queryHelloWorld = resolveHelloWorld }

    -------------------------------------------------------------
    mutationResolver = Mutation { mutationCreateLobby = createLobby pipe
                                }

    subscriptionResolver = Undefined

----- QUERY RESOLVERS -----
resolveHelloWorld :: () -> IORes USEREVENT Text
resolveHelloWorld = constRes "helloWorld" -- TODO: remove this, when there are other queries

----- MUTATION RESOLVERS -----

createLobby:: Pipe -> MutationCreateLobbyArgs -> ResolveM USEREVENT IO Lobby
createLobby pipe args = liftIO (_createLobby pipe args)
  where _createLobby = run Lobby.insertLobby _lobby
        _lobby = Lobby{lobby_id = "alsjdfö,
        "}