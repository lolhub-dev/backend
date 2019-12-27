{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module LolHub.Graphql.Resolver.LobbyResolver (lobbyApi, USEREVENT) where

import           Core.DB.MongoUtil (run)
import           LolHub.Graphql.Query.LobbyQuery
import qualified LolHub.Domain.Lobby as Lobby
import qualified LolHub.Domain.User as User
import qualified LolHub.DB.Actions as Actions
import           LolHub.Graphql.Types
import           Database.MongoDB (Pipe, Failure, genObjectId, ObjectId)
import           Data.Text (pack, unpack, Text)
import           Data.ByteString.Lazy (ByteString)
import           Data.Morpheus (interpreter)
import           Data.Either.Utils
import           Data.Morpheus.Types (Event(..), GQLRootResolver(..), IOMutRes
                                    , IORes, ResolveM, ResolveQ, ResolveS
                                    , Undefined(..), Resolver(..), constRes
                                    , liftEither)

----- API ------
lobbyApi :: Pipe -> User.SessionE -> ByteString -> IO ByteString
lobbyApi pipe session = interpreter $ lobbyGqlRoot pipe session

lobbyGqlRoot :: Pipe
             -> User.SessionE
             -> GQLRootResolver IO USEREVENT Query Mutation Undefined
lobbyGqlRoot pipe session =
  GQLRootResolver { queryResolver, mutationResolver, subscriptionResolver }
  where
    queryResolver = Query { helloWorld = resolveHelloWorld }

    -------------------------------------------------------------
    mutationResolver =
      Mutation { createLobby = resolveCreateLobby session pipe }

    subscriptionResolver = Undefined

----- QUERY RESOLVERS -----
resolveHelloWorld :: () -> IORes USEREVENT Text
resolveHelloWorld = constRes "helloWorld" -- TODO: remove this, when there are other queries

----- MUTATION RESOLVERS -----
resolveCreateLobby
  :: User.SessionE -> Pipe -> CreateLobbyArgs -> ResolveM USEREVENT IO Lobby
resolveCreateLobby session pipe args = liftEither
  (resolveCreateLobby' session pipe args)
  where
    resolveCreateLobby' :: User.SessionE
                        -> Pipe
                        -> CreateLobbyArgs
                        -> IO (Either String (Lobby (IOMutRes USEREVENT)))
    resolveCreateLobby' session pipe args = do
      oid <- genObjectId
      uname <- return $ unpack $ User.uname session
      maybeCreator <- run (Actions.getUserByName uname) pipe
      maybeLobby <- return $ createMaybeLobby lobbyKind maybeCreator oid
      return (maybeToEither "" maybeLobby)

    lobbyKind = toLobbyKindE $ kind args :: Lobby.LobbyKindE

    createMaybeLobby :: Lobby.LobbyKindE
                     -> Maybe User.UserE
                     -> ObjectId
                     -> Maybe (Lobby (IOMutRes USEREVENT))
    createMaybeLobby kindE userE oid = do
      creator <- userE
      lobby <- Lobby.createLobby lobbyKind creator oid
      return $ run (Actions.insertLobby lobby) pipe
      return $ resolveLobby lobby creator