{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module LolHub.Graphql.Resolver.LobbyResolver (lobbyApi, USEREVENT) where

import           Core.DB.MongoUtil (run)
import           LolHub.Graphql.Query.LobbyQuery
import qualified LolHub.Domain.Lobby as Lobby
import qualified LolHub.Domain.User as User
import qualified LolHub.DB.Actions as DB
import           LolHub.Graphql.Types
import           Database.MongoDB (Pipe, Failure, genObjectId, ObjectId)
import           Data.Text
import           Data.ByteString.Lazy (ByteString)
import           Control.Monad.Trans.Class (lift)
import           Data.Morpheus (interpreter)
import           Data.Morpheus.Types (Event(..), GQLRootResolver(..), IOMutRes
                                    , IORes, ResolveM, ResolveQ, ResolveS
                                    , Undefined(..), Resolver(..), constRes
                                    , liftEither)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Web.JWT

----- API ------
lobbyApi :: User.SessionE -> Pipe -> ByteString -> IO ByteString
lobbyApi session pipe = interpreter $ lobbyGqlRoot session pipe

lobbyGqlRoot :: User.SessionE
             -> Pipe
             -> GQLRootResolver IO USEREVENT Query Mutation Undefined
lobbyGqlRoot session pipe =
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
resolveCreateLobby :: User.SessionE
                   -> Pipe
                   -> MutationCreateLobbyArgs
                   -> ResolveM USEREVENT IO Lobby
resolveCreateLobby session pipe args = liftEither
  (resolveCreateLobby' session pipe args)

resolveCreateLobby' :: User.SessionE
                    -> Pipe
                    -> MutationCreateLobbyArgs
                    -> IO (Either String (Lobby (IOMutRes USEREVENT)))
resolveCreateLobby' session pipe args = do
  oid <- genObjectId
  uname <- return $ unpack $ User.uname session
  maybeCreator <- run (DB.getUserByName uname) pipe
  maybeLobby <- return (Lobby.createLobby lobbyKind maybeCreator oid)
  case maybeLobby of
    Nothing -> return $ Left "invalid Lobby"
    Just lobby -> case maybeCreator of
      Nothing      -> return $ Left "invalid User"
      Just creator -> do
        res <- run (DB.insertLobby lobby) pipe
        print res
        return $ Right $ resolveLobby lobby creator
    -- parse lobby into gql type and return here
      where
        cid = Lobby.creator lobby :: ObjectId
  where
    lobbyKind = toLobbyKindE $ kind args :: Lobby.LobbyKindE