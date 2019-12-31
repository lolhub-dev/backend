{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module LolHub.Graphql.Resolver.LobbyResolver (lobbyApi, USEREVENT) where

import           Control.Monad.Trans (lift)
import           Core.DB.MongoUtil (run, (<<-))
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
import           Text.Read hiding (lift)
import           Data.Bson.Mapping (toBson, fromBson)
import           Data.Morpheus.Types (Event(..), GQLRootResolver(..), IOMutRes
                                    , IOSubRes, IORes, ResolveM, ResolveQ
                                    , ResolveS, Undefined(..), Resolver(..)
                                    , constRes, liftEither)

----- API ------
lobbyApi :: Pipe -> Maybe User.SessionE -> ByteString -> IO ByteString
lobbyApi pipe session = interpreter $ lobbyGqlRoot pipe session

lobbyGqlRoot :: Pipe
             -> Maybe User.SessionE
             -> GQLRootResolver IO USEREVENT Query Mutation Subscription
lobbyGqlRoot pipe session =
  GQLRootResolver { queryResolver, mutationResolver, subscriptionResolver }
  where
    queryResolver = Query { helloWorld = resolveHelloWorld }

    -------------------------------------------------------------
    mutationResolver = Mutation { create = resolveCreateLobby session pipe
                                , join = resolveJoinLobby session pipe
                                }

    subscriptionResolver =
      Subscription { joined = resolveJoinedLobby session pipe }

----- QUERY RESOLVERS -----
resolveHelloWorld :: () -> IORes USEREVENT Text
resolveHelloWorld = constRes "helloWorld" -- //TODO: remove this, when there are other queries

----- MUTATION RESOLVERS -----
resolveCreateLobby :: Maybe User.SessionE
                   -> Pipe
                   -> CreateLobbyArgs
                   -> ResolveM USEREVENT IO Lobby
resolveCreateLobby session pipe args = liftEither
  (resolveCreateLobby' session pipe args)
  where
    resolveCreateLobby' session pipe args = do
      oid <- genObjectId
      uname <- return $ User._uname <$> session
      creator <- run (Actions.getUserByName <<- uname) pipe
      lobby <- return $ (Lobby.createLobby lobbyKind oid) =<< creator
      run (Actions.insertLobby <<- lobby) pipe
      return
        (maybeToEither "Invalid Session" $ resolveLobby <$> lobby <*> creator)

    lobbyKind = toLobbyKindE $ kind args :: Lobby.LobbyKindE

resolveJoinLobby :: Maybe User.SessionE
                 -> Pipe
                 -> JoinLobbyArgs
                 -> ResolveM USEREVENT IO Lobby
resolveJoinLobby session pipe JoinLobbyArgs { _id, team } = do
  value <- liftEither (resolveJoinLobby' session pipe _id team)
  MutResolver $ return ([Event [USER] (Content { contentID = 12 })], value)
  where
    resolveJoinLobby' :: Maybe User.SessionE
                      -> Pipe
                      -> Text
                      -> TeamColor
                      -> IO (Either String (Lobby (IOMutRes USEREVENT)))
    resolveJoinLobby' session pipe lobbyId tc = do
      uname <- return $ User._uname <$> session
      user <- run (Actions.getUserByName <<- uname) pipe
      lid <- return $ (readMaybe $ unpack lobbyId :: Maybe ObjectId)
      lobby <- run (Actions.findLobby <<- lid) pipe
      lobby' <- return
        $ Lobby.joinLobby <$> lobby <*> user <*> (return $ toTeamColorE tc)
      result <- run
        (Actions.updateLobby <<- lobby')
        pipe -- //TODO: magically worked, after a few commits, why...keep an eye on that !!!
      return
        (maybeToEither "Invalid Session" $ resolveLobby <$> lobby' <*> user)

resolveJoinedLobby :: Maybe User.SessionE
                   -> Pipe
                   -> JoinedLobbyArgs
                   -> ResolveS USEREVENT IO UserJoined
resolveJoinedLobby session pipe args =
  SubResolver { subChannels = [USER], subResolver = subResolver }
  where
    subResolver (Event _ content) = lift (resolveJoinedLobby' content)

    resolveJoinedLobby' :: Content -> IO (UserJoined (IORes USEREVENT))
    resolveJoinedLobby' content = return
      UserJoined { userJoinedUsername =
                     constRes $ pack $ show $ contentID content
                 }