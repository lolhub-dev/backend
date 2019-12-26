{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module LolHub.Graphql.Resolver.LobbyResolver (lobbyApi, USEREVENT) where

import           Core.DB.MongoUtil (run)
import           LolHub.Graphql.Query.LobbyQuery
import qualified LolHub.Domain.Lobby as Lobby
import qualified LolHub.DB.Actions as DB
import           LolHub.Graphql.Types
import           Database.MongoDB (Pipe, Failure, genObjectId)
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

----- EVENTS -----
----- API ------
lobbyApi :: Pipe -> ByteString -> IO ByteString
lobbyApi pipe = interpreter $ lobbyGqlRoot pipe

lobbyGqlRoot
  :: Pipe -> GQLRootResolver IO USEREVENT LobbyQuery Undefined Undefined
lobbyGqlRoot pipe =
  GQLRootResolver { queryResolver, mutationResolver, subscriptionResolver }
  where
    queryResolver = LobbyQuery { queryHelloWorld = resolveHelloWorld }

    -------------------------------------------------------------
    mutationResolver = Undefined

    --Mutation { mutationCreateLobby = createLobby pipe }
    subscriptionResolver = Undefined

----- QUERY RESOLVERS -----
resolveHelloWorld :: () -> IORes USEREVENT Text
resolveHelloWorld = constRes "helloWorld" -- TODO: remove this, when there are other queries
{-| 
----- MUTATION RESOLVERS -----
createLobby :: Pipe -> MutationCreateLobbyArgs -> ResolveM USEREVENT IO Lobby
createLobby pipe args = MutResolver
  $ do
    value <- lift (createLobby' pipe args)
    pure ([addressUpdate], value)

createLobby'
  :: Pipe -> MutationCreateLobbyArgs -> IO (Lobby (IOMutRes USEREVENT))
createLobby' pipe args = do
  oid <- genObjectId
  lobby <- return
    $ Lobby.LobbyE { Lobby._id = oid
                   , Lobby.state = Lobby.OPEN
                   , Lobby.kind = Lobby.PUBLIC
                   , Lobby.creator = "userid"
                   , Lobby.teams = Lobby.TeamsE { Lobby.blueTeam = ["user1"]
                                                , Lobby.redTeam = ["user2"]
                                                }
                   }
  return (run (DB.insertLobby lobby) pipe)
  return
    Lobby { lobby_id = constRes $ pack $ show $ Lobby._id lobby
          , lobbyState = constRes WAITING
          , lobbyKind = constRes $ mutationCreateLobbyArgsKind args
          , lobbyCreator = constRes
              $ UserUnverifiedUser
              $ UnverifiedUser { unverifiedUserEmail = constRes "email"
                               , unverifiedUserFirstname = constRes "firstname"
                               , unverifiedUserLastname = constRes "lastname"
                               , unverifiedUserToken = constRes "fakeToken"
                               , unverifiedUserUsername = constRes "username"
                               }
          , lobbyTeams = constRes
              $ Teams { teamsBlueTeam =
                          constRes $ Team { teamMembers = constRes [] }
                      , teamsRedTeam =
                          constRes $ Team { teamMembers = constRes [] }
                      }
          }
|-}