{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module LolHub.Graphql.Resolver.UserResolver (userApi, USEREVENT) where

import           Control.Exception (SomeException, catch)
import           Control.Monad
import           Control.Monad.IO.Class
import           Core.DB.MongoUtil (run)
import           Core.Exception
import           Data.ByteString.Lazy (ByteString)
import           Data.Either.Utils
import           Data.Morpheus (interpreter)
import           Data.Morpheus.Types (Event(..), GQLRootResolver(..), IOMutRes
                                    , IORes, ResolveM, ResolveQ, ResolveS
                                    , Undefined(..), constRes, liftEither, lift)
import           Data.Text (Text, pack, unpack)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.UUID (UUID)
import           Data.UUID.V4 as V4
import qualified Database.MongoDB as Mongo (Action, Failure, Pipe, Value
                                          , genObjectId)
import qualified LolHub.DB.User as UAction
import qualified LolHub.Domain.User as User
import qualified LolHub.Domain.SummonerToken as SummonerToken
import           LolHub.Graphql.Query.UserQuery
import           LolHub.Graphql.Types
import           Prelude hiding (exp)

userApi :: Mongo.Pipe -> ByteString -> IO ByteString
userApi pipe = interpreter $ userGqlRoot pipe

userGqlRoot
  :: Mongo.Pipe -> GQLRootResolver IO USEREVENT Query Mutation Undefined
userGqlRoot pipe =
  GQLRootResolver { queryResolver, mutationResolver, subscriptionResolver }
  where
    queryResolver = Query { helloWorld = resolveHelloWorld }

    -------------------------------------------------------------
    mutationResolver = Mutation { register = resolveRegisterUser pipe
                                , login = resolveLoginUser pipe
                                }

    subscriptionResolver = Undefined

----- QUERY RESOLVERS -----
resolveHelloWorld :: () -> IORes USEREVENT Text
resolveHelloWorld = constRes "helloWorld" -- //TODO: remove this, when there are other queries

----- MUTATION RESOLVERS -----
resolveLoginUser :: Mongo.Pipe -> LoginArgs -> ResolveM USEREVENT IO User
resolveLoginUser pipe LoginArgs { username, password } = liftEither
  (resolveLoginUser' pipe username password)
  where
    resolveLoginUser' -- //TODO: actually check password on login
      :: Mongo.Pipe
      -> Text
      -> Text
      -> IO (Either String (User (IOMutRes USEREVENT)))
    resolveLoginUser' pipe uname pword = do
      user <- run (UAction.loginUser uname pword) pipe
      return $ maybeToEither "Wrong Credentials" $ resolveUser <$> user

resolveRegisterUser :: Mongo.Pipe -> RegisterArgs -> ResolveM USEREVENT IO User
resolveRegisterUser pipe args = liftEither (resolveRegisterUser' pipe args)
  where
    resolveRegisterUser' :: Mongo.Pipe
                         -> RegisterArgs
                         -> IO (Either String (User (IOMutRes USEREVENT)))
    resolveRegisterUser'
      pipe
      RegisterArgs { username, firstname, lastname, email, password } = do
      oid <- Mongo.genObjectId
      currTime <- getPOSIXTime
      token
        <- return $ User.encodeSession $ User.createSession username currTime
      user <- return
        User.UserE { _id = oid
                   , username = username
                   , email = email
                   , firstname = firstname
                   , lastname = lastname
                   , password = password
                   , token = token
                   }
      result <- run (UAction.insertUser user) pipe `catch` anyException
      return
        $ maybeToEither "Username already taken"
        $ result >> (return $ resolveUser user)

resolveGetToken :: Text -> ResolveM USEREVENT IO SummonerToken
resolveGetToken username = lift $ resolveGetToken' username
  where
    resolveGetToken' :: Text -> IO (SummonerToken (IOMutRes USEREVENT))
    resolveGetToken' name = do
      uuid <- V4.nextRandom
      return
        $ resolveSummonerToken
        $ SummonerToken.SummonerTokenE { SummonerToken.name = name
                                       , SummonerToken.token = pack $ show uuid
                                       }

