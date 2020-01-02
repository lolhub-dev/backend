{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module LolHub.Graphql.Resolver.UserResolver (userApi, USEREVENT) where

import           Prelude hiding (exp)
import           Core.Exception
import           LolHub.Graphql.Types
import           LolHub.Graphql.Query.UserQuery
import qualified LolHub.DB.User as Action
import qualified LolHub.Domain.User as User
import           Core.DB.MongoUtil (run)
import qualified Database.MongoDB as Mongo (Pipe, Value, Failure, Action
                                          , genObjectId)
import           Data.Text
import           Data.Morpheus (interpreter)
import           Data.Either.Utils
import           Data.Morpheus.Types (Event(..), GQLRootResolver(..), IOMutRes
                                    , IORes, ResolveM, ResolveQ, ResolveS
                                    , MUTATION, QUERY, SUBSCRIPTION
                                    , Resolver(..), Undefined(..), constRes
                                    , liftEither)
import           Data.Text (Text)
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString.Lazy (ByteString)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Control.Exception (catch, SomeException)

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
    resolveLoginUser' :: Mongo.Pipe
                      -> Text
                      -> Text
                      -> IO (Either String (User (IOMutRes USEREVENT)))
    resolveLoginUser' pipe uname pword = do
      user <- run (Action.loginUser uname pword) pipe
      pure $ maybeToEither "Wrong Credentials" $ resolveUser <$> user

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
                   , _username = username
                   , _email = email
                   , _firstname = firstname
                   , _lastname = lastname
                   , _password = password
                   , _token = token
                   }
      result <- run (Action.insertUser user) pipe `catch` anyException
      return
        $ maybeToEither "Username already taken"
        $ result >> (return $ resolveUser user)