{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module LolHub.Graphql.Api.UserApi (userApi, USEREVENT) where

import           Core.Exception
import           Core.DB.MongoUtil (run)
import           LolHub.Graphql.Types
import           LolHub.Graphql.Resolver
import qualified LolHub.DB.User as Action
import qualified LolHub.Domain.User as User
import           Control.Exception (catch)
import           Data.Text (Text)
import           Data.Either.Utils
import           Data.ByteString.Lazy (ByteString)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Morpheus (interpreter)
import           Data.Morpheus.Document (importGQLDocument)
import           Data.Morpheus.Types (Event(..), GQLRootResolver(..), IOMutRes
                                    , IORes, ResolveM, ResolveQ, ResolveS
                                    , MUTATION, QUERY, SUBSCRIPTION
                                    , Resolver(..), Undefined(..), constRes
                                    , liftEither, lift)
import qualified Database.MongoDB as Mongo (Pipe, Value, Failure, Action
                                          , genObjectId)

importGQLDocument "src/LolHub/Graphql/Query/User.gql"

userApi :: Mongo.Pipe -> ByteString -> IO ByteString
userApi pipe = interpreter $ userGqlRoot pipe

userGqlRoot
  :: Mongo.Pipe -> GQLRootResolver IO USEREVENT Undefined Mutation Undefined
userGqlRoot pipe =
  GQLRootResolver { queryResolver, mutationResolver, subscriptionResolver }
  where
    queryResolver = Undefined

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
resolveLoginUser pipe LoginArgs { username, password } = liftEither $ resolveLoginUser' pipe username password
  where
    resolveLoginUser'
      :: Mongo.Pipe -> Text -> Text -> IO (EitherObject MUTATION USEREVENT String User)
    resolveLoginUser' pipe uname pword = do
      user <- run (Action.loginUser uname pword) pipe
      return $ maybeToEither "Wrong Credentials" $ resolveUser <$> user

resolveRegisterUser :: Mongo.Pipe -> RegisterArgs -> ResolveM USEREVENT IO User
resolveRegisterUser pipe args = lift (resolveRegisterUser' pipe args)
  where
    resolveRegisterUser' :: Mongo.Pipe
                         -> RegisterArgs
                         -> IO (Object MUTATION USEREVENT User)
    resolveRegisterUser'
      pipe
      RegisterArgs { username, firstname, lastname, email, password } = do
      oid <- Mongo.genObjectId
      currTime <- getPOSIXTime
      token
        <- return $ User.encodeSession $ User.createSession username currTime
      userE <- return
        User.UserE { _id = oid
                   , _username = username
                   , _email = email
                   , _firstname = firstname
                   , _lastname = lastname
                   , _password = password
                   , _token = token
                   }
      result <- run (Action.insertUser userE) pipe `catch` anyException
      user <- return $ resolveUser userE
      return user
      -- maybeToEither "Username already taken"
        -- $ result >> (Just user)