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

module LolHub.Graphql.Api.UserApi
  ( userApi
  , USEREVENT
  ) where

import           Control.Exception       (catch)
import           Core.DB.MongoUtil       (run)
import           Core.Exception
import           Data.ByteString.Lazy    (ByteString)
import           Data.Either.Utils
import           Data.Morpheus           (interpreter)
import           Data.Morpheus.Document  (importGQLDocument)
import           Data.Morpheus.Types     (Event (..), GQLRootResolver (..),
                                          IOMutRes, IORes, MUTATION, QUERY,
                                          ResolveM, ResolveQ, ResolveS,
                                          Resolver (..), SUBSCRIPTION,
                                          Undefined (..), constRes, lift,
                                          liftEither)
import           Data.Text               (Text, pack, unpack)
import           Data.Time.Clock.POSIX   (getPOSIXTime)
import           Data.UUID.V4            as V4
import qualified Database.MongoDB        as Mongo (Action, Failure, Pipe, Value,
                                                   genObjectId)
import qualified LolHub.DB.User          as Action
import qualified LolHub.Domain.User      as User
import           LolHub.Graphql.Resolver
import           LolHub.Graphql.Types

importGQLDocument "src/LolHub/Graphql/Query/User.gql"

userApi :: Mongo.Pipe -> ByteString -> IO ByteString
userApi pipe = interpreter $ userGqlRoot pipe

userGqlRoot ::
     Mongo.Pipe -> GQLRootResolver IO USEREVENT Undefined Mutation Undefined
userGqlRoot pipe =
  GQLRootResolver {queryResolver, mutationResolver, subscriptionResolver}
  where
    queryResolver = Undefined
    -------------------------------------------------------------
    mutationResolver =
      Mutation
        {register = resolveRegisterUser pipe, login = resolveLoginUser pipe}
    subscriptionResolver = Undefined

----- QUERY RESOLVERS -----
resolveHelloWorld :: () -> IORes USEREVENT Text
resolveHelloWorld = constRes "helloWorld" -- //TODO: remove this, when there are other queries

----- MUTATION RESOLVERS -----
resolveLoginUser :: Mongo.Pipe -> LoginArgs -> ResolveM USEREVENT IO User
resolveLoginUser pipe LoginArgs {username, password} =
  liftEither $ resolveLoginUser' pipe username password
  where
    resolveLoginUser' ::
         Mongo.Pipe
      -> Text
      -> Text
      -> IO (EitherObject MUTATION USEREVENT String User)
    resolveLoginUser' pipe uname pword = do
      user <- run (Action.loginUser uname pword) pipe
      return $ maybeToEither "Wrong Credentials" $ resolveUser <$> user

resolveRegisterUser :: Mongo.Pipe -> RegisterArgs -> ResolveM USEREVENT IO User
resolveRegisterUser pipe args = lift (resolveRegisterUser' pipe args)
  where
    resolveRegisterUser' ::
         Mongo.Pipe -> RegisterArgs -> IO (Object MUTATION USEREVENT User)
    resolveRegisterUser' pipe RegisterArgs { username
                                           , firstname
                                           , lastname
                                           , email
                                           , password
                                           } = do
      oid <- Mongo.genObjectId
      currTime <- getPOSIXTime
      token <-
        return $ User.encodeSession $ User.createSession username currTime
      userE <-
        return
          User.UserE
            { _id = oid
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

resolveGetToken :: Text -> ResolveM USEREVENT IO SummonerToken
resolveGetToken username = lift $ resolveGetToken' username
  where
    resolveGetToken' :: Text -> IO (SummonerToken (IOMutRes USEREVENT))
    resolveGetToken' name = do
      uuid <- V4.nextRandom
      return $
        resolveSummonerToken $
        User.SummonerTokenE {User._sname = name, User._vtoken = pack $ show uuid}
