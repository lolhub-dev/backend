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
import           LolHub.Graphql.Types
import           LolHub.Graphql.Query.UserQuery
import qualified LolHub.DB.User as Action
import           LolHub.Domain.User
import           Core.DB.MongoUtil (run)
import           Database.MongoDB (Pipe, Failure, genObjectId)
import           Data.Text
import           Data.Morpheus (interpreter)
import           Data.Morpheus.Types (Event(..), GQLRootResolver(..), IOMutRes
                                    , IORes, ResolveM, ResolveQ, ResolveS
                                    , Undefined(..), constRes, liftEither)
import           Data.Text (Text)
import           Control.Monad.IO.Class (liftIO)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.ByteString.Lazy (ByteString)

userApi :: Pipe -> ByteString -> IO ByteString
userApi pipe = interpreter $ userGqlRoot pipe

userGqlRoot :: Pipe -> GQLRootResolver IO USEREVENT Query Mutation Undefined
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
resolveHelloWorld = constRes "helloWorld" -- TODO: remove this, when there are other queries

----- MUTATION RESOLVERS -----
resolveLoginUser :: Pipe -> LoginArgs -> ResolveM USEREVENT IO User
resolveLoginUser pipe LoginArgs { username, password } = liftEither
  (resolveLoginUser' pipe username)
  where
    resolveLoginUser'
      :: Pipe -> Text -> IO (Either String (User (IOMutRes USEREVENT)))
    resolveLoginUser' pipe uname = do
      result <- run (Action.getUserByName $ unpack uname) pipe
      print result
      return
        $ case result of
          Nothing   -> Left "No such user found"
          Just user -> Right
            $ UserUnverifiedUser
            $ UnverifiedUser { unverifiedUserUsername =
                                 constRes $ Action.username user
                             , unverifiedUserFirstname =
                                 constRes $ Action.firstname user
                             , unverifiedUserLastname =
                                 constRes $ Action.lastname user
                             , unverifiedUserEmail =
                                 constRes $ Action.email user
                             , unverifiedUserToken =
                                 constRes $ Action.token user
                             }

resolveRegisterUser :: Pipe -> RegisterArgs -> ResolveM USEREVENT IO User
resolveRegisterUser pipe args = liftEither (setDBUser pipe args)

setDBUser
  :: Pipe -> RegisterArgs -> IO (Either String (User (IOMutRes USEREVENT)))
setDBUser
  pipe
  RegisterArgs { username, firstname, lastname, email, password } = do
  objectId <- genObjectId
  currTime <- getPOSIXTime
  token <- return
    $ encodeSession
    $ SessionE { uname = username
               , iat = currTime
               , exp = currTime + 1000 -- TODO: declare offset here" 
               }
  user <- return
    UserE { _id = objectId
          , username = username
          , email = email
          , firstname = firstname
          , lastname = lastname
          , password = password
          , token = token
          }
  eitherActionOrFailure <- Action.insertUser user
  case eitherActionOrFailure of
    Right action -> liftIO
      $ do
        run action pipe
        return
          $ Right
          $ UserUnverifiedUser
          $ UnverifiedUser { unverifiedUserUsername =
                               constRes $ Action.username user
                           , unverifiedUserFirstname =
                               constRes $ Action.firstname user
                           , unverifiedUserLastname =
                               constRes $ Action.lastname user
                           , unverifiedUserEmail = constRes $ Action.email user
                           , unverifiedUserToken = constRes token
                           }
    Left failure -> return $ Left $ show failure