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

module LolHub.Graphql.Resolver.UserResolver (userApi, USEREVENT) where

import           Prelude hiding (exp)
import           LolHub.Graphql.Types
import           LolHub.Graphql.Query.UserQuery
import qualified LolHub.DB.User as DB
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
    queryResolver = Query { queryHelloWorld = resolveHelloWorld }

    -------------------------------------------------------------
    mutationResolver = Mutation { mutationRegister = registerUser pipe
                                , mutationLogin = loginUser pipe
                                }

    subscriptionResolver = Undefined

----- QUERY RESOLVERS -----
resolveHelloWorld :: () -> IORes USEREVENT Text
resolveHelloWorld = constRes "helloWorld" -- TODO: remove this, when there are other queries

----- MUTATION RESOLVERS -----
loginUser :: Pipe -> MutationLoginArgs -> ResolveM USEREVENT IO User
loginUser
  pipe
  MutationLoginArgs { mutationLoginArgsUsername, mutationLoginArgsPassword } =
  liftEither (getDBUser pipe mutationLoginArgsUsername)

registerUser :: Pipe -> MutationRegisterArgs -> ResolveM USEREVENT IO User
registerUser pipe args = liftEither (setDBUser pipe args)

----- STUB DB -----
getDBUser :: Pipe -> Text -> IO (Either String (User (IOMutRes USEREVENT)))
getDBUser pipe uname = do
  result <- run (DB.getUserByName $ unpack uname) pipe
  print result
  return
    $ case result of
      Nothing   -> Left "No such user found"
      Just user -> Right
        $ UserUnverifiedUser
        $ UnverifiedUser { unverifiedUserUsername = constRes $ DB.username user
                         , unverifiedUserFirstname =
                             constRes $ DB.firstname user
                         , unverifiedUserLastname = constRes $ DB.lastname user
                         , unverifiedUserEmail = constRes $ DB.email user
                         , unverifiedUserToken = constRes $ DB.token user
                         }

setDBUser :: Pipe
          -> MutationRegisterArgs
          -> IO (Either String (User (IOMutRes USEREVENT)))
setDBUser
  pipe
  MutationRegisterArgs { mutationRegisterArgsUsername
                       , mutationRegisterArgsFirstname
                       , mutationRegisterArgsLastname
                       , mutationRegisterArgsEmail
                       , mutationRegisterArgsPassword
                       } = do
  objectId <- genObjectId
  currTime <- getPOSIXTime
  token <- return
    $ encodeSession
    $ SessionE { uname = mutationRegisterArgsUsername
               , iat = currTime
               , exp = currTime + 1000 -- TODO: declare offset here" 
               }
  user <- return
    UserE { _id = objectId
          , username = mutationRegisterArgsUsername
          , email = mutationRegisterArgsEmail
          , firstname = mutationRegisterArgsFirstname
          , lastname = mutationRegisterArgsLastname
          , password = mutationRegisterArgsPassword
          , token = token
          }
  eitherActionOrFailure <- DB.insertUser user
  case eitherActionOrFailure of
    Right action -> liftIO
      $ do
        run action pipe
        return
          $ Right
          $ UserUnverifiedUser
          $ UnverifiedUser { unverifiedUserUsername = constRes $ username user
                           , unverifiedUserFirstname =
                               constRes $ firstname user
                           , unverifiedUserLastname = constRes $ lastname user
                           , unverifiedUserEmail = constRes $ email user
                           , unverifiedUserToken = constRes $ token
                           }
    Left failure -> return $ Left $ show failure