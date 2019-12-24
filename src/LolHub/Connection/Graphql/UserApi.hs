{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module LolHub.Connection.Graphql.UserApi (userApi, USEREVENT) where

import           Control.Monad.Trans (lift)
import qualified Data.ByteString.Lazy.Char8 as B
import           Database.MongoDB (Pipe, Failure, genObjectId)
import           Data.Text
import           Data.Morpheus (interpreter)
import           Data.Morpheus.Document (importGQLDocumentWithNamespace)
import           Data.Morpheus.Types (Event(..), GQLRootResolver(..), IOMutRes
                                    , IORes, ResolveM, ResolveQ, ResolveS
                                    , Undefined(..), constRes, liftEither)
import           Data.Text (Text)
import qualified LolHub.Domain.User as UserE
import           Core.DB.MongoUtil (run)
import qualified LolHub.Connection.DB.User as User
import           Control.Monad.IO.Class (liftIO)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Web.JWT

importGQLDocumentWithNamespace "src/LolHub/Connection/Graphql/UserApi.gql"

data Channel = USER
  deriving (Show, Eq, Ord)

newtype Content = Content { contentID :: Int }

type USEREVENT = (Event Channel Content)

userApi :: Pipe -> B.ByteString -> IO B.ByteString
userApi pipe = interpreter $ userGqlRoot pipe

userGqlRoot :: Pipe -> GQLRootResolver IO USEREVENT Query Mutation Undefined
userGqlRoot pipe =
  GQLRootResolver { queryResolver, mutationResolver, subscriptionResolver }
  where
    queryResolver = Query { queryHelloWorld = resolveHelloWorld }

    -------------------------------------------------------------
    mutationResolver = Mutation { mutationLogin = loginUser pipe
                                , mutationRegister = registerUser pipe
                                }

    subscriptionResolver = Undefined

----- QUERY RESOLVERS -----
loginUser :: Pipe -> MutationLoginArgs -> ResolveM USEREVENT IO User
loginUser
  pipe
  MutationLoginArgs { mutationLoginArgsUsername, mutationLoginArgsPassword } =
  liftEither (getDBUser pipe mutationLoginArgsUsername)

resolveHelloWorld :: () -> IORes USEREVENT Text
resolveHelloWorld = constRes "helloWorld" -- TODO: remove this, when there are other queries

----- MUTATION RESOLVERS -----
registerUser :: Pipe -> MutationRegisterArgs -> ResolveM USEREVENT IO User
registerUser pipe args = liftEither (setDBUser pipe args)

----- STUB DB -----
getDBUser :: Pipe -> Text -> IO (Either String (User (IOMutRes USEREVENT)))
getDBUser pipe uname = do
  result <- run (User.getUserByName $ unpack uname) pipe
  print result
  return
    $ case result of
      Nothing   -> Left "No such user found"
      Just user -> Right
        $ UserUnverifiedUser
        $ UnverifiedUser { unverifiedUserUsername =
                             constRes $ pack $ User.username user
                         , unverifiedUserFirstname =
                             constRes $ pack $ User.firstname user
                         , unverifiedUserLastname =
                             constRes $ pack $ User.lastname user
                         , unverifiedUserEmail =
                             constRes $ pack $ User.email user
                         , unverifiedUserToken = constRes $ User.token user
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
    $ UserE.encodeSession
    $ UserE.Session { UserE.uname = mutationRegisterArgsUsername
                    , UserE.iat = currTime
                    , UserE.exp = currTime + 1000 -- TODO: declare offset here" 
                    }
  user <- return
    User.User { User._id = objectId
              , User.username = unpack mutationRegisterArgsUsername
              , User.email = unpack mutationRegisterArgsEmail
              , User.firstname = unpack mutationRegisterArgsFirstname
              , User.lastname = unpack mutationRegisterArgsLastname
              , User.password = unpack mutationRegisterArgsPassword
              , User.token = token
              }
  eitherActionOrFailure <- User.insertUser user
  case eitherActionOrFailure of
    Right action -> liftIO
      $ do
        run action pipe
        return
          $ Right
          $ UserUnverifiedUser
          $ UnverifiedUser { unverifiedUserUsername =
                               constRes $ pack $ User.username user
                           , unverifiedUserFirstname =
                               constRes $ pack $ User.firstname user
                           , unverifiedUserLastname =
                               constRes $ pack $ User.lastname user
                           , unverifiedUserEmail =
                               constRes $ pack $ User.email user
                           , unverifiedUserToken = constRes $ User.token user
                           }
    Left failure -> return $ Left $ show failure
